#' Feature Network UI Module
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
mod_feature_network_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    bslib::page_fluid(
      class = "p-0",
      bslib::layout_sidebar(
        fillable = FALSE,
        padding = 0,

        sidebar = bslib::sidebar(
          title = "Feature Network",
          width = 350,
          bg = "#f8f9fa",

          tags$h6(class = "fw-bold text-primary", "1. Relationship Detection"),
          shinyWidgets::prettyCheckboxGroup(
            inputId = ns("detect_types"),
            label = "Relationship Types:",
            choices = c(
              "Natural Isotopes" = "isotope",
              "Adducts" = "adduct",
              "In-source Fragments" = "isf"
            ),
            selected = c("isotope", "adduct", "isf"),
            icon = icon("check"),
            status = "primary"
          ),
          selectInput(
            ns("ion_mode"),
            "Ion Mode:",
            choices = c("Auto" = "auto", "Positive" = "positive", "Negative" = "negative"),
            selected = "auto"
          ),
          numericInput(ns("ppm"), "Mass Tolerance (ppm)", value = 10, min = 1, step = 1),
          numericInput(ns("rt_tolerance"), "RT Window (sec)", value = 1, min = 0.1, step = 0.1),
          sliderInput(ns("cor_cutoff"), "Abundance Correlation", min = 0, max = 1, value = 0.7, step = 0.05),
          numericInput(ns("max_charge"), "Max Isotope Charge", value = 2, min = 1, max = 4, step = 1),
          numericInput(ns("max_nl_charge"), "Max Neutral Loss Charge", value = 1, min = 1, max = 3, step = 1),

          tags$hr(),
          tags$h6(class = "fw-bold text-success", "2. Visualization"),
          selectInput(ns("view_mode"), "Mode:", choices = c("Positive" = "positive", "Negative" = "negative"), selected = "positive"),
          selectInput(ns("sub_network"), "Sub-network:", choices = c("All networks" = "all")),
          checkboxInput(ns("interactive_plot"), "Interactive network", value = TRUE),
          sliderInput(ns("min_confidence"), "Min Edge Confidence", min = 0, max = 1, value = 0, step = 0.05),
          numericInput(ns("max_render_edges"), "Max Rendered Edges", value = 1500, min = 50, step = 50),
          numericInput(ns("max_subnetworks"), "Max Sub-network Choices", value = 50, min = 5, step = 5),

          tags$hr(),
          actionButton(ns("run_network"), "Build Feature Network", icon = icon("project-diagram"), class = "btn-teal w-100 fw-bold shadow-sm")
        ),

        div(
          class = "p-3",

          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("info-circle"), " Method Summary"),
          div(
            class = "card bg-light border-start border-primary border-3 mb-4",
            div(class = "card-body py-2", verbatimTextOutput(ns("method_summary"), placeholder = TRUE))
          ),

          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("activity"), " Network Status"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header("Positive Mode Status", class = "bg-info-subtle text-info-emphasis"),
              verbatimTextOutput(ns("status_pos"), placeholder = TRUE),
              style = "min-height: 170px;"
            ),
            bslib::card(
              bslib::card_header("Negative Mode Status", class = "bg-warning-subtle text-warning-emphasis"),
              verbatimTextOutput(ns("status_neg"), placeholder = TRUE),
              style = "min-height: 170px;"
            )
          ),

          br(),

          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("diagram-3"), " Feature Relationship Network"),
          bslib::card(
            height = "720px",
            full_screen = TRUE,
            bslib::card_header(textOutput(ns("network_title"), inline = TRUE), class = "bg-light"),
            uiOutput(ns("network_ui"))
          ),

          br(),

          tags$h5(class = "text-primary fw-bold mb-3", bsicons::bs_icon("table"), " Empirical Compound Quantification"),
          bslib::card(
            bslib::card_header("Network Tables", class = "bg-light"),
            bslib::navset_card_tab(
              bslib::nav_panel("Edges", DT::dataTableOutput(ns("tbl_edges"))),
              bslib::nav_panel("Pseudo Area", DT::dataTableOutput(ns("tbl_pseudo_area"))),
              bslib::nav_panel("Compound Info", DT::dataTableOutput(ns("tbl_compound_info"))),
              bslib::nav_panel("Feature Mapping", DT::dataTableOutput(ns("tbl_feature_mapping")))
            )
          )
        )
      )
    )
  )
}

#' Feature Network Server Module
#'
#' @param id Module id.
#' @param global_data ReactiveValues. Expects `object_pos_norm` and
#'   `object_neg_norm`.
#' @param prj_init Project init reactive object.
#' @noRd
mod_feature_network_server <- function(id, global_data, prj_init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_progress_modal <- function(title = "Processing...", message = "Please wait...", value = 0) {
      showModal(modalDialog(
        title = div(tags$span(class = "spinner-border spinner-border-sm text-primary", role = "status"), " ", title),
        div(class = "modal-progress-text", id = ns("progress_message"), message),
        div(
          class = "progress",
          style = "height: 20px;",
          div(
            id = ns("progress_bar"),
            class = "progress-bar progress-bar-striped progress-bar-animated bg-success",
            role = "progressbar",
            style = paste0("width: ", value, "%;"),
            `aria-valuenow` = value,
            `aria-valuemin` = "0",
            `aria-valuemax` = "100",
            paste0(value, "%")
          )
        ),
        footer = NULL,
        easyClose = FALSE,
        size = "m"
      ))
    }

    update_progress_modal <- function(value, message = NULL) {
      shinyjs::runjs(sprintf("$('#%s').css('width', '%s%%').text('%s%%');", ns("progress_bar"), value, value))
      if (!is.null(message)) {
        shinyjs::runjs(sprintf("$('#%s').text('%s');", ns("progress_message"), message))
      }
    }

    close_progress_modal <- function() {
      removeModal()
    }

    get_input_obj <- function(mode) {
      if (identical(mode, "positive")) {
        if (!is.null(global_data$object_pos_network)) return(global_data$object_pos_network)
        if (!is.null(global_data$object_pos_norm)) return(global_data$object_pos_norm)
        if (!is.null(prj_init$object_positive.init)) return(prj_init$object_positive.init)
        return(NULL)
      }

      if (!is.null(global_data$object_neg_network)) return(global_data$object_neg_network)
      if (!is.null(global_data$object_neg_norm)) return(global_data$object_neg_norm)
      if (!is.null(prj_init$object_negative.init)) return(prj_init$object_negative.init)
      NULL
    }

    get_pseudo <- function(mode) {
      if (identical(mode, "positive")) {
        return(global_data$pseudo_area_pos)
      }
      global_data$pseudo_area_neg
    }

    selected_object <- reactive({
      get_input_obj(input$view_mode)
    })

    selected_network <- reactive({
      obj <- selected_object()
      if (is.null(obj)) return(empty_feature_network())
      net <- extract_feature_network(obj)
      net[net$confidence >= input$min_confidence, , drop = FALSE]
    })

    output$method_summary <- renderText({
      paste0(
        "Detect: ", paste(input$detect_types, collapse = ", "),
        "\nIon mode: ", input$ion_mode,
        "\nMass tolerance: ", input$ppm, " ppm",
        "\nRT window: ", input$rt_tolerance, " sec",
        "\nCorrelation cutoff: ", input$cor_cutoff,
        "\nNeutral loss charge: z <= ", input$max_nl_charge
      )
    })

    observe({
      choices <- c()
      if (!is.null(global_data$object_pos_norm) || !is.null(global_data$object_pos_network)) {
        choices <- c(choices, "Positive" = "positive")
      }
      if (!is.null(global_data$object_neg_norm) || !is.null(global_data$object_neg_network)) {
        choices <- c(choices, "Negative" = "negative")
      }
      if (length(choices) > 0) {
        updateSelectInput(session, "view_mode", choices = choices, selected = choices[[1]])
      }
    })

    observe({
      obj <- selected_object()
      net <- selected_network()
      choices <- c("All networks" = "all")
      if (!is.null(obj) && nrow(net) > 0 && requireNamespace("igraph", quietly = TRUE)) {
        comp <- get_network_component_membership(net)
        involved_ids <- unique(c(net$from, net$to))
        comp_sizes <- sort(table(comp[involved_ids]), decreasing = TRUE)
        comp_sizes <- utils::head(comp_sizes, input$max_subnetworks)
        comp_choices <- stats::setNames(
          names(comp_sizes),
          paste0("Sub-network ", names(comp_sizes), " (", as.integer(comp_sizes), " features)")
        )
        choices <- c(choices, comp_choices)
      }
      updateSelectInput(session, "sub_network", choices = choices, selected = "all")
    })

    build_one_mode <- function(object, mode, progress_value, progress_label) {
      if (is.null(object)) {
        return(NULL)
      }

      update_progress_modal(progress_value, paste("Detecting relationships:", progress_label))
      ion_mode <- input$ion_mode
      if (identical(ion_mode, "auto")) {
        ion_mode <- mode
      }

      obj_net <- detect_feature_relationships(
        object = object,
        mode = ion_mode,
        detect = input$detect_types,
        ppm = input$ppm,
        rt_tolerance = input$rt_tolerance,
        cor_cutoff = input$cor_cutoff,
        max_charge = input$max_charge,
        max_neutral_loss_charge = input$max_nl_charge,
        store = TRUE
      )

      update_progress_modal(progress_value + 12, paste("Collapsing subnetworks:", progress_label))
      pseudo <- collapse_to_pseudo_area(obj_net)

      list(object = obj_net, pseudo = pseudo)
    }

    observeEvent(input$run_network, {
      pos_in <- global_data$object_pos_norm
      neg_in <- global_data$object_neg_norm

      if (is.null(pos_in) && is.null(neg_in)) {
        shinyalert::shinyalert(
          "Data Missing",
          "No normalized mass_dataset objects found. Please run Normalization first.",
          type = "error"
        )
        return()
      }

      shinyjs::disable("run_network")
      show_progress_modal("Building Feature Network", "Preparing feature data...", 0)

      tryCatch({
        if (!is.null(pos_in)) {
          res_pos <- build_one_mode(pos_in, "positive", 20, "Positive Mode")
          global_data$object_pos_network <- res_pos$object
          global_data$pseudo_area_pos <- res_pos$pseudo
          if (!is.null(prj_init$mass_dataset_dir)) {
            object_pos_network <- res_pos$object
            pseudo_area_pos <- res_pos$pseudo
            save(object_pos_network, file = file.path(prj_init$mass_dataset_dir, "06.object_pos_feature_network.rda"))
            save(pseudo_area_pos, file = file.path(prj_init$mass_dataset_dir, "06.pseudo_area_pos.rda"))
          }
        }

        if (!is.null(neg_in)) {
          res_neg <- build_one_mode(neg_in, "negative", 55, "Negative Mode")
          global_data$object_neg_network <- res_neg$object
          global_data$pseudo_area_neg <- res_neg$pseudo
          if (!is.null(prj_init$mass_dataset_dir)) {
            object_neg_network <- res_neg$object
            pseudo_area_neg <- res_neg$pseudo
            save(object_neg_network, file = file.path(prj_init$mass_dataset_dir, "06.object_neg_feature_network.rda"))
            save(pseudo_area_neg, file = file.path(prj_init$mass_dataset_dir, "06.pseudo_area_neg.rda"))
          }
        }

        update_progress_modal(100, "Done!")
        Sys.sleep(0.5)
        close_progress_modal()
        shinyjs::enable("run_network")
        shinyalert::shinyalert("Feature Network Completed", "Feature relationships and empirical compounds are ready.", type = "success")
      }, error = function(e) {
        close_progress_modal()
        shinyjs::enable("run_network")
        shinyalert::shinyalert("Feature Network Failed", paste("Error:", e$message), type = "error")
      })
    })

    summarize_network <- function(mode) {
      obj <- get_input_obj(mode)
      pseudo <- get_pseudo(mode)
      if (is.null(obj)) {
        return("No normalized object available.")
      }

      net <- extract_feature_network(obj)
      type_counts <- if (nrow(net) > 0) {
        paste(names(table(net$type)), as.integer(table(net$type)), sep = "=", collapse = ", ")
      } else {
        "none"
      }

      compound_count <- if (!is.null(pseudo)) nrow(pseudo$expression_data) else 0
      paste0(
        "Features: ", nrow(massdataset::extract_variable_info(obj)),
        "\nEdges: ", nrow(net),
        "\nEdge types: ", type_counts,
        "\nEmpirical compounds: ", compound_count
      )
    }

    output$status_pos <- renderText(summarize_network("positive"))
    output$status_neg <- renderText(summarize_network("negative"))

    network_data <- reactive({
      obj <- selected_object()
      net <- selected_network()
      if (is.null(obj) || nrow(net) == 0) {
        return(NULL)
      }
      make_network_display_data(
        object = obj,
        network = net,
        sub_network = input$sub_network,
        max_edges = input$max_render_edges
      )
    })

    output$network_title <- renderText({
      mode_label <- if (identical(input$view_mode, "positive")) "Positive Mode" else "Negative Mode"
      sub_label <- if (identical(input$sub_network, "all")) "All networks" else paste("Sub-network", input$sub_network)
      display <- network_data()
      render_label <- if (!is.null(display) && isTRUE(display$truncated)) {
        paste0(" - showing top ", nrow(display$edges), " edges by confidence")
      } else {
        ""
      }
      paste0(mode_label, " - ", sub_label, render_label)
    })

    output$network_ui <- renderUI({
      if (isTRUE(input$interactive_plot)) {
        visNetwork::visNetworkOutput(ns("network_vis"), height = "650px")
      } else {
        plotOutput(ns("network_static"), height = "650px")
      }
    })

    output$network_vis <- visNetwork::renderVisNetwork({
      display <- network_data()
      validate(need(!is.null(display), "No network edges available. Run detection first or lower the confidence cutoff."))
      visNetwork::visNetwork(display$nodes, display$edges, height = "650px") |>
        visNetwork::visNodes(shape = "dot", size = 18, font = list(size = 18)) |>
        visNetwork::visGroups(groupname = "Parent ion", color = "#008080") |>
        visNetwork::visGroups(groupname = "ISF", color = "#d95f02") |>
        visNetwork::visGroups(groupname = "Natural isotope", color = "#1b9e77") |>
        visNetwork::visGroups(groupname = "Adduct", color = "#7570b3") |>
        visNetwork::visGroups(groupname = "Feature", color = "#7f8c8d") |>
        visNetwork::visEdges(arrows = "to", smooth = FALSE, font = list(align = "middle", size = 12)) |>
        visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
        visNetwork::visInteraction(dragNodes = TRUE, dragView = TRUE, hover = TRUE, tooltipDelay = 80) |>
        visNetwork::visPhysics(enabled = TRUE, stabilization = TRUE) |>
        visNetwork::visLegend(useGroups = TRUE, position = "right")
    })

    output$network_static <- renderPlot({
      display <- network_data()
      validate(need(!is.null(display), "No network edges available. Run detection first or lower the confidence cutoff."))
      g <- igraph::graph_from_data_frame(display$edges[, c("from", "to")], directed = TRUE, vertices = display$nodes)
      node_colors <- c(
        "Parent ion" = "#008080",
        "ISF" = "#d95f02",
        "Natural isotope" = "#1b9e77",
        "Adduct" = "#7570b3",
        "Feature" = "#7f8c8d"
      )
      plot(
        g,
        vertex.color = node_colors[display$nodes$group],
        vertex.label = display$nodes$id,
        vertex.label.cex = 0.8,
        vertex.size = 18,
        edge.arrow.size = 0.4,
        edge.label = display$edges$label,
        edge.label.cex = 0.7,
        layout = igraph::layout_with_fr(g)
      )
    })

    output$tbl_edges <- DT::renderDataTable({
      net <- selected_network()
      DT::datatable(net, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_pseudo_area <- DT::renderDataTable({
      pseudo <- get_pseudo(input$view_mode)
      req(pseudo)
      tbl <- pseudo$expression_data
      tbl <- cbind(compound_id = rownames(tbl), tbl)
      DT::datatable(tbl, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_compound_info <- DT::renderDataTable({
      pseudo <- get_pseudo(input$view_mode)
      req(pseudo)
      DT::datatable(pseudo$compound_info, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$tbl_feature_mapping <- DT::renderDataTable({
      pseudo <- get_pseudo(input$view_mode)
      req(pseudo)
      DT::datatable(pseudo$feature_mapping, options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}

make_network_display_data <- function(object, network, sub_network = "all", max_edges = Inf) {
  if (nrow(network) == 0) {
    return(NULL)
  }

  membership <- get_network_component_membership(network)

  if (!identical(sub_network, "all")) {
    selected <- names(membership)[membership == as.integer(sub_network)]
    network <- network[network$from %in% selected & network$to %in% selected, , drop = FALSE]
  }

  if (nrow(network) == 0) {
    return(NULL)
  }

  truncated <- FALSE
  if (identical(sub_network, "all") && is.finite(max_edges) && nrow(network) > max_edges) {
    network <- network[order(network$confidence, decreasing = TRUE), , drop = FALSE]
    network <- utils::head(network, max_edges)
    truncated <- TRUE
  }

  variable_info <- massdataset::extract_variable_info(object)
  feature_ids <- unique(c(network$from, network$to))
  variable_info <- variable_info[match(feature_ids, variable_info$variable_id), , drop = FALSE]

  node_type <- rep("Feature", length(feature_ids))
  names(node_type) <- feature_ids
  node_type[unique(network$to[network$type == "Adduct"])] <- "Adduct"
  node_type[unique(network$to[network$type == "Isotope"])] <- "Natural isotope"
  node_type[unique(network$to[network$type == "ISF"])] <- "ISF"
  node_type[unique(network$from[network$type == "ISF"])] <- "Parent ion"

  nodes <- data.frame(
    id = feature_ids,
    label = feature_ids,
    group = unname(node_type[feature_ids]),
    title = sprintf(
      "ID: %s<br>Type: %s<br>m/z: %.5f<br>RT: %.2f",
      feature_ids,
      unname(node_type[feature_ids]),
      variable_info$mz,
      variable_info$rt
    ),
    stringsAsFactors = FALSE
  )

  edges <- data.frame(
    from = network$from,
    to = network$to,
    label = network$annotation,
    title = sprintf(
      "Type: %s<br>Annotation: %s<br>Confidence: %.3f<br>Correlation: %.3f<br>m/z error: %.3f ppm<br>RT diff: %.2f sec",
      network$type,
      network$annotation,
      network$confidence,
      network$abundance_cor,
      network$mz_error_ppm,
      network$rt_diff
    ),
    arrows = "to",
    stringsAsFactors = FALSE
  )

  list(nodes = nodes, edges = edges, truncated = truncated)
}

get_network_component_membership <- function(network) {
  feature_ids <- unique(c(network$from, network$to))
  graph <- igraph::graph_from_data_frame(network[, c("from", "to"), drop = FALSE],
                                         directed = FALSE,
                                         vertices = data.frame(name = feature_ids))
  igraph::components(graph)$membership
}
