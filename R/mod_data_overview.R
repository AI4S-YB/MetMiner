#' Data Overview & QC Module
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs toggle hidden
#' @importFrom shinyWidgets switchInput pickerInput
#' @importFrom ggplot2 ggtitle
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom massdataset extract_sample_info show_mz_rt_plot show_sample_missing_values
#' @importFrom massqc massqc_pca massqc_cumulative_rsd_plot massqc_sample_boxplot massqc_sample_correlation
#' @importFrom rmarkdown render
mod_data_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    # --- Custom CSS ---
    tags$style(HTML("
      .switch-no-margin .form-group { margin-bottom: 0 !important; }
      .control-bar-item { display: flex; align-items: center; }
    ")),

    bslib::page_fluid(
      class = "p-3",

      # --- Top Control Bar ---
      div(class = "d-flex justify-content-between align-items-center mb-4 p-3 bg-white rounded shadow-sm border",
          div(class = "d-flex align-items-center gap-3",
              actionButton(ns("start_qc"), "Start Analysis", icon = icon("play"), class = "btn-teal fw-bold"),
              downloadButton(ns("export_report"), "Generate Report", class = "btn-outline-secondary", icon = icon("file-pdf")),
              div(style = "border-left: 1px solid #dee2e6; height: 24px; margin: 0 5px;"),
              # Restore Interactive Switch
              div(class = "switch-no-margin control-bar-item",
                  shinyWidgets::switchInput(inputId = ns("interactive_mode"), label = "Interactive", labelWidth = "80px", onStatus = "success", offStatus = "secondary", size = "small", value = FALSE, inline = TRUE))
          ),
          div(class = "d-flex align-items-center switch-no-margin",
              span("Polarity:", class = "me-2 fw-bold text-secondary", style = "font-size: 0.9rem;"),
              shinyWidgets::switchInput(inputId = ns("polarity_mode"), onLabel = "POS", offLabel = "NEG", onStatus = "danger", offStatus = "primary", value = TRUE, size = "normal", inline = TRUE)
          )
      ),

      # --- Dashboard Grid ---
      bslib::layout_column_wrap(
        width = 1/2, heights_equal = "row",

        # Row 1
        bslib::card(
          height = "500px", bslib::card_header(bsicons::bs_icon("graph-up"), " m/z vs RT"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(open = FALSE, bg = "#f8f9fa", checkboxInput(ns("mzrt_hex"), "Use Hexbin", value = TRUE)),
            uiOutput(ns("ui_plot_mzrt"))
          )
        ),
        bslib::card(
          height = "500px", bslib::card_header(bsicons::bs_icon("bar-chart"), " Sample Missing Values"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              open = FALSE, bg = "#f8f9fa",
              selectInput(ns("mv_color"), "Color By", choices = NULL),
              selectInput(ns("mv_order"), "Order By", choices = NULL),
              checkboxInput(ns("mv_pct"), "Show Percentage", value = TRUE),
              checkboxInput(ns("mv_text"), "Show X Text", value = TRUE),
              checkboxInput(ns("mv_desc"), "Descending Order", value = FALSE)
            ),
            uiOutput(ns("ui_plot_mv"))
          )
        ),

        # Row 2
        bslib::card(
          height = "500px", bslib::card_header(bsicons::bs_icon("compass"), " PCA"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              open = FALSE, bg = "#f8f9fa",
              checkboxInput(ns("pca_scale"), "Log2 + Scale", value = TRUE),
              selectInput(ns("pca_color"), "Color By", choices = NULL),
              sliderInput(ns("pca_alpha"), "Point Alpha", 0.1, 1, 0.8),
              sliderInput(ns("pca_size"), "Point Size", 1, 10, 3)
            ),
            uiOutput(ns("ui_plot_pca"))
          )
        ),
        bslib::card(
          height = "500px", bslib::card_header(bsicons::bs_icon("graph-up"), " Cumulative RSD"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              open = FALSE, bg = "#f8f9fa",
              selectInput(ns("rsd_color"), "Color By", choices = NULL),
              numericInput(ns("rsd_cutoff"), "RSD Cutoff (%)", value = 30),
              checkboxInput(ns("rsd_desc"), "Descending", value = FALSE)
            ),
            uiOutput(ns("ui_plot_rsd"))
          )
        ),

        # Row 3
        bslib::card(
          height = "500px", bslib::card_header(bsicons::bs_icon("box"), " Sample Boxplot"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              open = FALSE, bg = "#f8f9fa",
              selectInput(ns("box_color"), "Color By", choices = NULL),
              selectInput(ns("box_fill"), "Fill By", choices = NULL),
              checkboxInput(ns("box_point"), "Show Points", value = FALSE)
            ),
            uiOutput(ns("ui_plot_boxplot"))
          )
        ),
        bslib::card(
          height = "500px", bslib::card_header(bsicons::bs_icon("grid-3x3"), " Sample Correlation"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              open = FALSE, bg = "#f8f9fa",
              selectInput(ns("cor_method"), "Method", choices = c("spearman", "pearson", "kendall")),
              selectInput(ns("cor_order"), "Order By", choices = NULL)
            ),
            uiOutput(ns("ui_plot_cor"))
          )
        )
      ),

      if(exists("metminer_footer")) metminer_footer() else tags$div()
    )
  )
}

#' Data Overview Server Module
#'
#' @param id Module id.
#' @param global_data ReactiveValues containing imported mass_dataset objects.
#' @param downloads ReactiveValues used for downloadable report state.
#' @noRd
mod_data_overview_server <- function(id, global_data, downloads) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Data Selection (Always Raw)
    current_obj <- reactive({
      req(input$start_qc) # Trigger

      is_pos <- input$polarity_mode

      # Always use raw data for initial QC
      obj <- if(is_pos) global_data$object_pos_raw else global_data$object_neg_raw

      if(is.null(obj)) {
        shinyalert::shinyalert("No Data", "Please import data first.", type = "warning")
        return(NULL)
      }
      return(obj)
    })

    # 2. Update Inputs (Color By, etc.)
    observeEvent(current_obj(), {
      obj <- current_obj()
      req(obj)
      sample_cols <- colnames(massdataset::extract_sample_info(obj))
      valid_cols <- sample_cols

      get_def <- function(pref, opts, fallback=NULL) {
        if(pref %in% opts) pref else if(!is.null(fallback) && fallback %in% opts) fallback else opts[1]
      }

      updateSelectInput(session, "mv_color", choices = valid_cols, selected = get_def("class", valid_cols))
      updateSelectInput(session, "mv_order", choices = valid_cols, selected = get_def("injection.order", valid_cols, fallback="sample_id"))
      updateSelectInput(session, "pca_color", choices = valid_cols, selected = get_def("class", valid_cols))
      updateSelectInput(session, "rsd_color", choices = valid_cols, selected = get_def("class", valid_cols))
      updateSelectInput(session, "box_color", choices = valid_cols, selected = get_def("class", valid_cols))
      updateSelectInput(session, "box_fill", choices = valid_cols, selected = get_def("batch", valid_cols, fallback="class"))
      updateSelectInput(session, "cor_order", choices = valid_cols, selected = get_def("sample_id", valid_cols))

      if(ncol(obj) > 30) updateCheckboxInput(session, "mv_text", value = FALSE)
    })

    # 3. Helper to Render Interactive/Static
    render_plot_ui <- function(plot_expr, output_id) {
      renderUI({
        if(input$interactive_mode) {
          plotly::plotlyOutput(ns(paste0(output_id, "_ly")), height = "100%")
        } else {
          plotOutput(ns(paste0(output_id, "_st")), height = "100%")
        }
      })
    }

    # Helper for Plot Logic
    make_plot <- function(id) {
      obj <- current_obj(); req(obj)
      is_valid <- function(val) !is.null(val) && val != ""

      p <- switch(id,
                  "mzrt" = massdataset::show_mz_rt_plot(obj, hex = input$mzrt_hex),
                  "mv" = {
                    req(is_valid(input$mv_color), is_valid(input$mv_order))
                    massdataset::show_sample_missing_values(obj, input$mv_color, input$mv_order, input$mv_pct, input$mv_text, input$mv_desc)
                  },
                  "pca" = {
                    req(is_valid(input$pca_color))
                    temp <- obj
                    temp <- log(temp + 1, 2)
                    if(input$pca_scale) {
                      ed <- temp@expression_data
                      scaled <- t(scale(t(ed)))
                      temp@expression_data <- as.data.frame(scaled)
                    }
                    massqc::massqc_pca(temp, color_by = input$pca_color, order_by = "sample_id", point_alpha = input$pca_alpha, point_size = input$pca_size)
                  },
                  "rsd" = {
                    massqc::massqc_cumulative_rsd_plot(obj, rsd_cutoff = input$rsd_cutoff, color = "black")
                  },
                  "boxplot" = {
                    req(is_valid(input$box_color), is_valid(input$box_fill))
                    temp <- log(obj + 1, 2)
                    massqc::massqc_sample_boxplot(temp, color_by = input$box_color, fill_by = input$box_fill, point = input$box_point) + ggtitle("Log2 Intensity")
                  },
                  "cor" = {
                    req(is_valid(input$cor_order))
                    massqc::massqc_sample_correlation(obj, cor_method = input$cor_method, order_by = input$cor_order)
                  }
      )
      return(p)
    }

    ids <- c("mzrt", "mv", "pca", "rsd", "boxplot", "cor")
    for(id in ids) {
      local({
        my_id <- id
        output[[paste0("ui_plot_", my_id)]] <- render_plot_ui(NULL, my_id)
        output[[paste0(my_id, "_st")]] <- renderPlot({ make_plot(my_id) })
        output[[paste0(my_id, "_ly")]] <- plotly::renderPlotly({ plotly::ggplotly(make_plot(my_id)) })
      })
    }

    # 4. Report Generation
    output$export_report <- downloadHandler(
      filename = function() {
        paste0("MetMiner_QC_Report_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
      },
      content = function(file) {
        # Show waiting modal
        showModal(modalDialog(
          title = "Generating Report",
          div(
            class = "text-center p-3",
            div(class = "spinner-border text-primary mb-3", role = "status",
                style = "width: 3rem; height: 3rem;"),
            tags$h5("Compiling Quarto Document...", class = "mb-2"),
            tags$p("This may take a few seconds. Please do not close the browser.",
                   class = "text-muted")
          ),
          footer = NULL,
          easyClose = FALSE,
          size = "m"
        ))

        # Ensure modal is closed
        on.exit({
          removeModal()
        }, add = TRUE)

        # Prepare plot parameters
        plot_params <- list(
          mzrt_hex    = input$mzrt_hex,
          mv_color    = if (!is.null(input$mv_color)) input$mv_color else "class",
          mv_order    = if (!is.null(input$mv_order)) input$mv_order else "injection.order",
          mv_pct      = input$mv_pct,
          mv_text     = input$mv_text,
          mv_desc     = input$mv_desc,
          pca_scale   = input$pca_scale,
          pca_color   = if (!is.null(input$pca_color)) input$pca_color else "class",
          pca_order   = "sample_id",
          pca_alpha   = input$pca_alpha,
          pca_size    = input$pca_size,
          rsd_cutoff  = input$rsd_cutoff,
          box_color   = if (!is.null(input$box_color)) input$box_color else "class",
          box_fill    = if (!is.null(input$box_fill)) input$box_fill else "batch",
          box_point   = input$box_point,
          cor_method  = input$cor_method,
          cor_order   = if (!is.null(input$cor_order)) input$cor_order else "no"
        )

        # Get data objects (strictly raw for QC)
        obj_pos <- global_data$object_pos_raw
        obj_neg <- global_data$object_neg_raw

        # Check if data exists
        if(is.null(obj_pos) && is.null(obj_neg)) {
          showNotification("No data available to generate report.", type = "error")
          return(NULL)
        }

        # Generate report
        tryCatch({
          export_qc_report(
            object_pos = obj_pos,
            object_neg = obj_neg,
            plot_params = plot_params,
            file = file
          )

          showNotification("Report generated successfully!",
                           type = "success", duration = 5)
        }, error = function(e) {
          showNotification(paste("Error generating report:", e$message),
                           type = "error", duration = 10)
        })
      }
    )
  })
}
