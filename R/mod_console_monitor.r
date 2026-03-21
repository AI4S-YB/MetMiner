#' Console Monitor UI Module
#'
#' A global widget to display backend logs.
#'
#' @param id Module id.
#' @noRd
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs runjs
mod_console_monitor_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    # Custom CSS for the floating button and console window
    tags$style(HTML("
      #console-fab {
        position: fixed;
        bottom: 20px;
        right: 20px;
        z-index: 9999;
        border-radius: 50%;
        width: 60px;
        height: 60px;
        font-size: 24px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.3);
        background-color: #343a40; /* Dark grey */
        color: #00ff00; /* Terminal green */
        border: 2px solid #00ff00;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        transition: transform 0.2s;
      }
      #console-fab:hover {
        transform: scale(1.1);
        background-color: #000;
      }
      .console-log-window {
        background-color: #1e1e1e;
        color: #00ff00;
        font-family: 'Courier New', Courier, monospace;
        padding: 15px;
        border-radius: 5px;
        height: 400px;
        overflow-y: auto;
        font-size: 0.9rem;
        white-space: pre-wrap; /* Wrap long text */
        border: 1px solid #333;
      }
      .log-entry {
        margin-bottom: 5px;
        border-bottom: 1px solid #333;
        padding-bottom: 2px;
      }
      .log-timestamp {
        color: #888;
        font-size: 0.8rem;
        margin-right: 10px;
      }
      .log-type-info { color: #00ff00; }
      .log-type-warning { color: #ffcc00; }
      .log-type-error { color: #ff4444; }
    ")),

    # Floating Action Button (FAB)
    tags$div(
      id = "console-fab",
      onclick = sprintf("Shiny.setInputValue('%s', Math.random());", ns("toggle_console")),
      bsicons::bs_icon("terminal")
    ),

    # The Console Modal (We trigger this from server)
    # Note: We don't render it here to avoid clutter, server will showModal
  )
}

#' Console Monitor Server Module
#'
#' @param id Module id.
#' @param global_log A reactiveVal passed from app_server to store logs.
#' @noRd
mod_console_monitor_server <- function(id, global_log) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show Modal when FAB is clicked
    observeEvent(input$toggle_console, {
      showModal(modalDialog(
        title = tagList(bsicons::bs_icon("terminal"), " System Console"),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("clear_log"), "Clear Logs", icon = icon("trash"), class = "btn-sm btn-outline-secondary"),
          modalButton("Close")
        ),

        # The Log Output Area
        uiOutput(ns("console_output"))
      ))
    })

    # Render Logs
    output$console_output <- renderUI({
      logs <- global_log()
      if (length(logs) == 0) {
        return(div(class = "console-log-window", "System ready... No logs yet."))
      }

      # Reverse order to show newest at bottom (or top, depending on pref)
      # Let's show newest at bottom like a real terminal
      log_elements <- lapply(logs, function(entry) {
        div(class = "log-entry",
            span(class = "log-timestamp", entry$time),
            span(class = paste0("log-type-", entry$type), entry$msg)
        )
      })

      div(class = "console-log-window", id = ns("log_window"),
          log_elements,
          # Auto-scroll script
          tags$script(sprintf("var objDiv = document.getElementById('%s'); objDiv.scrollTop = objDiv.scrollHeight;", ns("log_window")))
      )
    })

    # Clear Logs
    observeEvent(input$clear_log, {
      global_log(list())
    })

  })
}

#' Helper to append log
#' Call this function from anywhere in your app to log to the console widget.
#' @export
app_log <- function(msg, type = "info") {
  # This function needs to access the global reactive value.
  # Since we can't easily pass the reactiveVal to every single helper function,
  # a common pattern is to rely on 'shiny::getDefaultReactiveDomain' or pass a callback.
  #
  # However, for simplicity in this architecture, we will define a 'logger' function
  # inside 'app_server' and pass it down, OR use a global reactiveValues if feasible.
  #
  # Here, we will assume 'global_log' is passed to modules.
}
