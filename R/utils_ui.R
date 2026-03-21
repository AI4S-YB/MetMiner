#' Custom Horizontal Rule
#'
#' Creates a custom styled horizontal rule.
#' @param style A string indicating the style of the horizontal rule.
#' @return An HTML horizontal rule element with custom style.
#' @importFrom shiny tags
#' @noRd
custom_hr <- function(style) {
  shiny::tags$hr(style = style)
}

#' Main Horizontal Rule
#'
#' This function creates the main styled horizontal rule used in the UI.
#' @return An HTML horizontal rule element for main style.
#' @noRd
hr_main <- function() {
  custom_hr("border-top: 6px double #008080; border-bottom: 3px solid #008080;")
}

#' Bar Horizontal Rule
#'
#' This function creates a bar styled horizontal rule.
#' @return An HTML horizontal rule element for bar style.
#' @noRd
hr_bar <- function() {
  custom_hr("border-top: 3px dotted #008080;")
}

#' Head Horizontal Rule
#'
#' This function creates a header styled horizontal rule.
#' @return An HTML horizontal rule element for header style.
#' @noRd
hr_head <- function() {
  custom_hr("border: 0; padding-top: 1.5px; background: linear-gradient(to right, transparent, #008080, transparent);")
}

#' Website Logo
#'
#' MetMiner logo.
#' @return shinyDashboardLogoDIY object.
#' @param version version of metminer
#' @importFrom dashboardthemes shinyDashboardLogoDIY
#' @noRd
customLogo <- function(version) {
  dashboardthemes::shinyDashboardLogoDIY(
    boldText = "Zhang Lab",
    mainText = "MetMiner",
    textSize = 14,
    badgeText = version,
    badgeTextColor = "white",
    badgeTextSize = 2,
    badgeBackColor = "#40E0D0",
    badgeBorderRadius = 3
  )
}

#' Reformed textInput
#'
#' Helper to create a textInput with a hover title.
#' @param inputId see `shiny::textInput`
#' @param label see `shiny::textInput`
#' @param value see `shiny::textInput`
#' @param placeholder see `shiny::textInput`
#' @param title hover_text of this input box
#' @importFrom shiny div textInput
#' @noRd
textInput_div = function(inputId, label, value, placeholder, title) {
  shiny::div(
    shiny::textInput(
      inputId = inputId,
      label = label,
      value = value,
      placeholder = placeholder
    ),
    title = title
  )
}

#' Reformed selectInput_div
#'
#' Helper to create a selectInput with a hover title.
#' @param inputId see `shiny::selectInput`
#' @param label see `shiny::selectInput`
#' @param choices see `shiny::selectInput`
#' @param selected see `shiny::selectInput`
#' @param multiple see `shiny::selectInput`
#' @param title hover_text of this input box
#' @importFrom shiny div selectInput
#' @noRd
selectInput_div = function(inputId, label, choices, selected, multiple, title) {
  shiny::div(
    shiny::selectInput(
      inputId = inputId,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple,
    ),
    title = title
  )
}

#' Common App Footer
#'
#' A standardized footer to be used across all modules.
#' @return HTML tag list
#' @export
metminer_footer <- function() {
  shiny::tags$footer(
    class = "footer mt-auto py-3 bg-light text-center border-top",
    style = "width: 100%; margin-top: 50px !important;",
    shiny::div(
      class = "container-fluid",
      shiny::div(
        class = "row align-items-center justify-content-center",
        shiny::div(
          class = "col-md-12",
          shiny::span(class = "text-muted small", "© 2024-2025 MetMiner | "),
          shiny::span(class = "text-primary small fw-bold", "State Key Laboratory of Crop Stress Adaptation and Improvement"),
          shiny::br(),
          shiny::span(class = "text-muted small", style="font-size: 0.8rem;", "Henan University, Kaifeng 475004, China")
        )
      )
    )
  )
}
