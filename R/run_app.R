#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @param maxRequestSize upload file size mb, default: 100 mb
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options

run_metMiner <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    maxRequestSize = 100,
    ...
) {
  options(shiny.maxRequestSize = maxRequestSize * 1024^2)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
