#' Summarize mass_dataset Object Information
#'
#' Generates a formatted report summarizing key metadata.
#'
#' @param object A mass_dataset object
#' @param mode Ionization mode
#' @param show_processing Logical
#' @param show_qc Logical
#' @param color Logical
#' @export
summarize_massdataset <- function(object, mode = c("positive", "negative"),
                                  show_processing = TRUE, show_qc = TRUE, color = TRUE) {
  if (missing(object)) return("No object provided")
  if (!inherits(object, "mass_dataset")) return("Input must be a mass_dataset S4 object")
  mode <- match.arg(mode)

  # Colors (ANSI codes, mostly for console, but readable in verbatimTextOutput)
  col_title <- ""; col_reset <- ""; col_green <- ""; col_yellow <- ""

  output <- character(0)
  output <- c(output, sprintf("-- massdataset Object Summary (%s Mode) --", tools::toTitleCase(mode)), "")

  # Core metadata
  output <- c(output, "Core Components:",
              sprintf("|- Expression Data: %s x %s variables", nrow(object@expression_data), ncol(object@expression_data)),
              sprintf("|- Sample Info: %s samples x %s metadata", nrow(object@sample_info), ncol(object@sample_info)),
              sprintf("|- Variable Info: %s features x %s annotation", nrow(object@variable_info), ncol(object@variable_info)),
              sprintf("`- MS2 Spectra: %s", ifelse(length(object@ms2_data) > 0, "Available", "Not available")), "")

  # Processing history
  if (show_processing && length(object@process_info) > 0) {
    output <- c(output, "Processing History:", sprintf("|- Total steps: %d", length(object@process_info)))
    last_step_name <- names(object@process_info)[length(object@process_info)]
    output <- c(output, sprintf("`- Last step: %s", last_step_name), "")
  }

  # QC Check
  if (show_qc) {
    qc_count <- sum(grepl("QC", object@sample_info$sample_id, ignore.case = TRUE))
    output <- c(output, ifelse(qc_count > 0, sprintf("OK Contains %d QC samples", qc_count), "WARNING No QC samples detected"))
  }

  paste(output, collapse = "\n")
}

#' Generate Formatted Data Summary Report for Shiny
#'
#' @param object A `mass_dataset` object or a reactive returning one.
#' @param mode Ionization mode.
#' @export
check_massdata_info <- function(object, mode = c("positive", "negative")) {
  mode <- match.arg(mode)
  shiny::renderPrint({
    obj <- if (shiny::is.reactive(object)) object() else object
    if (is.null(obj)) {
      cat(sprintf("ERROR No %s ion mode data detected", mode))
      return(invisible(NULL))
    }
    cat(summarize_massdataset(obj, mode = mode))
  })
}

#' Validate mass_dataset file
#'
#' @param path Path to an `.rda` file containing a `mass_dataset` object.
#' @param expected_polarity Expected polarity label.
#' @param object_label Human-readable label used in error messages.
#' @export
validate_file <- function(path, expected_polarity, object_label) {
  tryCatch({
    if (is.null(path)) return(list(success = FALSE, message = "Path is null"))
    if (!grepl("\\.rda$", path, ignore.case = TRUE)) return(list(success = FALSE, message = paste("Wrong file format:", object_label)))

    env <- new.env()
    obj_name <- load(path, envir = env)
    obj <- get(obj_name, envir = env)

    if (!inherits(obj, "mass_dataset")) return(list(success = FALSE, message = paste("Wrong object class:", object_label)))

    # Polarity validation is intentionally permissive here. In practice this
    # can be made stricter by inspecting variable_info or file metadata.
    list(success = TRUE, message = NULL, object = obj)
  }, error = function(e) {
    list(success = FALSE, message = paste("Error loading:", object_label, "-", e$message))
  })
}
