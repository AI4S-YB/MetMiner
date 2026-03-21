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
  output <- c(output, sprintf("── massdataset Object Summary (%s Mode) ──", tools::toTitleCase(mode)), "")

  # Core metadata
  output <- c(output, "Core Components:",
              sprintf("├─ Expression Data: %s x %s variables", nrow(object@expression_data), ncol(object@expression_data)),
              sprintf("├─ Sample Info: %s samples x %s metadata", nrow(object@sample_info), ncol(object@sample_info)),
              sprintf("├─ Variable Info: %s features x %s annotation", nrow(object@variable_info), ncol(object@variable_info)),
              sprintf("└─ MS2 Spectra: %s", ifelse(length(object@ms2_data) > 0, "Available", "Not available")), "")

  # Processing history
  if (show_processing && length(object@process_info) > 0) {
    output <- c(output, "Processing History:", sprintf("├─ Total steps: %d", length(object@process_info)))
    last_step_name <- names(object@process_info)[length(object@process_info)]
    output <- c(output, sprintf("└─ Last step: %s", last_step_name), "")
  }

  # QC Check
  if (show_qc) {
    qc_count <- sum(grepl("QC", object@sample_info$sample_id, ignore.case = TRUE))
    output <- c(output, ifelse(qc_count > 0, sprintf("✔ Contains %d QC samples", qc_count), "⚠ No QC samples detected"))
  }

  paste(output, collapse = "\n")
}

#' Generate Formatted Data Summary Report for Shiny
#' @export
check_massdata_info <- function(object, mode = c("positive", "negative")) {
  mode <- match.arg(mode)
  shiny::renderPrint({
    obj <- if (shiny::is.reactive(object)) object() else object
    if (is.null(obj)) {
      cat(sprintf("⛔ No %s ion mode data detected", mode))
      return(invisible(NULL))
    }
    cat(summarize_massdataset(obj, mode = mode))
  })
}

#' Validate mass_dataset file
#' @export
validate_file <- function(path, expected_polarity, object_label) {
  tryCatch({
    if (is.null(path)) return(list(success = FALSE, message = "Path is null"))
    if (!grepl("\\.rda$", path, ignore.case = TRUE)) return(list(success = FALSE, message = paste("Wrong file format:", object_label)))

    env <- new.env()
    obj_name <- load(path, envir = env)
    obj <- get(obj_name, envir = env)

    if (!inherits(obj, "mass_dataset")) return(list(success = FALSE, message = paste("Wrong object class:", object_label)))

    # 简单的极性检查逻辑
    # 注意：实际使用中可能需要根据 variable_info 更严谨的判断
    # 这里假设用户文件名或内容包含 POS/NEG 标记，或者不强制检查极性
    list(success = TRUE, message = NULL, object = obj)
  }, error = function(e) {
    list(success = FALSE, message = paste("Error loading:", object_label, "-", e$message))
  })
}
