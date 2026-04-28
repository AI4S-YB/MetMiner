#' Export QC Report (Quarto)
#'
#' Generates an HTML report using the parameterized Quarto template.
#'
#' @param object_pos Positive mode mass_dataset object (or NULL)
#' @param object_neg Negative mode mass_dataset object (or NULL)
#' @param plot_params A list containing all plotting parameters
#' @param file Output file path provided by downloadHandler
#'
#' @importFrom quarto quarto_render quarto_path
#' @export
export_qc_report <- function(object_pos, object_neg, plot_params, file) {

  # 0. Check parameters and set defaults
  if (is.null(plot_params)) {
    plot_params <- list(
      mzrt_hex = TRUE,
      mv_color = "class",
      mv_order = "injection.order",
      mv_pct = TRUE,
      mv_text = FALSE,
      mv_desc = FALSE,
      pca_scale = TRUE,
      pca_color = "class",
      pca_alpha = 0.8,
      pca_size = 3,
      rsd_cutoff = 30,
      box_color = "class",
      box_fill = "batch",
      box_point = FALSE,
      cor_method = "pearson",
      cor_order = "no",
      pca_order = "sample_id"
    )
  }

  # 1. Check Quarto
  q_path <- NULL
  tryCatch({
    q_path <- quarto::quarto_path()
  }, error = function(e) {
    common_paths <- c(
      "/usr/local/bin/quarto",
      "/opt/homebrew/bin/quarto",
      Sys.which("quarto"),
      Sys.getenv("QUARTO_PATH")
    )

    for (p in common_paths) {
      if (!is.na(p) && nchar(p) > 0 && file.exists(p)) {
        q_path <- p
        Sys.setenv(QUARTO_PATH = p)
        break
      }
    }
  })

  if (is.null(q_path) || !file.exists(q_path)) {
    stop("Quarto CLI not found. Please install Quarto from https://quarto.org/")
  }

  # 2. Locate template
  # Prefer package-internal path
  template_path <- system.file("app/report_template.qmd", package = "MetMiner")

  # Dev environment fallback paths
  if (template_path == "" || !file.exists(template_path)) {
    dev_paths <- c(
      "inst/app/report_template.qmd",
      "app/report_template.qmd",
      "report_template.qmd"
    )

    for (p in dev_paths) {
      if (file.exists(p)) {
        template_path <- normalizePath(p)
        break
      }
    }
  }

  if (template_path == "" || !file.exists(template_path)) {
    stop("Report template not found.")
  }

  # 3. Create temporary working directory
  work_dir <- tempfile(pattern = "quarto_report_")
  dir.create(work_dir, recursive = TRUE)

  # Copy template
  temp_qmd <- file.path(work_dir, "report.qmd")
  file.copy(template_path, temp_qmd, overwrite = TRUE)

  # 4. Save data objects - use saveRDS instead of save
  if (!is.null(object_pos)) {
    saveRDS(object_pos, file = file.path(work_dir, "object_pos.rds"))
  }
  if (!is.null(object_neg)) {
    saveRDS(object_neg, file = file.path(work_dir, "object_neg.rds"))
  }

  # 5. Prepare render parameters
  params_list <- list(
    work_dir = work_dir,
    viz = plot_params
  )

  # 6. Render Quarto document
  old_wd <- getwd()
  setwd(work_dir)  # Set working directory to temp directory

  tryCatch({
    # Render document
    quarto::quarto_render(
      input = "report.qmd",
      output_file = "report.html",
      execute_params = params_list,
      quiet = FALSE
    )

    # Check output file
    output_file <- file.path(work_dir, "report.html")

    if (!file.exists(output_file)) {
      stop("Quarto output file was not generated.")
    }

    # 7. Copy to target location
    if (!dir.exists(dirname(file))) {
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    }

    success <- file.copy(output_file, file, overwrite = TRUE)

    if (!success) {
      stop("Failed to copy output file.")
    }

    message("Report generated successfully: ", file)

  }, error = function(e) {
    # Restore working directory
    setwd(old_wd)
    # Cleanup
    unlink(work_dir, recursive = TRUE)
    stop("Error generating report: ", e$message)
  })

  # Restore working directory
  setwd(old_wd)

  # 8. Cleanup (optional, comment out for debugging)
  unlink(work_dir, recursive = TRUE)

  return(invisible(file))
}
