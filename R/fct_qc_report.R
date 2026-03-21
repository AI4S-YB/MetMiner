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

  # 0. 检查参数并设置默认值
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

  # 1. 检查 Quarto
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

  # 2. 定位模板
  # 优先检查包内路径
  template_path <- system.file("app/report_template.qmd", package = "MetMiner")

  # 开发环境备选路径
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

  # 3. 创建临时工作目录
  work_dir <- tempfile(pattern = "quarto_report_")
  dir.create(work_dir, recursive = TRUE)

  # 复制模板
  temp_qmd <- file.path(work_dir, "report.qmd")
  file.copy(template_path, temp_qmd, overwrite = TRUE)

  # 4. 保存数据对象 - 使用 saveRDS 替代 save
  if (!is.null(object_pos)) {
    saveRDS(object_pos, file = file.path(work_dir, "object_pos.rds"))
  }
  if (!is.null(object_neg)) {
    saveRDS(object_neg, file = file.path(work_dir, "object_neg.rds"))
  }

  # 5. 准备渲染参数
  params_list <- list(
    work_dir = work_dir,
    viz = plot_params
  )

  # 6. 渲染 Quarto 文档
  old_wd <- getwd()
  setwd(work_dir)  # 设置工作目录到临时目录

  tryCatch({
    # 渲染文档
    quarto::quarto_render(
      input = "report.qmd",
      output_file = "report.html",
      execute_params = params_list,
      quiet = FALSE
    )

    # 检查输出文件
    output_file <- file.path(work_dir, "report.html")

    if (!file.exists(output_file)) {
      stop("Quarto output file was not generated.")
    }

    # 7. 复制到目标位置
    if (!dir.exists(dirname(file))) {
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    }

    success <- file.copy(output_file, file, overwrite = TRUE)

    if (!success) {
      stop("Failed to copy output file.")
    }

    message("Report generated successfully: ", file)

  }, error = function(e) {
    # 恢复工作目录
    setwd(old_wd)
    # 清理
    unlink(work_dir, recursive = TRUE)
    stop("Error generating report: ", e$message)
  })

  # 恢复工作目录
  setwd(old_wd)

  # 8. 清理（可选，调试时可注释掉）
  unlink(work_dir, recursive = TRUE)

  return(invisible(file))
}
