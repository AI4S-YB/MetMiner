#' Validate sample files match sample info
#'
#' @param mode "POS" or "NEG"
#' @param QC_num.p List of QC files (POS)
#' @param S_num.p List of Subject files (POS)
#' @param QC_num.n List of QC files (NEG)
#' @param S_num.n List of Subject files (NEG)
#' @param sample_info_temp Sample info dataframe
#' @importFrom utils head
#' @export
validate_sample_files <- function(mode = "POS", QC_num.p, S_num.p, QC_num.n, S_num.n, sample_info_temp) {
  qc_files <- if (mode == "POS") QC_num.p else QC_num.n
  subj_files <- if (mode == "POS") S_num.p else S_num.n
  all_files <- c(qc_files, subj_files)

  # Remove file extensions for comparison
  clean_files <- gsub("\\.mzXML$", "", all_files, ignore.case = TRUE)

  # Check 1: Row count
  if (nrow(sample_info_temp) != length(all_files)) {
    shinyalert::shinyalert("Error",
                           paste0("Sample info count (", nrow(sample_info_temp),
                                  ") doesn't match ", mode, " files count (", length(all_files), ")"),
                           type = "error")
    return(FALSE)
  }

  # Check 2: ID consistency
  mismatch_ids <- setdiff(sample_info_temp$sample_id, clean_files)
  if (length(mismatch_ids) > 0) {
    shinyalert::shinyalert("Error",
                           HTML(paste("Sample ID mismatch in", mode, "mode:<br>",
                                      paste(head(mismatch_ids, 5), collapse = "<br>"),
                                      if(length(mismatch_ids)>5) "...and more" else "")),
                           type = "error", html = TRUE)
    return(FALSE)
  }
  return(TRUE)
}

#' Helper to check directory structure for Raw Import
#'
#' @param path Directory containing raw MS1 data folders.
#' @export
check_ms1_structure <- function(path) {
  dir_pos <- dir.exists(file.path(path, "POS"))
  dir_neg <- dir.exists(file.path(path, "NEG"))
  list(pos = dir_pos, neg = dir_neg)
}
