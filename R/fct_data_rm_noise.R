#' Find and Remove Noise
#'
#' Detects errors or batch effects in untargeted metabolomics data
#' based on missing value frequency in QC and sample groups,
#' with optional RSD-based filtering.
#'
#' @param object A mass_dataset class object.
#' @param tag Column name in sample_info used for grouping (default "class").
#' @param qc_na_freq Missing value ratio threshold for QC samples.
#' @param S_na_freq Missing value ratio threshold for sample groups.
#' @param do_rsd Whether to apply RSD-based filtering after MV filtering.
#' @param rsd_cutoff RSD threshold (%) for QC samples (only used if do_rsd = TRUE).
#' @return A cleaned mass_dataset object, or NULL if no features survive filtering.
#' @importFrom massdataset extract_sample_info extract_expression_data
#'   extract_variable_info activate_mass_dataset mutate_rsd
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull
#' @export
find_noise_multiple <- function(object, tag = "class", qc_na_freq = 0.2, S_na_freq = 0.5,
                                 do_rsd = FALSE, rsd_cutoff = 30) {

  if (!inherits(object, "mass_dataset")) {
    stop("Input object must be a 'mass_dataset' class object.")
  }

  sample_info <- massdataset::extract_sample_info(object)

  # Build grouping vector
  if (tag == "class") {
    group_vector <- sample_info$class
  } else {
    if (!tag %in% colnames(sample_info)) {
      stop(paste("Column", tag, "not found in sample_info"))
    }
    group_vector <- ifelse(sample_info$class == "QC", "QC",
                           as.character(sample_info[[tag]]))
  }
  unique_groups <- unique(group_vector)
  expression_data <- massdataset::extract_expression_data(object)

  # 1. QC NA check
  qc_idx <- which(group_vector == "QC")
  qc_na_ratios <- if (length(qc_idx) > 0) {
    rowMeans(is.na(expression_data[, qc_idx, drop = FALSE]))
  } else {
    rep(0, nrow(expression_data))
  }

  # 2. Group NA check (exclude QC)
  non_qc_groups <- setdiff(unique_groups, "QC")
  if (length(non_qc_groups) > 0) {
    group_pass_list <- lapply(non_qc_groups, function(grp) {
      g_idx <- which(group_vector == grp)
      if (length(g_idx) == 0) return(rep(FALSE, nrow(expression_data)))
      g_na <- rowMeans(is.na(expression_data[, g_idx, drop = FALSE]))
      return(g_na <= S_na_freq)
    })
    any_group_pass <- Reduce(`|`, group_pass_list)
  } else {
    any_group_pass <- rep(TRUE, nrow(expression_data))
  }

  keep_mv <- (qc_na_ratios <= qc_na_freq) & any_group_pass
  keep_mv[is.na(keep_mv)] <- FALSE

  keep_indices <- which(unname(keep_mv))
  if (length(keep_indices) == 0) {
    warning("No features kept after MV filter.")
    return(NULL)
  }
  object_mv <- object[keep_indices, ]

  # 3. Optional RSD filtering
  if (do_rsd) {
    s_info_curr <- massdataset::extract_sample_info(object_mv)
    qc_ids <- s_info_curr %>%
      dplyr::filter(class == "QC") %>%
      dplyr::pull(sample_id)

    if (length(qc_ids) > 1) {
      object_mv <- massdataset::mutate_rsd(object_mv,
                                            according_to_samples = qc_ids)
      object_mv <- object_mv %>%
        massdataset::activate_mass_dataset(what = "variable_info") %>%
        dplyr::filter(rsd < rsd_cutoff)
    }
  }

  return(object_mv)
}
