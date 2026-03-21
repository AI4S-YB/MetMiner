#' Find and Remove Noise
#'
#' Detected errors or batch effects of your untargeted metabolomics data.
#' Based on missing value frequency in QC and Sample groups.
#'
#' @param object a mass_dataset class
#' @param tag Remove noise based on column names of `sample_info` in object
#' @param qc_na_freq missing value ratio of QC sample (threshold)
#' @param S_na_freq missing value ratio of tag groups (threshold)
#' @return A list containing `object_mv` (cleaned object) and `noisy_tbl` (removed features info).
#' @importFrom massdataset extract_sample_info activate_mass_dataset mutate_variable_na_freq extract_variable_info
#' @importFrom magrittr %>%
#' @importFrom dplyr rename mutate case_when filter across left_join anti_join pull if_any starts_with select
#' @export
find_noise_multiple <- function(object, tag = "class", qc_na_freq = 0.2, S_na_freq = 0.5) {

  if (!inherits(object, "mass_dataset")) {
    stop("Input object must be a 'mass_dataset' class object.")
  }

  # Clean up potential conflict column 'key'
  if("key" %in% colnames(object@sample_info)){
    object <- object %>%
      massdataset::activate_mass_dataset('sample_info') %>%
      dplyr::select(-key)
  }

  # Prepare sample info with 'key' column for grouping
  sample_info <- massdataset::extract_sample_info(object)

  if(tag == "class") {
    temp_sample_info <- sample_info %>%
      dplyr::mutate(key = class)
  } else {
    # Check if tag exists
    if(!tag %in% colnames(sample_info)) {
      stop(paste("Column", tag, "not found in sample_info"))
    }

    temp_sample_info <- sample_info %>%
      dplyr::rename("tag_col" = dplyr::all_of(tag)) %>%
      dplyr::mutate(key = dplyr::case_when(
        class == "QC" ~ "QC",
        TRUE ~ as.character(tag_col)
      ))
  }

  temp_keys <- unique(temp_sample_info$key)

  # Join key back to object for calculation
  object <- object %>%
    massdataset::activate_mass_dataset("sample_info") %>%
    dplyr::left_join(temp_sample_info %>% dplyr::select(sample_id, key), by = "sample_id")

  # Calculate NA freq for each group (QC and others)
  for (k in temp_keys) {
    # Get samples belonging to this key
    # Note: We use local variable to avoid lazy evaluation issues in loops
    current_key <- k
    temp_ids <- object %>%
      massdataset::activate_mass_dataset(what = "sample_info") %>%
      dplyr::filter(key == current_key) %>%
      dplyr::pull(sample_id)

    if(length(temp_ids) > 0) {
      object <- object %>%
        massdataset::mutate_variable_na_freq(according_to_samples = temp_ids)
    }
  }

  var_info <- massdataset::extract_variable_info(object)
  expression_data <- massdataset::extract_expression_data(object)

  # 1. Check QC NA
  qc_ids <- temp_sample_info %>% dplyr::filter(key == "QC") %>% dplyr::pull(sample_id)
  if(length(qc_ids) > 0) {
    qc_na <- rowSums(is.na(expression_data[, qc_ids, drop=FALSE])) / length(qc_ids)
  } else {
    qc_na <- rep(0, nrow(var_info)) # If no QC, assume pass
  }

  # 2. Check Sample Groups NA (Excluding QC)
  group_keys <- setdiff(temp_keys, "QC")
  group_pass <- rep(FALSE, nrow(var_info))

  if(length(group_keys) > 0) {
    for(g in group_keys) {
      g_ids <- temp_sample_info %>% dplyr::filter(key == g) %>% dplyr::pull(sample_id)
      if(length(g_ids) > 0) {
        g_na <- rowSums(is.na(expression_data[, g_ids, drop=FALSE])) / length(g_ids)
        # If this group passes the threshold, mark as TRUE
        group_pass <- group_pass | (g_na <= S_na_freq)
      }
    }
  } else {
    group_pass <- rep(TRUE, nrow(var_info)) # No groups? Pass.
  }

  # 3. Filter
  keep_idx <- (qc_na <= qc_na_freq) & group_pass

  object_mv <- object[keep_idx, ]

  # Remove the temporary 'key' column if we added it to sample_info in object
  if("key" %in% colnames(object_mv@sample_info)){
    object_mv <- object_mv %>%
      massdataset::activate_mass_dataset('sample_info') %>%
      dplyr::select(-key)
  }

  vari_ori <- massdataset::extract_variable_info(object)
  vari_filter <- massdataset::extract_variable_info(object_mv)
  vari_noisy <- dplyr::anti_join(vari_ori, vari_filter, by = "variable_id")

  out <- list(
    noisy_tbl = vari_noisy,
    object_mv = object_mv
  )
  return(out)
}
