#' Calculate Outlier Table (Simulating masscleaner::extract_outlier_table structure)
#'
#' @param object A mass_dataset object
#' @param th_na NA percentage threshold (0-1)
#' @param th_sd SD Fold Change threshold
#' @param th_mad MAD Fold Change threshold
#' @param th_dist PCA Mahalanobis distance P-value threshold
#'
#' @return A data.frame with logical columns: according_to_na, pc_sd, pc_mad, according_to_distance
#' @importFrom stats cov mahalanobis pchisq prcomp var
#' @export
calc_outlier_table <- function(object, th_na, th_sd, th_mad, th_dist) {
  if(is.null(object)) return(NULL)

  expr <- massdataset::extract_expression_data(object)
  s_info <- massdataset::extract_sample_info(object)
  sample_ids <- s_info$sample_id

  # 1. NA Percentage
  na_pct <- colMeans(is.na(expr))
  res_na <- na_pct > th_na

  # 2. PCA Distance (Robust Mahalanobis)
  res_dist <- rep(FALSE, length(sample_ids))

  # Only run PCA if enough features and samples
  if(ncol(expr) > 3 && nrow(expr) > 2) {
    # a. Imputation (Row-wise mean for features)
    expr_imp <- as.matrix(expr)
    row_means <- rowMeans(expr_imp, na.rm = TRUE)

    # Filter out features that are ALL NA
    valid_rows <- !is.na(row_means)
    expr_imp <- expr_imp[valid_rows, , drop = FALSE]
    row_means <- row_means[valid_rows]

    # Identify indices of NAs and impute
    na_idx <- which(is.na(expr_imp), arr.ind = TRUE)
    if(nrow(na_idx) > 0) {
      expr_imp[na_idx] <- row_means[na_idx[, 1]]
    }

    # b. Zero Variance Filter
    row_vars <- apply(expr_imp, 1, var)
    keep_feat <- !is.na(row_vars) & row_vars > 1e-12
    expr_imp <- expr_imp[keep_feat, , drop = FALSE]

    # Ensure we still have enough dimensions
    if(nrow(expr_imp) >= 2) {
      pca_res <- prcomp(t(expr_imp), scale. = TRUE, center = TRUE)

      # Use first 2 PCs (or min available)
      n_pcs <- min(2, ncol(pca_res$x))
      if(n_pcs >= 1) {
        pc_scores <- pca_res$x[, 1:n_pcs, drop=FALSE]

        if(n_pcs >= 2) {
          center <- colMeans(pc_scores)
          cov_mat <- cov(pc_scores)
          try({
            d2 <- mahalanobis(pc_scores, center, cov_mat)
            p_vals <- pchisq(d2, df = n_pcs, lower.tail = FALSE)
            res_dist <- p_vals < th_dist
          }, silent = TRUE)
        }
      }
    }
  }

  # 3. Intensity Heuristics (SD / MAD)
  sum_int <- colSums(expr, na.rm = TRUE)

  # SD Fold
  mean_int <- mean(sum_int, na.rm = TRUE)
  if(is.na(mean_int) || mean_int == 0) mean_int <- 1
  ratio_sd <- sum_int / mean_int
  res_sd <- (ratio_sd > th_sd) | (ratio_sd < 1/th_sd)

  # MAD Fold
  med_int <- median(sum_int, na.rm = TRUE)
  if(is.na(med_int) || med_int == 0) med_int <- 1
  ratio_mad <- sum_int / med_int
  res_mad <- (ratio_mad > th_mad) | (ratio_mad < 1/th_mad)

  # Construct Table (Naming to match masscleaner::extract_outlier_table)
  outlier_tbl <- data.frame(
    according_to_na = res_na,
    pc_sd = res_sd,
    pc_mad = res_mad,
    according_to_distance = res_dist
  )

  rownames(outlier_tbl) <- sample_ids
  return(outlier_tbl)
}

#' Process Outliers in Mass Spectrometry Data
#'
#' @param object A mass_dataset object
#' @param mv_method Detection method ("By tidymass" or "By myself")
#' @param by_witch Parameters for automated detection (regex patterns for column names)
#' @param outlier_samples Manually specified outlier samples
#' @param outlier_table Precomputed outlier table (for "By tidymass" method)
#'
#' @return A list containing:
#' - $object: Filtered mass_dataset object
#' - $outlier_ids: Identified outlier sample IDs
#' - $message: Processing status messages
#' @export
process_outliers <- function(object,
                             mv_method = c("By tidymass", "By myself"),
                             by_witch = NULL,
                             outlier_samples = NULL,
                             outlier_table = NULL) {
  # check parameters
  mv_method <- match.arg(mv_method)
  if (!inherits(object, "mass_dataset")) {
    return(list(
      error = "Invalid input: object must be a mass_dataset",
      object = NULL,
      outlier_ids = NULL
    ))
  }

  tryCatch({
    if (mv_method == "By tidymass") {
      # Handle case where outlier_table might be NULL if calc failed
      if(is.null(outlier_table)) stop("Outlier table calculation failed.")

      outlier_ids <- outlier_table %>%
        tibble::rownames_to_column("sample_id") %>%
        tidyr::pivot_longer(
          cols = -sample_id,
          names_to = "condition",
          values_to = "judge"
        ) %>%
        # Use str_detect to match criteria selected in UI (e.g. "na" matches "according_to_na")
        dplyr::filter(stringr::str_detect(condition, paste(by_witch, collapse = "|"))) %>%
        dplyr::group_by(sample_id) %>%
        dplyr::summarise(is_outlier = all(judge == TRUE)) %>%
        dplyr::filter(is_outlier) %>%
        dplyr::pull(sample_id)
    } else {
      outlier_ids <- outlier_samples
    }

    if (length(outlier_ids) > 0 && !"none" %in% outlier_ids) {
      filtered_object <- object %>%
        massdataset::activate_mass_dataset(what = "sample_info") %>%
        dplyr::filter(!sample_id %in% outlier_ids)
      return(list(
        object = filtered_object,
        outlier_ids = outlier_ids,
        message = paste("Removed", length(outlier_ids), "outliers")
      ))
    } else {
      return(list(
        object = object,
        outlier_ids = NULL,
        message = "No outliers removed"
      ))
    }
  }, error = function(e) {
    return(list(
      error = paste("Processing failed:", e$message),
      object = NULL,
      outlier_ids = NULL
    ))
  })
}
