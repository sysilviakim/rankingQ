#' Implements Plug-in Bias-Corrected Estimators for Ranking Data (Rcpp)
#'
#' @description This function implements the bias correction of the ranking
#' distribution using a paired anchor question. This is a fast Rcpp-based
#' implementation that is approximately 200-300x faster than the tidyverse
#' version.
#'
#' @importFrom dplyr `%>%` group_by summarise
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom stats quantile
#'
#' @param data The input dataset with ranking data.
#' @param J The number of items in the ranking question. Defaults to NULL,
#' in which case it will be inferred from the data, only if the column for
#' `main_q` exists in the data.
#' @param main_q Column name for the main ranking question to be analyzed.
#' Using this argument, the function automatically looks for columns with
#' marginal rankings. For example, if `main_q` is `app_identity`, the function
#' looks for `app_identity_1`, `app_identity_2`, `app_identity_3`, and so on,
#' with an underbar separator followed by numbers.
#' @param anc_correct Indicator for passing the anchor question.
#' @param population Choice of the target population out of
#' non-random respondents (default) or all respondents.
#' @param assumption Choice of the identifying assumption if `population` is
#' set to all
#' @param n_bootstrap Number of bootstraps. Defaults to 200.
#' @param seed Seed for \code{set.seed} for reproducibility.
#' @param weight A vector of weights. Defaults to NULL.
#' @param verbose Indicator for verbose output. Defaults to FALSE.
#'
#' @return A list with two elements:
#'   \item{est_p_random}{Summary statistics for the estimated proportion of
#'     random respondents (mean, lower, upper)}
#'   \item{results}{A tibble with bias-corrected estimates for all items,
#'     including average ranks, pairwise probabilities, top-k probabilities,
#'     and marginal probabilities}
#'
#' @export

imprr_direct_rcpp <- function(data,
                              J = NULL,
                              main_q,
                              anc_correct,
                              population = "non-random",
                              assumption = "contaminated",
                              n_bootstrap = 200,
                              seed = 123456,
                              weight = NULL,
                              verbose = FALSE) {
  ## Suppress global variable warning
  qoi <- outcome <- bc_estimate <- item <- NULL

  # Setup ======================================================================
  N <- nrow(data)
  if (is.null(N)) {
    stop("There is no data to analyze. Please check the input data.")
  }

  if (!is.character(main_q) || length(main_q) != 1) {
    stop("main_q must be a single column name.")
  }
  if (!is.character(anc_correct) || length(anc_correct) != 1) {
    stop("anc_correct must be a single column name.")
  }
  if (!(anc_correct %in% names(data))) {
    stop("anc_correct column not found in data.")
  }
  if (!is.numeric(n_bootstrap) || length(n_bootstrap) != 1 ||
      is.na(n_bootstrap) || n_bootstrap < 1 ||
      n_bootstrap != as.integer(n_bootstrap)) {
    stop("n_bootstrap must be a single integer >= 1.")
  }

  if (is.null(J)) {
    if (!(main_q %in% names(data))) {
      stop(
        "When J is NULL, main_q must exist as a column in data so J can be inferred."
      )
    }
    J <- nchar(data[[main_q]][[1]])
  }
  if (!is.numeric(J) || length(J) != 1 || is.na(J) ||
      J < 2 || J != as.integer(J)) {
    stop("J must be a single integer >= 2.")
  }
  J <- as.integer(J)

  if (is.null(weight)) {
    weight <- rep(1.0, N)
  }
  if (length(weight) != N) {
    stop("weight must have the same length as the number of rows in data.")
  }

  if (population == "all" && assumption == "uniform") {
    data[[anc_correct]] <- rep(1, N)
  }

  # Pre-compute constants
  J_1 <- J - 1

  # Extract ranking columns
  ranking_cols <- paste0(main_q, "_", 1:J)
  data_matrix <- as.matrix(data[, ranking_cols])
  storage.mode(data_matrix) <- "double"

  # Extract anchor correct vector
  anc_vec <- as.numeric(data[[anc_correct]])
  weights_vec <- as.numeric(weight)

  # Run C++ bootstrap
  if (verbose) message("Running Rcpp bootstrap...")
  result_cpp <- bootstrap_qoi_cpp(
    data_matrix, anc_vec, weights_vec,
    as.integer(J), as.integer(n_bootstrap), as.integer(seed)
  )
  if (verbose) message("Bootstrapping finished.")

  # Format results to match original output format =============================

  # Proportion of random responses
  df_random_summary <- tibble::tibble(
    mean = mean(result_cpp$p_random, na.rm = TRUE),
    lower = stats::quantile(result_cpp$p_random, 0.025, na.rm = TRUE),
    upper = stats::quantile(result_cpp$p_random, 0.975, na.rm = TRUE)
  )

  # Build results tibble
  item_names <- ranking_cols
  all_results <- list()

  for (j in 1:J) {
    target_item <- item_names[j]
    other_items <- item_names[-j]

    # Average rank
    avg_rank_values <- result_cpp$avg_ranks[, j]
    all_results[[length(all_results) + 1]] <- tibble::tibble(
      item = target_item,
      qoi = "average rank",
      outcome = paste0("Avg: ", target_item),
      mean = mean(avg_rank_values, na.rm = TRUE),
      lower = stats::quantile(avg_rank_values, 0.025, na.rm = TRUE),
      upper = stats::quantile(avg_rank_values, 0.975, na.rm = TRUE)
    )

    # Pairwise probabilities
    for (k in 1:J_1) {
      col_idx <- (j - 1) * J_1 + k
      pairwise_values <- result_cpp$pairwise[, col_idx]
      all_results[[length(all_results) + 1]] <- tibble::tibble(
        item = target_item,
        qoi = "pairwise ranking",
        outcome = paste0("v. ", other_items[k]),
        mean = mean(pairwise_values, na.rm = TRUE),
        lower = stats::quantile(pairwise_values, 0.025, na.rm = TRUE),
        upper = stats::quantile(pairwise_values, 0.975, na.rm = TRUE)
      )
    }

    # Top-k probabilities
    for (k in 1:J_1) {
      col_idx <- (j - 1) * J_1 + k
      topk_values <- result_cpp$topk[, col_idx]
      all_results[[length(all_results) + 1]] <- tibble::tibble(
        item = target_item,
        qoi = "top-k ranking",
        outcome = paste0("Top-", k),
        mean = mean(topk_values, na.rm = TRUE),
        lower = stats::quantile(topk_values, 0.025, na.rm = TRUE),
        upper = stats::quantile(topk_values, 0.975, na.rm = TRUE)
      )
    }

    # Marginal probabilities
    for (k in 1:J) {
      col_idx <- (j - 1) * J + k
      marginal_values <- result_cpp$marginal[, col_idx]
      all_results[[length(all_results) + 1]] <- tibble::tibble(
        item = target_item,
        qoi = "marginal ranking",
        outcome = paste0("Ranked ", k),
        mean = mean(marginal_values, na.rm = TRUE),
        lower = stats::quantile(marginal_values, 0.025, na.rm = TRUE),
        upper = stats::quantile(marginal_values, 0.975, na.rm = TRUE)
      )
    }
  }

  df_qoi_summary <- do.call(rbind, all_results)

  return(
    list(
      est_p_random = df_random_summary,
      results = df_qoi_summary
    )
  )
}
