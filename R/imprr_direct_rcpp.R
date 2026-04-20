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
#' @param anc_correct Optional indicator for passing the anchor question.
#'   If `NULL`, `p_random` is used when supplied; otherwise the function
#'   defaults to `p_random = 0` and applies no correction.
#' @param population Choice of the target population out of
#' non-random respondents (default) or all respondents.
#' @param assumption Choice of identifying assumption when
#'   `population = "all"`: `uniform` assumes random respondents would have
#'   uniform counterfactual preferences, while `contaminated` assumes their
#'   counterfactual preferences match those of non-random respondents.
#' @param n_bootstrap Number of bootstraps. Defaults to 200.
#' @param seed Seed for \code{set.seed} for reproducibility.
#' @param weight The name of the weight column in `data`. Defaults to `NULL`,
#' which uses equal weights.
#' @param verbose Indicator for verbose output. Defaults to FALSE.
#' @param p_random Optional fixed proportion of random/inattentive respondents.
#'   When supplied, this overrides `anc_correct` and a message is shown if both
#'   are provided.
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
                              anc_correct = NULL,
                              population = "non-random",
                              assumption = "contaminated",
                              n_bootstrap = 200,
                              seed = 123456,
                              weight = NULL,
                              verbose = FALSE,
                              p_random = NULL) {
  ## Suppress global variable warning
  qoi <- outcome <- bc_estimate <- item <- NULL

  # Setup ======================================================================
  N <- nrow(data)
  if (is.null(N) || N == 0) {
    stop("There is no data to analyze. Please check the input data.")
  }

  if (!is.character(main_q) || length(main_q) != 1) {
    stop("main_q must be a single column name.")
  }
  normalized_args <- .normalize_population_args(population, assumption)
  population <- normalized_args$population
  assumption <- normalized_args$assumption

  if (population == "all" && assumption == "uniform") {
    if (!is.null(anc_correct) || !is.null(p_random)) {
      message(
        "population = 'all' with assumption = 'uniform' implies no correction; ",
        "ignoring anc_correct and p_random."
      )
    }
    random_spec <- list(method = "fixed", anc_correct = NULL, p_random = 0)
  } else {
    random_spec <- .resolve_random_response_inputs(data, anc_correct, p_random)
  }
  if (random_spec$method == "fixed") {
    if (verbose) {
      message(
        "Using the R implementation because the random-response rate is fixed ",
        "rather than estimated from anc_correct."
      )
    }
    return(
      imprr_direct(
        data = data,
        J = J,
        main_q = main_q,
        anc_correct = random_spec$anc_correct,
        population = population,
        assumption = assumption,
        n_bootstrap = n_bootstrap,
        seed = seed,
        weight = weight,
        verbose = verbose,
        p_random = random_spec$p_random
      )
    )
  }

  anc_correct <- random_spec$anc_correct
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
        paste(
          "When J is NULL, main_q must exist as a column in data",
          "so J can be inferred."
        )
      )
    }
    J <- .infer_ranking_size(data[[main_q]])
  }
  if (!is.numeric(J) || length(J) != 1 || is.na(J) ||
      J < 2 || J != as.integer(J)) {
    stop("J must be a single integer >= 2.")
  }
  J <- as.integer(J)
  ranking_cols <- paste0(main_q, "_", seq_len(J))
  missing_ranking_cols <- setdiff(ranking_cols, names(data))
  if (length(missing_ranking_cols) > 0) {
    stop(
      "Missing ranking columns for main_q: ",
      paste(missing_ranking_cols, collapse = ", ")
    )
  }

  if (is.null(weight)) {
    message("No weight column supplied; using equal weights for all observations.")
  }
  weight <- .resolve_weight_vector(data, weight, N)

  # Pre-compute constants ======================================================
  J_1 <- J - 1

  # Extract ranking columns ====================================================
  data_matrix <- as.matrix(data[, ranking_cols])
  storage.mode(data_matrix) <- "double"

  # Extract anchor correct vector ==============================================
  anc_vec <- as.numeric(data[[anc_correct]])
  weights_vec <- as.numeric(weight)

  # Run C++ bootstrap ==========================================================
  if (verbose) message("Running Rcpp bootstrap...")
  result_cpp <- bootstrap_qoi_cpp(
    data_matrix, anc_vec, weights_vec,
    as.integer(J), as.integer(n_bootstrap), as.integer(seed)
  )
  if (anyNA(result_cpp$p_random)) {
    stop(
      "Estimated non-random response rate is too small/non-finite. ",
      "Check anc_correct, weights, and J."
    )
  }
  if (verbose) message("Bootstrapping finished.")

  # Format results to match original output format =============================

  # Proportion of random responses ---------------------------------------------
  df_random_summary <- tibble::tibble(
    mean = mean(result_cpp$p_random, na.rm = TRUE),
    lower = stats::quantile(result_cpp$p_random, 0.025, na.rm = TRUE),
    upper = stats::quantile(result_cpp$p_random, 0.975, na.rm = TRUE)
  )

  # Build results tibble -------------------------------------------------------
  item_names <- ranking_cols
  all_results <- list()

  for (j in seq_len(J)) {
    target_item <- item_names[j]
    other_items <- item_names[-j]

    # Average rank -------------------------------------------------------------
    avg_rank_values <- result_cpp$avg_ranks[, j]
    all_results[[length(all_results) + 1]] <- tibble::tibble(
      item = target_item,
      qoi = "average rank",
      outcome = paste0("Avg: ", target_item),
      mean = mean(avg_rank_values, na.rm = TRUE),
      lower = stats::quantile(avg_rank_values, 0.025, na.rm = TRUE),
      upper = stats::quantile(avg_rank_values, 0.975, na.rm = TRUE)
    )

    # Pairwise probabilities ---------------------------------------------------
    for (k in seq_len(J_1)) {
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

    # Top-k probabilities ------------------------------------------------------
    for (k in seq_len(J_1)) {
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

    # Marginal probabilities ---------------------------------------------------
    for (k in seq_len(J)) {
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
