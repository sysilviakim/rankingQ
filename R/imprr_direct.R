#' Implements Plug-in Bias-Corrected Estimators for Ranking Data
#'
#' @description This function implements the bias correction of the ranking
#' distribution using a paired anchor question.
#'
#' @importFrom dplyr `%>%` mutate select group_by arrange summarise pull
#' @importFrom tidyselect matches
#' @importFrom estimatr lm_robust tidy
#' @importFrom stats runif
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
#' @param assumption Choice of identifying assumption when
#'   `population = "all"`: `uniform` assumes random respondents would have
#'   uniform counterfactual preferences, while `contaminated` assumes their
#'   counterfactual preferences match those of non-random respondents.
#' @param n_bootstrap Number of bootstraps. Defaults to 200.
#' @param seed Seed for \code{set.seed} for reproducibility.
#' @param weight A vector of weights. Defaults to NULL.
#' @param verbose Indicator for verbose output. Defaults to FALSE.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{est_p_random}{A data frame with summary statistics for the
#'     estimated proportion of random respondents, including columns
#'     \code{mean}, \code{lower}, and \code{upper} (95\% confidence interval).}
#'   \item{results}{A tibble with bias-corrected estimates grouped by
#'     \code{item}, \code{qoi} (quantity of interest), and \code{outcome},
#'     including columns \code{mean}, \code{lower}, and \code{upper}.}
#' }
#'
#' @export

imprr_direct <- function(data,
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
  estimate <- g_U <- est.p.random <- item <- qoi <-
    outcome <- bc_estimate <- NULL

  # Setup ======================================================================
  N <- nrow(data)
  if (is.null(N) || N == 0) {
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
  normalized_args <- .normalize_population_args(population, assumption)
  population <- normalized_args$population
  assumption <- normalized_args$assumption
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
  ranking_cols <- paste0(main_q, "_", seq_len(J))
  missing_ranking_cols <- setdiff(ranking_cols, names(data))
  if (length(missing_ranking_cols) > 0) {
    stop(
      "Missing ranking columns for main_q: ",
      paste(missing_ranking_cols, collapse = ", ")
    )
  }

  if (is.null(weight)) {
    weight <- rep(1, N)
  }
  if (length(weight) != N) {
    stop("weight must have the same length as the number of rows in data.")
  }

  if (population == "all" && assumption == "uniform") {
    data[[anc_correct]] <- rep(1, N) # get naive estimate for theta
  }
  # Under contaminated sampling, theta for the full population equals theta_z.

  # Pre-compute constants for efficiency =======================================
  J_factorial <- factorial(J)
  J_1 <- J - 1
  uniform_avg_rank <- (1 + J) / 2
  uniform_pairwise <- 0.5
  uniform_prob <- 1 / J
  min_p_non_random <- sqrt(.Machine$double.eps)

  # Check the validity of the input arguments ==================================

  ## List for bootstrapped results
  list_qoi <- list_prop <- vector("list", length = n_bootstrap)

  weighted_mean_safe <- function(x, w) {
    keep <- !is.na(x) & !is.na(w)
    if (!any(keep)) {
      return(NA_real_)
    }
    sum(x[keep] * w[keep]) / sum(w[keep])
  }

  # Boostrapping ===============================================================
  ## We bootstrap to account for uncertainty from the estimation of Pr(random)
  ## Sample with replacement
  ## There are three loops here with
  ### i: over n_bootstrap
  ### j: over J items
  ### k: over K estimates

  ## Save and restore RNG state
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    runif(1)
  }
  old_seed <- get(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  set.seed(seed)

  for (i in 1:n_bootstrap) {
    ## Sample indices
    index <- sample(1:nrow(data), size = nrow(data), replace = TRUE)

    ## This is the bootstrapped data
    boostrap_dat <- data[index, ]
    bootstrap_weight <- weight[index]

    # Step 1: Get the proportion of random answers -----------------------------
    ## This requires anchor questions and item order randomization
    prop_correct <- sum(boostrap_dat[[anc_correct]] * bootstrap_weight) /
      sum(bootstrap_weight)
    p_non_random <- (prop_correct - 1 / J_factorial) /
      (1 - 1 / J_factorial)
    if (!is.finite(p_non_random) || p_non_random <= min_p_non_random) {
      stop(
        "Estimated non-random response rate is too small/non-finite. ",
        "Check anc_correct, weights, and J."
      )
    }

    # Step 2: Get the naive estimates of simple quantities ---------------------
    item_names <- ranking_cols
    all_qoi_list <- list()

    for (j in 1:J) {
      # Specify each item as the target item in return
      target_item <- item_names[j]
      other_items <- item_names[-j]

      # Step 2.1: Compute weighted raw estimates directly
      Y_rank_target <- as.numeric(boostrap_dat[[target_item]])

      m_rank_target <- weighted_mean_safe(Y_rank_target, bootstrap_weight)
      m_pairwise <- vapply(
        other_items,
        FUN.VALUE = numeric(1),
        FUN = function(comp_item) {
          weighted_mean_safe(
            Y_rank_target < as.numeric(boostrap_dat[[comp_item]]),
            bootstrap_weight
          )
        }
      )
      m_top <- vapply(
        seq_len(J_1),
        FUN.VALUE = numeric(1),
        FUN = function(k) {
          weighted_mean_safe(Y_rank_target <= k, bootstrap_weight)
        }
      )
      m_marginal <- vapply(
        seq_len(J),
        FUN.VALUE = numeric(1),
        FUN = function(k) {
          weighted_mean_safe(Y_rank_target == k, bootstrap_weight)
        }
      )

      # Step 3: Get the QOI based on random responses
      ## g(random)---QOI based on uniform distribution
      gg_averagerank <- data.frame(estimate = m_rank_target) %>%
        mutate(
          outcome = paste0("Avg:", " ", target_item),
          qoi = "average rank",
          g_U = uniform_avg_rank
        )

      gg_pairwise <- data.frame(estimate = as.numeric(m_pairwise)) %>%
        mutate(
          outcome = paste0("v.", " ", other_items),
          qoi = "pairwise ranking",
          g_U = uniform_pairwise
        )

      gg_topk <- data.frame(estimate = as.numeric(m_top)) %>%
        mutate(
          outcome = paste0("Top-", "", 1:J_1),
          qoi = "top-k ranking",
          g_U = (1:J_1) / J
        )

      gg_marginal <- data.frame(estimate = as.numeric(m_marginal)) %>%
        mutate(
          outcome = paste0("Ranked", " ", 1:J),
          qoi = "marginal ranking",
          g_U = uniform_prob
        )

      # Step 4: Directly apply bias-correction (Equation 6)
      ## bc_estimate: bias-corrected estimate
      all_qoi <- rbind(
        gg_averagerank,
        gg_pairwise,
        gg_topk,
        gg_marginal
      ) %>%
        mutate(
          bc_estimate = (estimate - (g_U * (1 - p_non_random))) / p_non_random,
          item = target_item
        )

      ## Save the estimates for all quantities
      all_qoi_list[[j]] <- all_qoi
    }

    all_qoi_df <- do.call(rbind.data.frame, all_qoi_list)

    list_prop[[i]] <- 1 - p_non_random # Estimated prop of random responses
    list_qoi[[i]] <- all_qoi_df # Bias-corrected estimates of several QOIs
  }
  if (verbose) {
    message("Bootstrapping finished.")
  }

  # Summarize results
  ## Compute the mean and 95% CI based on bootstrapping
  ## Return all weights for IPW

  df_random <- do.call(rbind.data.frame, list_prop)
  colnames(df_random) <- "est.p.random"
  df_random_summary <- df_random %>%
    summarise(
      mean = mean(est.p.random),
      lower = quantile(est.p.random, 0.025),
      upper = quantile(est.p.random, 0.975)
    )

  df_qoi_summary <- do.call(rbind.data.frame, list_qoi) %>%
    group_by(item, qoi, outcome) %>%
    arrange(bc_estimate) %>%
    summarise(
      mean = mean(bc_estimate),
      lower = quantile(bc_estimate, 0.025),
      upper = quantile(bc_estimate, 0.975)
    )

  return(
    list(
      est_p_random = df_random_summary,
      results = df_qoi_summary
    )
  )
}
