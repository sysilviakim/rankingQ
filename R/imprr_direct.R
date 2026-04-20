#' Implements Plug-in Bias-Corrected Estimators for Ranking Data
#'
#' @description This function implements the bias correction of the ranking
#' distribution using a paired anchor question.
#'
#' @importFrom dplyr `%>%` mutate select group_by summarise pull
#' @importFrom tidyselect matches
#' @importFrom estimatr lm_robust tidy
#' @importFrom stats runif
#'
#' @param data The input dataset with ranking data.
#' @param J The number of items in the ranking question. Defaults to NULL,
#' in which case it will be inferred from the data. When `main_q` is a single
#' column name or unquoted symbol such as `app_identity`, the function looks
#' for `app_identity_1`, `app_identity_2`, `app_identity_3`, and so on. You may
#' also supply `main_q` directly as a character vector or unquoted
#' `c(...)` expression of ranking columns such as
#' `c(party, gender, race, religion)`.
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
#' which uses equal weights. This can also be supplied as a numeric vector or
#' as an unquoted column name.
#' @param verbose Indicator for verbose output. Defaults to FALSE.
#' @param p_random Optional fixed proportion of random/inattentive respondents.
#'   When supplied, this overrides `anc_correct` and a message is shown if both
#'   are provided.
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
                         anc_correct = NULL,
                         population = "non-random",
                         assumption = "contaminated",
                         n_bootstrap = 200,
                         seed = 123456,
                         weight = NULL,
                         verbose = FALSE,
                         p_random = NULL) {
  ## Suppress global variable warning
  estimate <- g_U <- est.p.random <- item <- qoi <-
    outcome <- bc_estimate <- NULL

  # Setup ======================================================================
  N <- nrow(data)
  if (is.null(N) || N == 0) {
    stop("There is no data to analyze. Please check the input data.")
  }

  env <- parent.frame()
  main_q_info <- .resolve_main_q_columns(data, substitute(main_q), env, J)
  main_q <- main_q_info$main_q
  ranking_cols <- main_q_info$ranking_cols
  J <- main_q_info$J
  anc_correct <- .resolve_name_vector_input(
    substitute(anc_correct),
    env,
    "anc_correct",
    allow_multiple = FALSE
  )
  weight_input <- .resolve_weight_input(substitute(weight), env)

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
  if (!is.numeric(n_bootstrap) || length(n_bootstrap) != 1 ||
      is.na(n_bootstrap) || n_bootstrap < 1 ||
      n_bootstrap != as.integer(n_bootstrap)) {
    stop("n_bootstrap must be a single integer >= 1.")
  }

  if (is.null(weight_input)) {
    message("No weight column supplied; using equal weights for all observations.")
  }
  weight <- .resolve_weight_vector(data, weight_input, N)

  # Pre-compute constants for efficiency =======================================
  J_factorial <- factorial(J)
  J_1 <- J - 1
  uniform_avg_rank <- (1 + J) / 2
  uniform_pairwise <- 0.5
  uniform_prob <- 1 / J
  min_p_non_random <- sqrt(.Machine$double.eps)
  fixed_p_non_random <- NULL
  if (random_spec$method == "fixed") {
    fixed_p_non_random <- 1 - random_spec$p_random
    if (!is.finite(fixed_p_non_random) ||
        fixed_p_non_random <= min_p_non_random) {
      stop(
        "Estimated non-random response rate is too small/non-finite. ",
        "Check anc_correct, weights, and J."
      )
    }
  }

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

  # Bootstrapping ==============================================================
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

  for (i in seq_len(n_bootstrap)) {
    ## Sample indices
    index <- sample.int(N, size = N, replace = TRUE)

    ## This is the bootstrapped data -------------------------------------------
    boostrap_dat <- data[index, ]
    bootstrap_weight <- weight[index]

    # Step 1: Get the proportion of random answers -----------------------------
    ## This requires anchor questions and item order randomization
    if (random_spec$method == "anchor") {
      p_non_random <- .estimate_p_non_random_from_anchor(
        boostrap_dat[[random_spec$anc_correct]],
        bootstrap_weight,
        J
      )
      if (!is.finite(p_non_random) || p_non_random <= min_p_non_random) {
        stop(
          "Estimated non-random response rate is too small/non-finite. ",
          "Check anc_correct, weights, and J."
        )
      }
    } else {
      p_non_random <- fixed_p_non_random
    }

    # Step 2: Get the naive estimates of simple quantities ---------------------
    item_names <- ranking_cols
    all_qoi_list <- list()

    for (j in seq_len(J)) {
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

      # Step 3: Get the QOI based on random responses --------------------------
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
          outcome = paste0("Top-", "", seq_len(J_1)),
          qoi = "top-k ranking",
          g_U = seq_len(J_1) / J
        )

      gg_marginal <- data.frame(estimate = as.numeric(m_marginal)) %>%
        mutate(
          outcome = paste0("Ranked", " ", seq_len(J)),
          qoi = "marginal ranking",
          g_U = uniform_prob
        )

      # Step 4: Directly apply bias-correction (Equation 6) --------------------
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

  # Summarize results ==========================================================
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
    summarise(
      mean = mean(bc_estimate),
      lower = quantile(bc_estimate, 0.025),
      upper = quantile(bc_estimate, 0.975),
      .groups = "drop"
    )

  return(
    list(
      est_p_random = df_random_summary,
      results = df_qoi_summary
    )
  )
}
