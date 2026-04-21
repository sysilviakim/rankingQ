#' Computes Bias-Correction Weights for Ranking Data
#'
#' @description This function implements the bias correction of the ranking
#' distribution using a paired anchor question, using the IPW estimator.
#'
#' @details `imprr_weights()` enumerates the full permutation space of rankings,
#' so its computational cost grows factorially in `J`. In practice, it is best
#' suited to small or moderate ranking questions. For larger `J`, prefer
#' `imprr_direct()` or `imprr_direct_rcpp()`.
#'
#' @importFrom dplyr `%>%` mutate select group_by left_join arrange summarise
#'   count rename
#' @importFrom tidyselect matches
#' @importFrom tidyr unite
#' @importFrom tibble tibble
#' @importFrom combinat permn
#' @importFrom stats quantile
#'
#' @param data The input dataset with ranking data.
#' @param J The number of items in the ranking question. Defaults to NULL,
#' in which case it will be inferred from the data.
#' @param main_q Ranking question to be analyzed. When `main_q` is a single
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
#' @param weight The name of the weight column in `data`. Defaults to `NULL`,
#' which uses equal weights. This can also be supplied as a numeric vector or
#' as an unquoted column name.
#' @param ranking The name of the column that will store the full ranking
#' profile. Defaults to "ranking". If `main_q` exists in the data, the produced
#' column should be identical to `main_q`. However, the function defaults to
#' creating another column by combining marginal rankings, just in case.
#' @param p_random Optional fixed proportion of random/inattentive respondents.
#'   When supplied, this overrides `anc_correct` and a message is shown if both
#'   are provided.
#'
#' @returns A list with three elements:
#' \describe{
#'   \item{est_p_random}{A numeric value representing the estimated proportion
#'     of random responses.}
#'   \item{results}{A data frame with the original data augmented with a
#'     \code{weights} column containing inverse probability weights and a
#'     \code{ranking} column with unified ranking patterns.}
#'   \item{rankings}{A data frame with ranking patterns, observed proportions
#'     (\code{prop_obs}), bias-corrected proportions (\code{prop_bc}), and
#'     inverse probability weights (\code{weights}) for each permutation.}
#' }
#'
#' @examples
#' out <- imprr_weights(
#'   identity,
#'   main_q = "app_identity",
#'   anc_correct = "anc_correct_identity"
#' )
#' head(out$results)
#' head(out$rankings)
#'
#' @export

imprr_weights <- function(data,
                          J = NULL,
                          main_q,
                          anc_correct = NULL,
                          population = "non-random",
                          assumption = "contaminated",
                          weight = NULL,
                          ranking = "ranking",
                          p_random = NULL) {
  ## Suppress global variable warning
  count <- n <- n_adj <- n_renormalized <- prop <- w <-
    prop_obs <- weights <- prop_bc <- NULL

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
  main_q_name <- if (length(main_q) == 1L) main_q[[1]] else NULL

  if (is.null(ranking)) {
    if (main_q_info$has_full_ranking_col) {
      ranking <- main_q_name
    } else {
      ranking <- "ranking"
    }
  }
  if (!is.character(ranking) || length(ranking) != 1 || is.na(ranking) ||
      !nzchar(ranking)) {
    stop("ranking must be a single column name.")
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

  conflicting_outputs <- character()
  if (!identical(ranking, main_q_name) && any(names(data) == ranking)) {
    conflicting_outputs <- c(conflicting_outputs, ranking)
  }
  if (any(names(data) == "weights")) {
    conflicting_outputs <- c(conflicting_outputs, "weights")
  }
  conflicting_outputs <- unique(conflicting_outputs)
  if (length(conflicting_outputs) > 0) {
    stop(
      "data already contains output column(s): ",
      paste(conflicting_outputs, collapse = ", "),
      ". imprr_weights() does not overwrite existing output columns. ",
      "Please rename or drop them manually before calling imprr_weights()."
    )
  }

  if (is.null(weight_input)) {
    message("No weight column supplied; using equal weights for all observations.")
  }
  weight <- .resolve_weight_vector(data, weight_input, N)

  # Pre-compute factorial for efficiency =======================================
  J_factorial <- factorial(J)

  # Check the validity of the input arguments ==================================
  ## Main ranking only

  glo_app <- data[, ranking_cols, drop = FALSE]

  # Step 1: Get the proportion of random answers -------------------------------
  if (random_spec$method == "anchor") {
    p_non_random <- .estimate_p_non_random_from_anchor(
      data[[random_spec$anc_correct]],
      weight,
      J
    )
  } else {
    p_non_random <- 1 - random_spec$p_random
  }
  if (!is.finite(p_non_random) || p_non_random <= sqrt(.Machine$double.eps)) {
    stop(
      "Estimated non-random response rate is too small/non-finite. ",
      "Check anc_correct, weights, and J."
    )
  }

  # Step 2: Get the uniform distribution ---------------------------------------
  U <- rep(1 / J_factorial, J_factorial)

  # Step 3: Get the observed PMF based on raw data -----------------------------
  ## Get raw counts of ranking profiles
  D_0 <- glo_app
  D_0[[ranking]] <- .collapse_ranking_columns(D_0, ranking_cols, J)
  D_0$survey_weight <- weight

  ## Get weighted counts by ranking profile (fast path via Rcpp)
  tab_vec <- weighted_table_cpp(D_0[[ranking]], D_0$survey_weight)
  D_PMF_0 <- data.frame(
    ranking_value = names(tab_vec),
    n = as.numeric(tab_vec),
    stringsAsFactors = FALSE
  )
  names(D_PMF_0)[1] <- ranking

  ## Create sample space to merge
  perm_j <- permn(seq_len(J))
  perm_j <- do.call(rbind.data.frame, perm_j)
  colnames(perm_j) <- c(paste0("position_", seq_len(J)))

  perm_j <- perm_j %>%
    unite(!!as.name(ranking), sep = if (J <= 9L) "" else "|") %>%
    arrange(!!as.name(ranking))

  ## We need this because some rankings may not appear in the data
  PMF_raw <- perm_j %>%
    left_join(D_PMF_0, by = ranking) %>%
    mutate(
      n = ifelse(is.na(n), 0, n),
      prop_obs = n / sum(weight),
      prop_obs = ifelse(is.na(prop_obs), 0, prop_obs)
    ) %>%
    arrange(!!as.name(ranking))

  # Step 4: Get the bias-corrected PMF -----------------------------------------
  ## Apply Equation A.11
  imp_PMF_0 <- (PMF_raw$prop_obs - (U * (1 - p_non_random))) / p_non_random

  ## Recombine with ranking ID
  imp_PMF_1 <- perm_j %>%
    mutate(n = imp_PMF_0)

  # Step 5: Re-normalize the PMF -----------------------------------------------
  ## The previous step may produce outside-the-bound values
  ## (negative proportions)
  imp_PMF <- imp_PMF_1 %>%
    mutate(
      n_adj = ifelse(n < 0, 0, n),
      n_renormalized = n_adj / sum(n_adj)
    ) %>%
    rename(
      prop_bc_raw = n,
      prop_bc_adj = n_adj,
      prop_bc = n_renormalized
    ) %>%
    arrange(!!as.name(ranking))
  if (!is.finite(sum(imp_PMF$prop_bc)) || sum(imp_PMF$prop_bc_adj) <= 0) {
    stop("Bias-corrected PMF could not be normalized to a valid distribution.")
  }

  # Step 6: Get the bias-correction weight vector ------------------------------
  df_w <- perm_j %>%
    mutate(
      # Inverse probability weight.
      weights = imp_PMF$prop_bc / PMF_raw$prop_obs,
      weights = ifelse(weights == Inf, 0, weights),
      weights = ifelse(is.na(weights), 0, weights)
    ) %>% # NA arise from 0/0
    arrange(!!as.name(ranking))

  # Turn the results into a tibble ---------------------------------------------
  tibble_w <- df_w %>% tibble()

  # Merge the weights back to the original data --------------------------------
  data_w <- data
  data_w[[ranking]] <- .collapse_ranking_columns(data, ranking_cols, J)

  data_w <- data_w %>%
    left_join(tibble_w, by = ranking) %>%
    select(weights, everything())

  out_rankings <- PMF_raw %>%
    left_join(imp_PMF, by = ranking) %>%
    left_join(df_w, by = ranking) %>%
    dplyr::select(
      all_of(ranking), n, prop_obs, prop_bc, weights,
      everything()
    )

  raw_qoi <- .rankingq_estimate_to_summary_table(
    .rankingq_qoi_from_rankings(
      out_rankings,
      ranking_col = ranking,
      item_names = ranking_cols,
      J = J,
      prob_col = "prop_obs"
    )
  )
  ipw_qoi <- .rankingq_estimate_to_summary_table(
    .rankingq_qoi_from_rankings(
      out_rankings,
      ranking_col = ranking,
      item_names = ranking_cols,
      J = J,
      prob_col = "prop_bc"
    )
  )

  # Summarize results ==========================================================
  return(
    .rankingq_structure_output(
      list(
      est_p_random = 1 - p_non_random,
      results = data_w,
      rankings = out_rankings
      ),
      class = c("imprr_weights", "rankingQ_point_estimate"),
      method_tables = list(
        raw = raw_qoi,
        ipw = ipw_qoi
      ),
      metadata = list(
        call = match.call(expand.dots = FALSE),
        primary_method = "ipw",
        J = J,
        n_bootstrap = NULL,
        n_obs = N,
        population = population,
        assumption = assumption,
        ranking_cols = ranking_cols,
        ranking_col = ranking
      )
    )
  )
}
