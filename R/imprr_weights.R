#' Computes Bias-Correction Weights for Ranking Data
#'
#' @description This function implements the bias correction of the ranking
#' distribution using a paired anchor question, using the IPW estimator.
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
#' @param seed Seed for \code{set.seed} for reproducibility.
#' @param weight A vector of weights. Defaults to NULL.
#' @param ranking The name of the column that will store the full ranking
#' profile. Defaults to "ranking". If `main_q` exists in the data, the produced
#' column should be identical to `main_q`. However, the function defaults to
#' creating another column by combining marginal rankings, just in case.
#'
#' @return A list with three elements:
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
#' @export

imprr_weights <- function(data,
                          J = NULL,
                          main_q,
                          anc_correct,
                          population = "non-random",
                          assumption = "contaminated",
                          seed = 123456,
                          weight = NULL,
                          ranking = "ranking") {
  ## Suppress global variable warning
  count <- n <- n_adj <- n_renormalized <- prop <- w <-
    prop_obs <- weights <- prop_bc <- NULL

  if (is.null(ranking) & main_q %in% names(data)) {
    ranking <- main_q
  }

  if ("ranking" %in% names(data)) {
    message("Existing 'ranking' column will be overwritten.")
  }

  # Setup ======================================================================
  N <- nrow(data)
  normalized_args <- .normalize_population_args(population, assumption)
  population <- normalized_args$population
  assumption <- normalized_args$assumption
  if (is.null(J)) {
    J <- nchar(data[[main_q]][[1]])
  }
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
  if (!missing(seed) && !is.null(seed)) {
    warning(
      "'seed' is currently ignored because imprr_weights is deterministic."
    )
  }

  if (population == "all" && assumption == "uniform") {
    data[[anc_correct]] <- rep(1, N) # get naive estimate for theta
  }
  # Under contaminated sampling, theta for the full population equals theta_z.

  # Pre-compute factorial for efficiency
  J_factorial <- factorial(J)

  # Check the validity of the input arguments ==================================
  ## Main ranking only

  glo_app <- data[, ranking_cols, drop = FALSE]

  # Step 1: Get the proportion of random answers -------------------------------
  p_non_random <- (mean(data[[anc_correct]]) - 1 / J_factorial) /
    (1 - 1 / J_factorial)
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
  D_0[[ranking]] <- do.call(paste0, lapply(D_0[ranking_cols], as.character))
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
  perm_j <- permn(1:J)
  perm_j <- do.call(rbind.data.frame, perm_j)
  colnames(perm_j) <- c(paste0("position_", 1:J))

  perm_j <- perm_j %>%
    unite(!!as.name(ranking), sep = "") %>%
    arrange(!!as.name(ranking))

  ## We need this because some rankings may not appear in the data
  PMF_raw <- perm_j %>%
    left_join(D_PMF_0, by = ranking) %>%
    mutate(
      n = ifelse(is.na(n) == T, 0, n),
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

  # Turn the results into a tibble
  tibble_w <- df_w %>% tibble()

  # Merge the weights back to the original data
  if (!(ranking %in% names(data))) {
    data_w <- data
    data_w[[ranking]] <- do.call(
      paste0,
      lapply(data_w[ranking_cols], as.character)
    )
  } else {
    data_w <- data
  }

  data_w <- data_w %>%
    left_join(tibble_w, by = ranking) %>%
    select(weights, everything())

  out_rankings <- PMF_raw %>%
    left_join(imp_PMF, by = "ranking") %>%
    left_join(df_w, by = "ranking") %>%
    dplyr::select(
      ranking, n, prop_obs, prop_bc, weights,
      everything()
    )

  # Summarize results ----------------------------------------------------------
  return(
    list(
      est_p_random = 1 - p_non_random,
      results = data_w,
      rankings = out_rankings
    )
  )
}
