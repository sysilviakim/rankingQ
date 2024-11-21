#' Computes Bias-Correction Weights for Ranking Data
#'
#' @description This function implements the bias correction of the ranking
#' distribution using a paired anchor question, using the IPW estimator.
#'
#' @importFrom dplyr `%>%` mutate select group_by left_join arrange summarise count
#' @importFrom tidyselect matches
#' @importFrom combinat permn
#' @importFrom questionr wtd.table
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
#' @param seed Seed for \code{set.seed} for reproducibility.
#' @param weight A vector of weights. Defaults to NULL.
#' @param ranking The name of the column that will store the full ranking
#' profile. Defaults to "ranking". If `main_q` exists in the data, the produced
#' column should be identical to `main_q`. However, the function defaults to
#' creating another column by combining marginal rankings, just in case.
#'
#' @return A list.
#'
#' @export

imprr_weights <- function(data,
                          J = NULL,
                          main_q,
                          anc_correct,
                          seed = 123456,
                          weight = NULL,
                          ranking = "ranking") {
  ## Suppress global variable warning
  count <- n <- n_adj <- n_renormalized <- prop <- w <- NULL

  if (is.null(ranking) & main_q %in% names(data)) {
    ranking <- main_q
  }

  if ("ranking" %in% names(data)) {
    message("Existing 'ranking' column will be overwritten.")
  }

  # Setup ======================================================================
  N <- nrow(data)
  if (is.null(J)) {
    J <- nchar(data[[main_q]][[1]])
  }

  if (is.null(weight)) {
    weight <- rep(1, N)
  }

  # Check the validity of the input arguments ==================================
  ## Main ranking only

  glo_app <- data %>%
    select(matches(main_q)) %>%
    select(matches("_[[:digit:]]$"))

  # Step 1: Get the proportion of random answers -------------------------------
  p_non_random <- (mean(data[[anc_correct]]) - 1 / factorial(J)) /
    (1 - 1 / factorial(J))

  # Step 2: Get the uniform distribution ---------------------------------------
  U <- rep(1 / factorial(J), factorial(J))

  # Step 3: Get the observed PMF based on raw data -----------------------------
  ## Get raw counts of ranking profiles
  D_0 <- glo_app %>%
    unite(!!as.name(ranking), sep = "") %>%
    mutate(survey_weight = weight)

  ## Get a weighted table
  tab_vec <- wtd.table(x = D_0[[ranking]], weights = D_0$survey_weight)%>%
    tibble()

  D_PMF_0 <- D_0 %>%
    group_by(!!as.name(ranking)) %>%
    count() %>%
    ungroup()

  ## Over-write "n" with weighted results
  D_PMF_0$n <- as.numeric(tab_vec$.)

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
      prop = n / sum(weight),
      prop = ifelse(is.na(prop), 0, prop)
    ) %>%
    arrange(!!as.name(ranking))

  # Step 4: Get the bias-corrected PMF -----------------------------------------
  ## Apply Equation A.11
  imp_PMF_0 <- (PMF_raw$prop - (U * (1 - p_non_random))) / p_non_random

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
      prop.raw = n,
      prop.adj = n_adj,
      prop = n_renormalized
    ) %>%
    arrange(!!as.name(ranking))

  # Step 6: Get the bias-correction weight vector ------------------------------
  df_w <- perm_j %>%
    mutate(
      w = imp_PMF$prop / PMF_raw$prop, # Inverse probability weight
      w = ifelse(w == Inf, 0, w),
      w = ifelse(is.na(w), 0, w)
    ) %>% # NA arise from 0/0
    arrange(!!as.name(ranking))

  # Turn the results into a tibble
  tibble_w <- df_w %>% tibble()

  var_vec <- paste0(main_q, "_", 1:J)

  # Merge the weights back to the original data
  if (!(ranking %in% names(data))) {
    data_w <- data %>%
      unite(
        !!as.name(ranking),
        matches(var_vec),
        sep = "", remove = FALSE
      )
  }

  data_w <- data_w %>%
    left_join(tibble_w, by = ranking) %>%
    select(w, everything())

  # Summarize results ----------------------------------------------------------
  return(
    list(
      est_p_random = 1 - p_non_random,
      obs_pmf = PMF_raw,
      corrected_pmf = imp_PMF,
      weights = df_w,
      data = data_w
    )
  )
}
