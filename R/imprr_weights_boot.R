.rankingq_qoi_from_rankings <- function(rankings_df,
                                        ranking_col,
                                        item_names,
                                        J,
                                        prob_col = "prop_bc") {
  if (!(ranking_col %in% names(rankings_df))) {
    stop("ranking_col must be contained in rankings_df.")
  }
  if (!(prob_col %in% names(rankings_df))) {
    stop("rankings_df must contain the requested probability column.")
  }

  ranking_mat <- .parse_ranking_vector(rankings_df[[ranking_col]], J = J)$values
  prob <- as.numeric(rankings_df[[prob_col]])

  if (length(prob) != nrow(ranking_mat) ||
      anyNA(prob) || any(!is.finite(prob))) {
    stop("The requested probability column must be finite and aligned.")
  }
  if (sum(prob) <= 0) {
    stop("The requested probability column must sum to a positive value.")
  }

  prob <- prob / sum(prob)
  J_1 <- J - 1L
  all_qoi_list <- vector("list", length = J)

  for (j in seq_len(J)) {
    target_item <- item_names[[j]]
    other_items <- item_names[-j]
    target_rank <- ranking_mat[, j]

    avg_rank <- sum(target_rank * prob)
    pairwise <- vapply(
      seq_along(other_items),
      FUN.VALUE = numeric(1),
      FUN = function(k) {
        sum((target_rank < ranking_mat[, -j, drop = FALSE][, k]) * prob)
      }
    )
    topk <- vapply(
      seq_len(J_1),
      FUN.VALUE = numeric(1),
      FUN = function(k) {
        sum((target_rank <= k) * prob)
      }
    )
    marginal <- vapply(
      seq_len(J),
      FUN.VALUE = numeric(1),
      FUN = function(k) {
        sum((target_rank == k) * prob)
      }
    )

    all_qoi_list[[j]] <- rbind(
      data.frame(
        item = target_item,
        qoi = "average rank",
        outcome = paste0("Avg: ", target_item),
        estimate = avg_rank,
        stringsAsFactors = FALSE
      ),
      data.frame(
        item = target_item,
        qoi = "pairwise ranking",
        outcome = paste0("v. ", other_items),
        estimate = as.numeric(pairwise),
        stringsAsFactors = FALSE
      ),
      data.frame(
        item = target_item,
        qoi = "top-k ranking",
        outcome = paste0("Top-", seq_len(J_1)),
        estimate = as.numeric(topk),
        stringsAsFactors = FALSE
      ),
      data.frame(
        item = target_item,
        qoi = "marginal ranking",
        outcome = paste0("Ranked ", seq_len(J)),
        estimate = as.numeric(marginal),
        stringsAsFactors = FALSE
      )
    )
  }

  do.call(rbind.data.frame, all_qoi_list)
}

#' Bootstrap IPW-Based Bias-Corrected Estimates for Ranking Data
#'
#' @description This function repeatedly resamples respondents, reruns
#' \code{imprr_weights()}, and summarizes downstream quantities of interest
#' such as average ranks, pairwise probabilities, top-k probabilities, and
#' marginal rank probabilities. It provides bootstrap uncertainty estimates for
#' the IPW workflow in a format parallel to \code{imprr_direct()}.
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
#' @param n_bootstrap Number of bootstrap resamples. Defaults to 200.
#' @param seed Seed for \code{set.seed} for reproducibility.
#' @param weight The name of the weight column in `data`. Defaults to `NULL`,
#' which uses equal weights. This can also be supplied as a numeric vector or
#' as an unquoted column name.
#' @param verbose Indicator for verbose output. Defaults to FALSE.
#' @param p_random Optional fixed proportion of random/inattentive respondents.
#'   When supplied, this overrides `anc_correct` and a message is shown if both
#'   are provided.
#'
#' @returns A list with two elements:
#' \describe{
#'   \item{est_p_random}{A data frame with summary statistics for the
#'     estimated proportion of random respondents, including columns
#'     \code{mean}, \code{lower}, and \code{upper} (95\% confidence interval).}
#'   \item{results}{A data frame with bootstrap summaries for the IPW-based
#'     bias-corrected quantities of interest, grouped by \code{item},
#'     \code{qoi}, and \code{outcome}, with columns \code{mean},
#'     \code{lower}, and \code{upper}.}
#' }
#'
#' @importFrom dplyr `%>%` group_by summarise
#' @importFrom stats quantile runif
#'
#' @examples
#' out <- imprr_weights_boot(
#'   identity,
#'   main_q = "app_identity",
#'   anc_correct = "anc_correct_identity",
#'   n_bootstrap = 2,
#'   seed = 123
#' )
#' out$est_p_random
#' head(out$results)
#'
#' @export
imprr_weights_boot <- function(data,
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
  est.p.random <- estimate <- item <- qoi <- outcome <- NULL

  N <- nrow(data)
  if (is.null(N) || N == 0) {
    stop("There is no data to analyze. Please check the input data.")
  }

  if (!is.numeric(n_bootstrap) || length(n_bootstrap) != 1 ||
      is.na(n_bootstrap) || n_bootstrap < 1 ||
      n_bootstrap != as.integer(n_bootstrap)) {
    stop("n_bootstrap must be a single integer >= 1.")
  }
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("verbose must be either TRUE or FALSE.")
  }

  env <- parent.frame()
  main_q_info <- .resolve_main_q_columns(data, substitute(main_q), env, J)
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

  if (is.null(weight_input)) {
    message("No weight column supplied; using equal weights for all observations.")
  }
  weight_vec <- .resolve_weight_vector(data, weight_input, N)

  ranking_name <- make.unique(c(names(data), ".rankingQ_boot_ranking"))
  ranking_name <- ranking_name[[length(ranking_name)]]

  loop_data <- data
  loop_ranking_cols <- ranking_cols
  loop_anc_correct <- random_spec$anc_correct
  if ("weights" %in% names(loop_data)) {
    input_weight_name <- make.unique(c(names(loop_data), ".rankingQ_input_weights"))
    input_weight_name <- input_weight_name[[length(input_weight_name)]]
    names(loop_data)[names(loop_data) == "weights"] <- input_weight_name
    loop_ranking_cols[loop_ranking_cols == "weights"] <- input_weight_name
    if (!is.null(loop_anc_correct) && loop_anc_correct == "weights") {
      loop_anc_correct <- input_weight_name
    }
  }

  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    runif(1)
  }
  old_seed <- get(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  set.seed(seed)

  list_prop <- vector("list", length = n_bootstrap)
  list_qoi <- vector("list", length = n_bootstrap)
  list_qoi_raw <- vector("list", length = n_bootstrap)

  for (i in seq_len(n_bootstrap)) {
    index <- sample.int(N, size = N, replace = TRUE)
    bootstrap_dat <- loop_data[index, , drop = FALSE]
    bootstrap_weight <- weight_vec[index]

    out_ipw <- imprr_weights(
      data = bootstrap_dat,
      J = J,
      main_q = loop_ranking_cols,
      anc_correct = loop_anc_correct,
      population = population,
      assumption = assumption,
      weight = bootstrap_weight,
      ranking = ranking_name,
      p_random = random_spec$p_random
    )

    list_prop[[i]] <- out_ipw$est_p_random
    list_qoi[[i]] <- .rankingq_qoi_from_rankings(
      out_ipw$rankings,
      ranking_col = ranking_name,
      item_names = loop_ranking_cols,
      J = J,
      prob_col = "prop_bc"
    )
    list_qoi_raw[[i]] <- .rankingq_qoi_from_rankings(
      out_ipw$rankings,
      ranking_col = ranking_name,
      item_names = loop_ranking_cols,
      J = J,
      prob_col = "prop_obs"
    )
  }

  if (verbose) {
    message("Bootstrapping finished.")
  }

  df_random <- data.frame(est.p.random = unlist(list_prop))
  df_random_summary <- df_random %>%
    summarise(
      mean = mean(est.p.random),
      lower = as.numeric(quantile(est.p.random, 0.025)),
      upper = as.numeric(quantile(est.p.random, 0.975))
    )

  df_qoi_summary <- do.call(rbind.data.frame, list_qoi) %>%
    group_by(item, qoi, outcome) %>%
    summarise(
      mean = mean(estimate),
      lower = as.numeric(quantile(estimate, 0.025)),
      upper = as.numeric(quantile(estimate, 0.975)),
      .groups = "drop"
    )
  df_raw_summary <- do.call(rbind.data.frame, list_qoi_raw) %>%
    group_by(item, qoi, outcome) %>%
    summarise(
      mean = mean(estimate),
      lower = as.numeric(quantile(estimate, 0.025)),
      upper = as.numeric(quantile(estimate, 0.975)),
      .groups = "drop"
    )

  .rankingq_structure_output(
    list(
      est_p_random = df_random_summary,
      results = df_qoi_summary
    ),
    class = c("imprr_weights_boot", "rankingQ_interval_estimate"),
    method_tables = list(
      raw = df_raw_summary,
      ipw = df_qoi_summary
    ),
    metadata = list(
      call = match.call(expand.dots = FALSE),
      primary_method = "ipw",
      J = J,
      n_bootstrap = n_bootstrap,
      n_obs = N,
      population = population,
      assumption = assumption,
      ranking_cols = ranking_cols
    )
  )
}
