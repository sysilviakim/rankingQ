#' Add IPW Weights to the Original Data
#'
#' @description This function is a thin convenience wrapper around
#' \code{imprr_weights()} for users who want respondent-level inverse
#' probability weights attached to the original data and do not necessarily
#' need the full ranking-profile output.
#'
#' @param data The input dataset with ranking data.
#' @param J The number of items in the ranking question. Defaults to NULL,
#'   in which case it will be inferred from the data.
#' @param main_q Ranking question to be analyzed. When \code{main_q} is a
#'   single column name or unquoted symbol such as \code{app_identity}, the
#'   function looks for \code{app_identity_1}, \code{app_identity_2},
#'   \code{app_identity_3}, and so on. You may also supply \code{main_q}
#'   directly as a character vector or unquoted \code{c(...)} expression of
#'   ranking columns such as \code{c(party, gender, race, religion)}.
#' @param anc_correct Optional indicator for passing the anchor question.
#'   If \code{NULL}, \code{p_random} is used when supplied; otherwise the
#'   function defaults to \code{p_random = 0} and applies no correction.
#' @param population Choice of the target population out of
#'   non-random respondents (default) or all respondents.
#' @param assumption Choice of identifying assumption when
#'   \code{population = "all"}: \code{uniform} assumes random respondents
#'   would have uniform counterfactual preferences, while \code{contaminated}
#'   assumes their counterfactual preferences match those of non-random
#'   respondents.
#' @param weight Optional weight specification for the estimation step. This
#'   can be the name of a weight column in \code{data}, a numeric vector with
#'   one weight per row, or an unquoted column name. Defaults to \code{NULL},
#'   which uses equal weights.
#' @param weight_col Name of the respondent-level IPW weight column to add to
#'   the returned data. Defaults to \code{"ipw_weights"}.
#' @param keep_ranking Logical; if \code{TRUE}, keep the unified ranking-profile
#'   column in the returned augmented data. Defaults to \code{FALSE}.
#' @param ranking_col Name of the unified ranking-profile column used in the
#'   augmented data and ranking summary. Defaults to \code{"ranking"}.
#' @param keep_rankings Logical; if \code{TRUE}, also return the permutation-
#'   level ranking summary and estimated random-response rate. Defaults to
#'   \code{FALSE}.
#' @param p_random Optional fixed proportion of random/inattentive respondents.
#'   When supplied, this overrides \code{anc_correct} and a message is shown if
#'   both are provided.
#'
#' @returns If \code{keep_rankings = FALSE}, a data frame equal to the original
#'   data augmented with \code{weight_col}. If \code{keep_rankings = TRUE}, a
#'   list with three elements:
#'   \describe{
#'     \item{data}{The augmented original data with respondent-level IPW
#'       weights.}
#'     \item{rankings}{The permutation-level ranking summary returned by
#'       \code{imprr_weights()}.}
#'     \item{est_p_random}{The estimated proportion of random responses.}
#'   }
#'
#' @examples
#' dat_w <- add_ipw_weights(
#'   identity,
#'   main_q = "app_identity",
#'   anc_correct = "anc_correct_identity"
#' )
#' head(dat_w)
#'
#' out <- add_ipw_weights(
#'   identity,
#'   main_q = "app_identity",
#'   anc_correct = "anc_correct_identity",
#'   keep_rankings = TRUE
#' )
#' head(out$data)
#' head(out$rankings)
#'
#' @export

add_ipw_weights <- function(data,
                            J = NULL,
                            main_q,
                            anc_correct = NULL,
                            population = "non-random",
                            assumption = "contaminated",
                            weight = NULL,
                            weight_col = "ipw_weights",
                            keep_ranking = FALSE,
                            ranking_col = "ranking",
                            keep_rankings = FALSE,
                            p_random = NULL) {
  make_internal_name <- function(base, taken) {
    candidate <- base
    suffix <- 1L
    while (candidate %in% taken) {
      candidate <- paste0(base, suffix)
      suffix <- suffix + 1L
    }
    candidate
  }

  if (!("data.frame" %in% class(data))) {
    stop("data must be a data frame.")
  }
  if (!is.character(weight_col) || length(weight_col) != 1L ||
      is.na(weight_col) || !nzchar(weight_col)) {
    stop("weight_col must be a single column name.")
  }
  if (!is.logical(keep_ranking) || length(keep_ranking) != 1L ||
      is.na(keep_ranking)) {
    stop("keep_ranking must be either TRUE or FALSE.")
  }
  if (!is.character(ranking_col) || length(ranking_col) != 1L ||
      is.na(ranking_col) || !nzchar(ranking_col)) {
    stop("ranking_col must be a single column name.")
  }
  if (!is.logical(keep_rankings) || length(keep_rankings) != 1L ||
      is.na(keep_rankings)) {
    stop("keep_rankings must be either TRUE or FALSE.")
  }
  if (weight_col %in% names(data)) {
    stop("weight_col already exists in data.")
  }
  if (keep_ranking && ranking_col %in% names(data)) {
    stop("ranking_col already exists in data.")
  }

  original_names <- names(data)
  data_work <- data
  env <- parent.frame()
  main_q_expr <- substitute(main_q)
  anc_correct_expr <- if (missing(anc_correct)) quote(NULL) else substitute(anc_correct)
  weight_spec <- .resolve_weight_input(substitute(weight), env)

  original_weight_name <- NULL
  temp_weight_name <- NULL
  if ("weights" %in% names(data_work)) {
    original_weight_name <- "weights"
    temp_weight_name <- make_internal_name(".tmp_input_weights", names(data_work))
    names(data_work)[names(data_work) == original_weight_name] <- temp_weight_name

    if (is.character(weight_spec) && identical(weight_spec, original_weight_name)) {
      weight_spec <- temp_weight_name
    }
  }

  ranking_internal <- if (keep_ranking) {
    ranking_col
  } else {
    make_internal_name(".tmp_ranking", names(data_work))
  }

  out <- eval(
    substitute(
      imprr_weights(
        data = data_work,
        J = J,
        main_q = MAIN_Q,
        anc_correct = ANC_CORRECT,
        population = population,
        assumption = assumption,
        weight = WEIGHT_SPEC,
        ranking = ranking_internal,
        p_random = p_random
      ),
      list(
        MAIN_Q = main_q_expr,
        ANC_CORRECT = anc_correct_expr,
        WEIGHT_SPEC = quote(weight_spec)
      )
    ),
    envir = environment()
  )

  data_aug <- out$results
  ipw_idx <- which(names(data_aug) == "weights")[1]
  names(data_aug)[ipw_idx] <- weight_col

  if (!is.null(temp_weight_name)) {
    names(data_aug)[names(data_aug) == temp_weight_name] <- original_weight_name
  }

  if (!keep_ranking) {
    data_aug <- data_aug[, setdiff(names(data_aug), ranking_internal), drop = FALSE]
    data_aug <- data_aug[, c(original_names, weight_col), drop = FALSE]
  } else {
    data_aug <- data_aug[, c(original_names, weight_col, ranking_col), drop = FALSE]
  }

  if (!keep_rankings) {
    return(data_aug)
  }

  rankings_out <- out$rankings
  names(rankings_out)[names(rankings_out) == ranking_internal] <- ranking_col

  list(
    data = data_aug,
    rankings = rankings_out,
    est_p_random = out$est_p_random
  )
}
