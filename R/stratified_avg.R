#' Stratified Estimate of Average Ranks
#'
#' @description This function estimates the average ranks based on
#' stratification.
#'
#' @importFrom dplyr `%>%` group_by group_split ungroup select
#'   filter bind_rows first
#' @importFrom purrr map
#' @importFrom rlang set_names
#'
#' @param data A data frame containing the ranking data as well as the
#' stratifying variable.
#' @param var_stratum The name of the stratifying variable.
#' @param J The number of items in the ranking question. Defaults to NULL,
#' in which case it will be inferred from the data.
#' @param main_q Column name for the main ranking question to be analyzed.
#' @param anc_correct Optional indicator for passing the anchor question.
#'   If `NULL`, `p_random` is used when supplied; otherwise the function
#'   defaults to `p_random = 0` and applies no correction.
#' @param labels A vector of labels for the items being ranked.
#' Defaults to NULL.
#' @param seed Seed for \code{set.seed} for reproducibility.
#' @param weight Either a numeric vector of weights with length `nrow(data)`
#' or the name of a weight column in `data`. Defaults to `NULL`.
#' @param n_bootstrap Number of bootstraps. Defaults to 200.
#' @param ipw Indicator for using inverse probability weighting. Defaults to
#' FALSE, in which case direct bias estimation will be employed.
#' @param verbose Indicator for verbose output. Defaults to FALSE.
#' @param p_random Optional fixed proportion of random/inattentive respondents.
#'   When supplied, this overrides `anc_correct`.
#'
#' @return A data frame with the bootstrap-estimated average ranks.
#'
#' @export

stratified_avg <- function(data, var_stratum, J = NULL,
                           main_q, anc_correct = NULL, labels = NULL,
                           seed = 1234,
                           weight = NULL, n_bootstrap = 200, ipw = FALSE,
                           verbose = FALSE, p_random = NULL) {
  . <- NULL

  if (!is.numeric(n_bootstrap) || length(n_bootstrap) != 1 ||
      is.na(n_bootstrap) || n_bootstrap < 1 ||
      n_bootstrap != as.integer(n_bootstrap)) {
    stop("n_bootstrap must be a single integer >= 1.")
  }
  if (!is.logical(ipw) || length(ipw) != 1 || is.na(ipw)) {
    stop("ipw must be either TRUE or FALSE.")
  }

  ## Save and restore RNG state
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    runif(1)
  }
  old_seed <- get(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  set.seed(seed)
  seed_list <- sample.int(
    max(n_bootstrap * 10, 1e4),
    n_bootstrap,
    replace = FALSE
  )

  ## class check
  if (!is.character(var_stratum)) {
    stop("var_stratum must be a character.")
  }
  if (!is.character(main_q)) {
    stop("main_q must be a character.")
  }
  if (!is.null(anc_correct) && !is.character(anc_correct)) {
    stop("anc_correct must be a character.")
  }
  random_spec <- .resolve_random_response_inputs(data, anc_correct, p_random)
  anc_correct <- random_spec$anc_correct
  p_random <- random_spec$p_random

  if (is.null(weight)) {
    message("No weight column supplied; using equal weights for all observations.")
    weight_col <- make.unique(c(names(data), ".rankingQ_equal_weight"))
    weight_col <- weight_col[length(weight_col)]
    data[[weight_col]] <- rep(1, nrow(data))
  } else {
    if (
      is.character(weight) &&
        length(weight) == 1 &&
        weight %in% names(data)
    ) {
      weight_vec <- data[[weight]]
    } else if (is.numeric(weight) && length(weight) == nrow(data)) {
      weight_vec <- weight
    } else {
      stop(
        paste(
          "weight must be either a numeric vector of nrow(data)",
          "or a column name."
        )
      )
    }
    weight_col <- make.unique(c(names(data), ".rankingQ_weight"))
    weight_col <- weight_col[length(weight_col)]
    data[[weight_col]] <- weight_vec
  }

  if (is.null(J)) {
    J <- .infer_ranking_size(data[[main_q]])
  }
  ranking_cols <- paste0(main_q, "_", seq_len(J))
  item_spec <- data.frame(
    variable = ranking_cols,
    item = if (is.null(labels)) ranking_cols else labels,
    stringsAsFactors = FALSE
  )

  ## Initialize output ---------------------------------------------------------
  out_stratification <- vector("list", length = n_bootstrap)

  for (b in seq_len(n_bootstrap)) {
    ## Sample indices ----------------------------------------------------------
    index <- sample.int(nrow(data), size = nrow(data), replace = TRUE)

    ## This is the bootstrapped data -------------------------------------------
    boostrap_dat <- data[index, ]

    ## Estimated proportions of strata -----------------------------------------
    p_X <- prop.table(table(boostrap_dat[[var_stratum]]))

    ## Stratify by partisanship ------------------------------------------------
    list_strata <- boostrap_dat %>%
      group_by(!!as.name(var_stratum)) %>%
      group_split(.keep = TRUE) %>%
      `names<-`({
        .
      } %>% map(~ .x[[var_stratum]][1]) %>% unlist())

    ## Apply bias correction (direct) ------------------------------------------
    if (!ipw) {
      imprr_list <- list_strata %>%
        map(
          ~ {
            ## Direct bias correction
            imprr_direct(
              data = .x,
              J = J,
              main_q = main_q,
              anc_correct = anc_correct,
              p_random = p_random,
              weight = weight_col,
              n_bootstrap = 1,
              seed = seed_list[b],
              verbose = verbose
            )
          }
        )
    } else {
      imprr_list <- list_strata %>%
        map(
          ~ {
            ## IPW bias correction
            imprr_weights(
              data = .x,
              J = J,
              main_q = main_q,
              anc_correct = anc_correct,
              p_random = p_random,
              weight = weight_col
            )
          }
        )
    }

    ## Stratification estimates ------------------------------------------------
    if (!ipw) {
      est_list <- imprr_list %>%
        map("results") %>%
        map(
          ~ .x %>%
            filter(qoi == "average rank") %>%
            ungroup() %>%
            select(item, mean)
        )
    } else {
      est_list <- imprr_list %>%
        map("results") %>%
        map(
          ~ suppressMessages(avg_rank(
            .x,
            items = item_spec,
            weight = "weights",
            raw = FALSE
          )) %>%
            select(item, mean)
        )
    }

    strat <- names(p_X) %>%
      map(
        ~ est_list[[.x]]$mean * p_X[[.x]]
      ) %>%
      ## Element-by-element summation, so not unlist -> sum
      Reduce(`+`, .)

    if (!is.null(labels)) {
      out <- data.frame(
        mean = strat,
        item = labels
      )
    } else {
      out <- data.frame(
        mean = strat,
        item = first(est_list)$item
      )
    }

    ## Put into list -----------------------------------------------------------
    out_stratification[[b]] <- out
  }

  return(out_stratification %>% bind_rows())
}
