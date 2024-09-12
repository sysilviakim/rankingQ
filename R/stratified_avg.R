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
#' @param anc_correct Indicator for passing the anchor question.
#' @param labels A vector of labels for the items being ranked.
#' Defaults to NULL.
#' @param seed Seed for \code{set.seed} for reproducibility.
#' @param weight A vector of weights. Defaults to NULL.
#' @param n_bootstrap Number of bootstraps. Defaults to 200.
#' @param ipw Indicator for using inverse probability weighting. Defaults to
#' FALSE, in which case direct bias estimation will be employed.
#' @param verbose Indicator for verbose output. Defaults to FALSE.
#'
#' @return A data frame with the bootstrap-estimated average ranks.
#'
#' @export

stratified_avg <- function(data, var_stratum, J = NULL,
                           main_q, anc_correct, labels = NULL, seed = 1234,
                           weight = NULL, n_bootstrap = 200, ipw = FALSE,
                           verbose = FALSE) {
  . <- NULL
  set.seed(seed)
  seed_list <-
    sample(1:max(n_bootstrap * 10, 1e4), n_bootstrap, replace = FALSE)

  ## class check
  if (!is.character(var_stratum)) {
    stop("var_stratum must be a character.")
  }
  if (!is.character(main_q)) {
    stop("main_q must be a character.")
  }
  if (!is.character(anc_correct)) {
    stop("main_q must be a character.")
  }

  if (is.null(J)) {
    J <- nchar(data[[main_q]][[1]])
  }

  ## Initialize output ---------------------------------------------------------
  out_stratification <- vector("list", length = n_bootstrap)

  for (b in 1:n_bootstrap) {
    ## Sample indices ----------------------------------------------------------
    index <- sample(1:nrow(data), size = nrow(data), replace = TRUE)

    ## This is the bootstrapped data -------------------------------------------
    boostrap_dat <- data[index, ]

    ## Estimated proportions of strata -----------------------------------------
    p_X <- prop.table(table(boostrap_dat[[var_stratum]]))

    ## Stratify by partisanship ------------------------------------------------
    list_strata <- boostrap_dat %>%
      group_by(!!as.name(var_stratum)) %>%
      group_split(.keep = TRUE) %>%
      `names<-`({.} %>% map(~ .x[[var_stratum]][1]) %>% unlist())

    ## Apply bias correction (direct) ------------------------------------------
    if (ipw == FALSE) {
      imprr_list <- list_strata %>%
        map(
          ~ {
            ## First, the weights in vector format
            if (is.null(weight)) {
              weights <- NULL
            } else {
              weights <- .x[["weight"]]
            }

            ## Direct bias correction
            imprr_direct(
              data = .x,
              J = J,
              main_q = main_q,
              anc_correct = anc_correct,
              weight = weights,
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
            ## First, the weights in vector format
            if (is.null(weight)) {
              weights <- NULL
            } else {
              weights <- .x[["weight"]]
            }

            ## IPW bias correction
            imprr_weights(
              data = .x,
              J = J,
              main_q = main_q,
              anc_correct = anc_correct,
              weight = weights,
              n_bootstrap = 1,
              seed = seed_list[b]
            )
          }
        )
    }

    ## Stratification estimates ------------------------------------------------
    est_list <- imprr_list %>%
      map("qoi") %>%
      map(
        ~ .x %>%
          filter(qoi == "average rank") %>%
          ungroup() %>%
          select(item, mean)
      )

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
