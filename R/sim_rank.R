#' Simulate the Probability that a Given Ranking Permutation Pattern is Chosen
#' Over a Random Effect Variable
#'
#' This function simulates the probability that a given ranking permutation
#' pattern is chosen over a random effect variable. The function is based on
#' the \code{sim} function from the \pkg{clarify} package as well as the
#' \code{mlogit} package, and designed to work with \code{mlogit}
#' objects.
#'
#' @param m An \code{mlogit} object.
#' @param permn A permutation pattern that is being checked over a combination
#' of variables.
#' @param random_var A character string that specifies the random effect
#' variable. Default is \code{NULL}.
#' @param newdata A data frame that contains the new data to be used for
#' prediction. Default is \code{NULL}. You cannot specify both \code{random_var}
#' and \code{newdata} and must pick one.
#' @param range_cont A vector that specifies the range of the random effect
#' variable to be visualized/simulated.
#' @param conf_level A numeric value that specifies the confidence level of the
#' confidence interval. The default is \code{0.95}.
#' @param seed An integer that specifies the seed for the random number.
#' Default is \code{NULL}.
#' @param n An integer that specifies the number of simulations.
#' Defaults to 1,000.
#' @param vcov Argument for \code{clarify::sim}. The default is \code{NULL}.
#' @param coefs Argument for \code{clarify::sim}. The default is \code{NULL}.
#' @param dist Argument for \code{clarify::sim}. The default is \code{NULL}.
#'
#' @return A data frame that contains the mean, lower bound, and upper bound of
#' the probability that a given ranking permutation pattern is chosen over a
#' random effect variable's values.
#'
#' @import mlogit
#' @import dfidx
#' @importFrom clarify sim
#' @importFrom tibble tibble enframe
#' @importFrom rlang `!!` set_names `:=`
#' @importFrom purrr map keep imap map_dfr imap_dfr map_dbl
#' @importFrom dplyr select bind_cols
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of
#' @importFrom magrittr `%>%`
#' @importFrom stats quantile
#'
#' @examples
#'
#' ## This model does not mean *anything.*
#' ## This is simply to demonstrate the function.
#' library(mlogit)
#' library(dfidx)
#'
#' data("Fishing", package = "mlogit")
#' Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
#' m1 <- mlogit(mode ~ 1 | price, reflevel = "beach", data = Fish)
#'
#' sim_rank_randeff(
#'   m = m1,
#'   permn = c("charter", "boat", "pier", "beach"),
#'   random_var = "price",
#'   range_cont = c(10, 50, 100),
#'   seed = 123
#' )
#'
#' @export

sim_rank <- function(m,
                     permn,
                     random_var = NULL,
                     newdata = NULL,
                     range_cont = NULL,
                     conf_level = 0.95,
                     seed = NULL,
                     n = 1000,
                     vcov = NULL,
                     coefs = NULL,
                     dist = NULL) {
  ## Suppress "no visible binding for global variable" warnings
  . <- NULL

  ## Set the seed if specified
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ## Check that the model is an `mlogit` object
  if (!inherits(m, "mlogit")) {
    stop("The model must be an `mlogit` object.")
  }

  ## Must have either `random_var` or `newdata` as non-null
  ## But both cannot be non-null
  if (is.null(random_var) & is.null(newdata)) {
    stop("Either one of `random_var` or `newdata` must be specified.")
  }
  if (!is.null(random_var) & !is.null(newdata)) {
    stop("Only one of `random_var` or `newdata` can be specified.")
  }

  ## Sanity check on the `random_var` argument
  if (!is.null(random_var)) {
    ## Sanity check on the `random_var` argument
    if (!is.character(random_var)) {
      stop("The random variable must be a character string.")
    }
    if (!any(grepl(random_var, m$formula))) {
      stop("The random variable must be included in the model formula.")
    }
    if (is.null(range_cont)) {
      ## An arbitrary range, if not specified
      range_cont <- seq(7)
    }
  }

  ## Sanity check on the `newdata` argument
  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) {
      stop("`newdata` must be a data frame.")
    }
  }

  ## First, simulate the model parameters from an `mlogit` object
  sim_coefs <- sim(m, n = n, vcov = vcov, coefs = coefs, dist = dist)

  ## Convert the simulation values into a data frame
  sim_values <- as.data.frame(sim_coefs$sim.coefs)

  ## The probabilities that the particular permutation pattern will be chosen
  ## over the random effect variable: initialization
  p_qoi <- tibble()

  ## Loop over the range of the random effect variable
  if (!is.null(random_var)) {
    pred_pattern <- pred_values <- vector("list", length(range_cont))
    names(pred_pattern) <- names(pred_values) <-
      paste0(random_var, "_", range_cont)
  }

  FUN <- function() {
    ## Predict in terms of probabilities
    pred_out <- seq(nrow(sim_coefs$sim.coefs)) %>%
      set_names(., .) %>%
      imap_dfr(
        ~ {
          m_copy <- m
          ## marginaleffects::set_coef does not work with `mlogit` objects
          ## so a workaround; perhaps glmnet will serve better?
          m_copy$coefficients <- sim_coefs$sim.coefs[.x, ]
          out <- predict(m_copy, newdata = newdata)
          if (is.matrix(out)) {
            out <- as.data.frame(out)
          } else {
            out <- enframe(out) %>% pivot_wider()
          }
          out$row <- seq(nrow(out))
          return(out)
        },
        .id = "sim"
      ) %>%
      ## This is so that it is aligned with the order provided with `permn`
      select(row, sim, all_of(permn))
    rownames(pred_out) <- NULL

    ## Using these probabilities, generate the ranking pattern of choice
    p <- seq(nrow(sim_coefs$sim.coefs)) %>%
      map_dbl(
        ~ seq(length(permn)) %>%
          map(
            function(x) {
              out <- pred_out %>% select(all_of(permn))
              return(out[.x, x] / Reduce(`+`, out[.x, x:length(permn)]))
            }
          ) %>%
          Reduce(`*`, .) %>%
          unlist() %>%
          unname()
      )

    if (!is.null(random_var)) {
      ## Create a data frame and bind to the initialized one
      p_qoi <<- bind_rows(
        p_qoi,
        tibble(
          `:=`(!!as.name(random_var), i),
          mean = mean(p),
          ## Use 2.5% and 97.5% percentiles of simulated values
          low = quantile(p, prob = (1 - conf_level) / 2),
          high = quantile(p, prob = 1 - (1 - conf_level) / 2),
          ## Record the ranking pattern for clarity
          ranking = paste0(permn, collapse = "_")
        )
      )
    } else {
      p_qoi <<- bind_rows(
        p_qoi,
        tibble(
          mean = mean(p),
          low = quantile(p, prob = (1 - conf_level) / 2),
          high = quantile(p, prob = 1 - (1 - conf_level) / 2),
          ranking = paste0(permn, collapse = "_")
        )
      )
    }

    if (!is.null(random_var)) {
      pred_values[[paste0(random_var, "_", i)]] <<- pred_out
      pred_pattern[[paste0(random_var, "_", i)]] <<- p
    } else {
      pred_values <<- pred_out
      pred_pattern <<- p
    }
  }

  if (!is.null(random_var)) {
    for (i in range_cont) {
      newdata <- seq(length(permn)) %>%
        ## Must match the number of available alternatives
        map_dfr(
          ~ tibble(`:=`(!!as.name(random_var), i)) %>%
            bind_cols(., typical_data(m, random_var = random_var))
        )
      FUN()
    }
  } else {
    FUN()
  }

  return(
    list(
      summ = p_qoi, sim_coefs = sim_coefs,
      pred_values = pred_values, p = pred_pattern
    )
  )
}

typical_data <- function(m, random_var = NULL) {
  probabilities <- linpred <- NULL

  df <- m$model %>%
    select(-all_of(random_var)) %>%
    select(-idx, -probabilities, -linpred)

  ## Generate a dataset with the typical values
  typical <- df %>%
    map_dfr(
      function(x) {
        ## If the variable is a factor, then choose the most common level
        if (is.factor(x)) {
          x <- names(sort(table(x), decreasing = TRUE))[1]
        } else {
          ## Otherwise, choose the mean after deleting missing values
          x <- mean(x, na.rm = TRUE)
        }
        return(x)
      }
    ) %>%
    select(-idx) %>%
    ## Delete the outcome variable
    select(-all_of(all.vars(m$call$formula)[1]))

  return(typical)
}
