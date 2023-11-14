#' Simulate the Probability that a Given Ranking Permutation Pattern is Chosen
#' Over a Random Effect Variable
#'
#' This function simulates the probability that a given ranking permutation
#' pattern is chosen over a random effect variable. The function is based on
#' the \code{sim} function from the \pkg{clarify} package as well as the
#' \code{mlogit} package, and designed to work with \code{mlogit}
#' objects.
#'
#' Warning: the `mlogit`'s `reflevel` should match the last element of
#' \code{permn}. (This should be fixed in a future version.)
#'
#' @param m An \code{mlogit} object.
#' @param permn A permutation pattern that is being checked over a combination
#' of variables.
#' @param random_var A character string that specifies the random effect
#' variable.
#' @param continuous A logical value that indicates whether the random effect
#' variable is continuous or not. The default is \code{TRUE}.
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
#' @importFrom clarify sim
#' @importFrom tibble tibble
#' @importFrom rlang `!!` set_names `:=`
#' @importFrom purrr map keep imap
#' @importFrom magrittr `%>%`
#' @importFrom stats quantile
#'
#' @export

sim_rank_randeff <- function(m,
                             permn,
                             random_var,
                             continuous = TRUE,
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

  ## Sanity check on the `random_var` argument
  if (!is.character(random_var)) {
    stop("The random variable must be a character string.")
  }
  if (!any(grepl(random_var, m$formula))) {
    stop("The random variable must be included in the model formula.")
  }

  ## `mlogit`'s `reflevel` should match the last element of `permn`
  reflevel <- m$call$reflevel
  # if (reflevel != permn[length(permn)]) {
  #   stop(
  #     "The `mlogit`'s `reflevel` should match the last element of `permn`."
  #   )
  # }

  ## Choices are already specified from the `mlogit` object
  choices <- names(m$freq)

  ## First, simulate the model parameters from an `mlogit` object
  sim_coefs <- sim(m, n = n, vcov = vcov, coefs = coefs, dist = dist)

  ## Convert the simulation values into a data frame
  sim_values <- as.data.frame(sim_coefs$sim.coefs)

  if (continuous == TRUE) {
    ## Initialize a vector to store the simulated choices
    ## over the random-effect variable
    if (is.null(range_cont)) {
      ## An arbitrary range, if not specified
      range_cont <- seq(7)
    }

    ## The probabilities that the particular permutation pattern will be chosen
    ## over the random effect variable: initialization
    p_qoi <- tibble(
      !!as.name(random_var) := range_cont,
      mean = NA,
      low = NA,
      high = NA,
      ranking = NA,
    )

    ## Must make sure that the last ranking item should be set as the base
    ## category in the `mlogit` object
    coef_by_item <- permn %>%
      set_names(., .) %>%
      map(~ names(sim_values)[grepl(.x, names(sim_values))]) %>%
      keep(~ length(.x) > 0)

    ## Loop over the range of the random effect variable
    for (i in range_cont) {
      exp_values <- coef_by_item %>%
        imap(
          ~ exp(
            sim_values[[.x[grepl("Intercept", .x)]]] +
              sim_values[[.x[grepl(random_var, .x)]]] * i
          )
        )

      ## Fill in the reference level with exp(0) values, which is 1
      exp_values[[reflevel]] <- rep(exp(0), length(exp_values[[1]]))

      ## Consecutive probabilities are multiplied together
      ## For example, if the permutation pattern is
      ## party - race - religion - gender for the four social identities ranked,
      ## this is equivalent to the following:
      ## exp_party / (exp_party + exp_race + exp_religion + exp_gender) *
      ## exp_race / (exp_race + exp_religion + exp_gender) *
      ## exp_religion / (exp_religion + exp_gender) *
      ## exp_gender / exp_gender

      p_list <- seq(length(permn)) %>%
        map(
          ~ exp_values[[permn[.x]]] /
            Reduce(`+`, exp_values[permn[.x:length(permn)]])
        )

      p <- Reduce(`*`, p_list)
      p_qoi[i, "mean"] <- mean(p)
      ## Use 2.5% and 97.5% percentiles of simulated values
      p_qoi[i, "low"] <- quantile(p, prob = 0.025)
      p_qoi[i, "high"] <- quantile(p, prob = 0.975)

      ## Record ranking pattern
      p_qoi[, "ranking"] <- paste0(permn, collapse = "_")
    }
  }

  return(p_qoi)
}
