#' Unbiased Estimator of the Proportion of Random and Non-random Responses
#'
#' This function computes the unbiased proportion of *correct* answers after
#' adjusting for the possibility that the respondent may have randomly guessed
#' the correct answer. The function is based on the formula provided by
#' Proposition 1 of Atsusaka and Kim (2025).
#'
#' @param mean_c This is the raw proportion of the correct answers.
#' @param J The number of items to rank order.
#'
#' @returns A number between 0-1.
#'
#' @examples
#' unbiased_correct_prop(0.7, 3)
#'
#' @export

unbiased_correct_prop <- function(mean_c, J) {
  if (!is.numeric(mean_c) || length(mean_c) != 1 || is.na(mean_c) ||
      !is.finite(mean_c) || mean_c < 0 || mean_c > 1) {
    stop("mean_c must be a single finite number between 0 and 1.")
  }
  if (!is.numeric(J) || length(J) != 1 || is.na(J) || !is.finite(J) ||
      J != as.integer(J) || J < 2) {
    stop("J must be a single integer >= 2.")
  }
  J <- as.integer(J)

  ## Average of correct answers
  numerator <- mean_c - (1 / factorial(J))
  denominator <- 1 - (1 / factorial(J))
  return(numerator / denominator)
}
