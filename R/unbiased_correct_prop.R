#' Unbiased Estimator of the Proportion of Random and Non-random Responses
#'
#' This function computes the unbiased proportion of *correct* answers after
#' adjusting for the possibility that the respondent may have randomly guessed
#' the correct answer. The function is based on the formula provided by
#' Proposition 1 of Atsusaka and Kim (2024).
#'
#' @param mean_c This is the raw proportion of the correct answers.
#' @param J The number of items to rank order.
#'
#' @return A number between 0-1.
#'
#' @examples
#' unbiased_correct_prop(0.7, 3)
#'
#' @export

unbiased_correct_prop <- function(mean_c, J) {
  ## Average of correct answers
  numerator <- mean_c - (1 / factorial(J))
  denominator <- 1 - (1 / factorial(J))
  return(numerator / denominator)
}
