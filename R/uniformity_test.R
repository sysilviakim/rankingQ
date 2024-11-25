#' Uniformity Test for Ranking Patterns
#'
#' @description This function implements the uniformity test for
#' ranking permutation patterns documented in Atsusaka and Kim (2024).
#'
#' @importFrom stats chisq.test
#'
#' @param data The input dataset with ranking data.
#' @param var The variable within \code{data} to be used in the test.
#' Defaults to NULL.
#'
#' @return A chi-square test result.
#'
#' @export

uniformity_test <- function(data, var = NULL) {
  ## Argument type check
  if (!is.null(var) & !is.character(var)) {
    stop("The 'var' argument must be a character string.")
  }

  if (!("table" %in% class(data)) & is.null(var)) {
    stop(
      paste0(
        "If the data is not already in a table format, ",
        "please specify the variable to be used in the test."
      )
    )
  } else if (!("table" %in% class(data)) & !is.null(var)) {
    if (!(var %in% names(data))) {
      stop("The variable specified is not in the data.")
    } else {
      tab <- table(data[[var]])
      tab <- permn_augment(tab)
      return(chisq.test(tab, p = rep(1 / length(tab), length(tab))))
    }
  } else if ("table" %in% class(data)) {
    tab <- permn_augment(tab)
    return(chisq.test(data, p = rep(1 / length(data), length(data))))
  }
}
