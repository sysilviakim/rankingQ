#' Uniformity Test for Ranking Patterns
#'
#' @description This function implements the uniformity test for
#' ranking permutation patterns documented in Atsusaka and Kim (2024).
#'
#' @importFrom stats chisq.test
#'
#' @param data The input dataset with ranking data.
#' @param table Logical value indicating whether the data
#' is already in a table format. Defaults to FALSE.
#' @param var The variable within \code{data} to be used in the test.
#' Defaults to NULL.
#'
#' @return A chi-square test result.
#'
#' @export

uniformity_test <- function(data, table = FALSE, var = NULL) {
  ## Argument type check
  if (!is.null(var) & !is.character(var)) {
    stop("The 'var' argument must be a character string.")
  }
  if (!is.logical(table)) {
    stop("The 'table' argument must be a logical value.")
  }
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }

  ## Sanity test
  if (!is.null(var) & !var %in% names(data)) {
    stop("The variable specified is not in the data.")
  }

  if (isFALSE(table) & is.null(var)) {
    stop(
      paste0(
        "If the data is not already in a table format, ",
        "please specify the variable to be used in the test."
      )
    )
  } else if (isFALSE(table) & !is.null(var)) {
    tab <- table(data[[var]])
    tab <- permn_augment(tab)
    return(chisq.test(tab, p = rep(1 / length(tab), length(tab))))
  } else if (isTRUE(table)) {
    tab <- permn_augment(tab)
    return(chisq.test(data, p = rep(1 / length(data), length(data))))
  }
}
