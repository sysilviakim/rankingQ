#' Uniformity Test for Ranking Patterns
#'
#' @description This function implements the uniformity test for
#' ranking permutation patterns documented in Atsusaka and Kim (2025).
#'
#' @importFrom stats chisq.test
#'
#' @param data The input dataset with ranking data.
#' @param var The variable within \code{data} to be used in the test.
#' Defaults to NULL.
#'
#' @returns A chi-square test result.
#'
#' @examples
#' tab <- table(c(
#'   rep("123", 10), rep("132", 10), rep("213", 10),
#'   rep("231", 10), rep("312", 10), rep("321", 10)
#' ))
#' uniformity_test(tab)
#'
#' @export

uniformity_test <- function(data, var = NULL) {
  is_table <- "table" %in% class(data)

  ## Argument type check
  if (!is.null(var) && !is.character(var)) {
    stop("The 'var' argument must be a character string.")
  }

  if (!is_table && is.null(var)) {
    stop(
      paste0(
        "If the data is not already in a table format, ",
        "please specify the variable to be used in the test."
      )
    )
  } else if (!is_table && !is.null(var)) {
    if (!(var %in% names(data))) {
      stop("The variable specified is not in the data.")
    } else {
      tab <- table(data[[var]])
      tab <- permn_augment(tab)
      return(chisq.test(tab, p = rep(1 / length(tab), length(tab))))
    }
  } else if (is_table) {
    if (!is.null(var)) {
      warning(
        paste0(
          "The 'var' argument is ignored when 'data' is already ",
          "a table."
        )
      )
    }
    tab <- permn_augment(data)
    return(chisq.test(tab, p = rep(1 / length(tab), length(tab))))
  }
}
