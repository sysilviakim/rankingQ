#' Generate an Ordinal Sequence from a Number
#'
#' This function generates an ordinal sequence of an arbitrary length. For
#' example, if the length is 3, the function will return the vector
#' \code{c("1st", "2nd", "3rd")}. This function is used within \code{avg_rank}
#' and such functions.
#'
#' @param length The length of the ordinal sequence to generate.
#' It should be a numeric value of length 1.
#' @return A vector of ordinal strings.
#'
#' @examples
#' ordinal_seq(11)
#'
#' @export

ordinal_seq <- function(length) {
  # Sanity check on `length` argument
  if (length(length) != 1 | !is.numeric(length)) {
    stop("`length` must be a numeric value of length 1.")
  }

  ordinal_suffix <- function(n) {
    if (n %% 100 %in% c(11, 12, 13)) {
      return("th")
    } else {
      return(c("th", "st", "nd", "rd", rep("th", 6))[(n %% 10) + 1])
    }
  }

  sequence <- seq(1, length.out = length)
  ordinal_sequence <- paste0(sequence, sapply(sequence, ordinal_suffix))

  return(ordinal_sequence)
}
