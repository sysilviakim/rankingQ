#' Turn the Frequency Table into a Tibble or Data Frame
#'
#' This function converts a frequency table to a tibble or data frame.
#' It also creates a proportion variable as well as the frequency variable.
#'
#' @param tab A frequency table.
#' @param tibble A logical value indicating whether the output should be
#' a tibble or data frame. Defaults to TRUE.
#'
#' @return A tibble or data frame, depending on the `tibble` argument.
#'
#' @importFrom tibble enframe
#' @importFrom dplyr mutate
#'
#' @export

table_to_tibble <- function(tab, tibble = TRUE) {
  ## Suppress "no visible binding for global variable" warnings
  freq <- prop <- ranking <- NULL

  if (!is.table(tab)) {
    stop("The input must be a table.")
  }

  out <- enframe(tab, name = "ranking", value = "freq") %>%
    mutate(
      ranking = factor(ranking),
      freq = as.numeric(freq),
      prop = freq / sum(freq)
    )

  if (tibble == FALSE) {
    out <- as.data.frame(out)
  }
  return(out)
}
