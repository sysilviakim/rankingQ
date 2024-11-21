#' Turn the Frequency Table into a Tibble or Data Frame
#'
#' This function converts a frequency table to a tibble or data frame.
#' It also creates a proportion variable as well as the frequency variable.
#' This function is useful when plotting distribution of ranking patterns;
#' see relevant vignette.
#'
#' @param tab A frequency table.
#' @param tibble A logical value indicating whether the output should be
#' a tibble or data frame. Default is \code{TRUE}.
#'
#' @return A tibble or data frame, depending on the \code{tibble} argument.
#'
#' @importFrom tibble enframe
#' @importFrom dplyr mutate
#'
#' @examples
#' tab <- lapply(combinat::permn(seq(3)), paste0, collapse = "") |>
#'   sample(30, replace = TRUE) |>
#'   unlist() |>
#'   table()
#' table_to_tibble(tab)
#'
#' @export

table_to_tibble <- function(tab, tibble = TRUE) {
  ## Suppress "no visible binding for global variable" warnings
  freq <- prop <- ranking <- NULL

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
