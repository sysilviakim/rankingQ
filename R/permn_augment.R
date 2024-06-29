#' Augmenting Permutation Patterns
#'
#' In some distribution of ranking data, not all possible permutation patterns
#' may be realized due to the sample size or skewed distribution of preferences.
#'
#' This function augments the given table with all possible observed
#' permutation patterns with a frequency of zero for unrealized patterns.
#'
#' @param tab A table of observed permutation patterns.
#' @param J The length of the reference choice set.
#' Defaults to NULL, in which it will count the number of characters
#' in the first input.
#'
#' @importFrom tibble enframe deframe
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map
#' @importFrom tidyselect contains
#' @importFrom combinat permn
#'
#' @return A table of observed permutation patterns augmented with all possible
#' permutation patterns.
#'
#' @examples
#' tab <- table(c(rep("123", 100), rep("321", 50)))
#' permn_augment(tab, J = 3)
#'
#' tab <- table(c("123", "321", "213", "312", "132", "231"))
#' permn_augment(tab, J = 3)
#'
#' @export

permn_augment <- function(tab, J = NULL) {
  ## Suppress "no visible binding for global variable" warnings
  . <- value <- name <- NULL

  if (is.null(J)) {
    J <- nchar(names(tab)[1])
  }

  ## If multiple instances of nchar, stop
  if (any(nchar(names(tab)) != J)) {
    stop("All names must have the same number of characters.")
  }

  ## It is necessary to augment more permutation patterns?
  temp <- permn(seq(J)) %>%
    map(~ paste(.x, collapse = "")) %>%
    unlist() %>%
    setdiff(., names(tab))

  if (length(temp) > 0) {
    out <- deframe(
      enframe(tab) %>%
        mutate(value = as.numeric(value)) %>%
        bind_rows(
          .,
          data.frame(name = temp, value = as.table(0)) %>%
            select(name, value = contains("freq"))
        )
    )
  } else {
    out <- tab
  }

  out <- out[sort(names(out))]
  return(out)
}
