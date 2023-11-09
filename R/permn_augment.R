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
#'
#' @importFrom tibble enframe deframe
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom combinat permn
#'
#' @return A table of observed permutation patterns augmented with all possible
#' permutation patterns.
#'
#' @examples
#' tab <- table(c(rep("123", 100), rep("321", 50)))
#' permn_augment(tab, J = 3)
#'
#' @export
#'
permn_augment <- function(tab, J = 4) {
  ## Suppress "no visible binding for global variable" warnings
  . <- NULL

  out <- deframe(
    enframe(tab) %>%
      bind_rows(
        .,
        data.frame(
          name = permn(seq(J)) %>%
            map(~ paste(.x, collapse = "")) %>%
            unlist() %>%
            setdiff(., names(tab)), value = as.table(0)
        )
      )
  )
  out <- out[sort(names(out))]
  return(out)
}
