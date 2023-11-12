#' Draw Samples from the Plackett-Luce Model
#'
#' This function draws samples from the Plackett-Luce model, using
#' Algorithm 2.1, "Efficient Sampling from Plackett-Luce," in Xia (2019),
#' page 20, Section 2.2.3 Sampling from Random Utility Models.
#' The name \code{rpluce} is a convention that follows random generations of
#' numbers from statistical distributions such as \code{rnorm} or
#' \code{rmultinom}.
#'
#' Input: A parameter \eqn{\overrightarrow{\gamma} = (\gamma_1, \cdots, \gamma_m)}
#' of Plackett-Luce. \cr
#'
#' Output: A ranking \eqn{R \in \mathcal{L}(\mathcal{A})} from
#' \eqn{pi_{\overrightarrow{\gamma}} ( \cdot )} under Plackett–Luce.  \cr
#'   1: Let \eqn{R = \emptyset} and \eqn{A = \mathcal{A}}.  \cr
#'   2: for \eqn{t = 1} to \eqn{m} do  \cr
#'   3:   Choose an alternative \eqn{a_{i_t}} from \eqn{A}
#'        with probability proportional to \eqn{\gamma_{i_t}}.  \cr
#'   4:   \eqn{R \leftarrow R \succ a_{i_t}} and
#'        \eqn{A \leftarrow A \ \{ a_{i_t} \}}.  \cr
#'   5: end for \cr
#'   6: return \eqn{R}.
#'
#' @param n The total number of samples to draw.
#' @param t The number of items or alternatives to choose from.
#' @param prob A vector of choice probabilities.
#' @param choices A vector of choices to be ranked.
#' @param seed An optional seed for the random number generator.
#'
#' @importFrom stats rmultinom
#'
#' @return A data frame of rankings of t items for n assessors.
#'
#' @examples
#' rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.2), seed = 123)
#'
#' @export

rpluce <- function(n, t, prob, choices = NULL, seed = NULL) {
  ## Set the seed if specified
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ## Sanity check on the `prob` argument.
  if (!is.numeric(prob)) {
    stop("The specified probability must be a number.")
  }
  if (any(is.na(prob))) {
    stop("NA values are not allowed in the prob argument.")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("The specified probability must be between 0 and 1.")
  }
  if (sum(prob) != 1) {
    stop("The specified probability must sum to 1.")
  }
  if (length(prob) != t) {
    stop("The specified probability must be of length t.")
  }

  ## Sanity check on the `choices` argument.
  if (!is.null(choices)) {
    if (!is.character(choices)) {
      stop("The specified choices must be a character vector.")
    }
    if (length(choices) != t) {
      stop("The specified choices must be of length t.")
    }
  }

  ## Initiate the matrix where the rankings will be stored
  rank_matrix <- matrix(nrow = n, ncol = t, NA)

  ## Draw samples from the Plackett-Luce model using a loop
  ## R, A, and Gamma are all notations from Xia (2019)
  for (j in 1:n) {
    ## Initialization: storage of rankings
    R <- vector("character", t - 1)

    ## Initial choice set: vector of items to be ranked
    A <- unique_alphabets(t)

    ## Initial choice probability (used uppercase to avoid function overlap)
    Gamma <- prob

    ## Loop over the number of items - 1 within one assessor
    for (i in seq(t - 1)) {
      ## Draw from a multinomial PMF
      draw <- as.vector(rmultinom(n = 1, size = 1, prob = Gamma))

      ## Item that was drawn
      R[[i]] <- A[draw == 1]

      ## Removing the i-th choice from the choice set
      ## because this was already drawn
      A <- A[draw == 0]

      ## Removing the i-th choice probability from the choice set
      Gamma <- Gamma[draw == 0]

      ## Renormalizing the choice probabilities
      Gamma <- Gamma / sum(Gamma)

      ## This is for when the remaining prob is all 0
      ## Adding `any` because there are two elements
      if (any(is.na(Gamma))) {
        Gamma <- c(1, rep(0, (length(Gamma) - 1)))
      }
    }

    ## The last element of R should be the remaining item
    ## which is the last choice in the reference choice set
    R[[t]] <- A

    ## Store the ranking in the matrix
    rank_matrix[j, ] <- R
  }

  ## Returning the rankings of t items for n assessors, but
  ## if the choices are specified, use them;
  ## otherwise, use generic names
  out <- as.data.frame(rank_matrix)

  if (is.null(choices)) {
    colnames(out) <- ordinal_seq(t)
  } else {
    colnames(out) <- choices
  }

  return(out)
}

## To create a vector of unique items to be ranked
## Not the most prettiest function, but it works for now.
unique_alphabets <- function(length) {
  if (length > 100000) {
    stop(
      paste0(
        "Reconsider the usage of this function!",
        "Are you sure that you need to rank more than 100,000 items?"
      )
    )
  }
  if (length <= 26) {
    return(letters[1:length])
  } else {
    if (length > 1000) {
      message("Ranking more than 1,000 items.")
    }
    repeat_chars <- ceiling(length / 26)
    if (repeat_chars - 1 <= 26) {
      out <- outer(letters, c("", letters[seq(repeat_chars - 1)]), paste0)
    } else {
      repeat_again <- ceiling(repeat_chars / 26)
      out <- outer(
        as.vector(
          outer(letters, c("", letters[seq(repeat_again - 1)]), paste0)
        ),
        as.vector(
          outer(letters, c("", letters[seq(repeat_again - 1)]), paste0)
        ),
        paste0
      )
    }
    out <- sort(unique(c(letters, as.vector(out)))[1:length])
    return(out)
  }
}
