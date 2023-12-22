#' Extract Weights of Ranking Permutation Patterns
#' Using a Paired Anchor Question
#'
#' @description
#'
#' This function extracts the weights of ranking permutation patterns
#' using a paired anchor question. The anchor question is a ranking question
#' with the same set of options as the main question. The anchor question
#' is used to identify the distribution of random answers, which is then
#' used to estimate the distribution of non-random answers in the main question.
#'
#' @importFrom dplyr `%>%` mutate left_join arrange case_when
#' @importFrom tibble tibble
#' @importFrom tidyr unite
#' @importFrom combinat permn
#' @importFrom assertthat assert_that
#'
#' @param dat The input dataset with ranking data.
#' @param main_q Column name for the main ranking question to be analyzed.
#' @param anchor_q Column name for the paired anchor question.
#' @param anc_correct Indicator for passing the anchor question.
#' @param anc_correct_pattern The correct pattern to pass the anchor question
#' filter, given the reference set. Defaults to NULL.
#' If NULL, it will taken on a J-length string with a natural progression of
#' numbers, such as "123", "1234", "1234567", and so on.
#' @param main_labels Labels for ranking options in the main question.
#' @param J Number of options in the ranking question.
#' This must be equal to the length of the \code{main_labels} argument.
#' This argument ensures that there is no error in both arguments.

weight_patterns <- function(dat,
                            main_q,
                            anchor_q,
                            anc_correct,
                            anc_correct_pattern = NULL,
                            main_labels,
                            J) {
  # Suppress
  Freq <- NULL

  # Check the validity of the input arguments.
  if (!(main_q %in% names(dat))) {
    stop("The main question is not a valid column name in the given datset.")
  }
  if (!(anchor_q %in% names(dat))) {
    stop("The anchor question is not a valid column name in the given datset.")
  }
  if (!(anc_correct %in% names(dat))) {
    stop(
      paste0(
        "The indicator for passing the anchor question ",
        "is not a valid column name in the given datset."
      )
    )
  }
  if (!all(main_labels %in% names(dat))) {
    stop(
      paste0(
        "One or more of labels in the ranking options is not a valid column",
        " in the given dataset."
      )
    )
  }
  if (length(main_labels) != J) {
    stop("Length of the label vector is not equal to the J.")
  }
  if (is.null(anc_correct_pattern)) {
    anc_correct_pattern <- seq(J) %>% paste(collapse = "")
  }
  if (nchar(anc_correct_pattern) != J) {
    stop("Length of anc_correct_pattern does not match J.")
  }

  N <- nrow(dat)

  # Proportion of random answers
  Corr <- dat[[anc_correct]]
  adjust <- mean(Corr) - 1 / factorial(J)
  normalizer <- (1 - 1 / factorial(J))
  p_non_random <- adjust / normalizer

  # Distribution of random answers
  # Generate PMF of anchor rankings
  temp <- table(dat[anchor_q])
  if (length(temp) != factorial(J)) {
    message("There are permutation patterns not realized in the survey output.")
  }

  perm_j <- permn(1:J)
  perm_j <- do.call(rbind.data.frame, perm_j)
  colnames(perm_j) <- c(paste0("position_", 1:J))
  perm_j <- perm_j %>% unite(col = "match", sep = "")

  obs_anchor_freq <- as.data.frame(table(dat[anchor_q]))
  colnames(obs_anchor_freq) <- c("match", "Freq")

  f_anchor <- perm_j %>%
    left_join(obs_anchor_freq) %>%
    # Impute 0s
    mutate(Freq = ifelse(is.na(Freq), 0, Freq)) %>%
    arrange(match)

  f_anc_true <- perm_j %>%
    mutate(
      # True density for anchor
      Freq = case_when(
        match == anc_correct_pattern ~ 1,
        TRUE ~ 0
      )
    ) %>%
    arrange(match)

  # Empirical PMF of rankings in anchor
  A <- f_anchor$Freq / N
  assert_that(sum(A) == 1)
  # Estimated proportion of non-random
  B <- p_non_random
  # True PMF of rankings in anchor
  C <- f_anc_true$Freq
  assert_that(sum(C) == 1)

  f_random <- (A - (B * C)) / (1 - B)
  assert_that(sum(f_random) == 1)

  # Alternatively, asymptotics give us
  # f_random <- rep(1 / factorial(J), factorial(J))
  # sum(f_random) # This must be 1

  # Distribution of error-free rankings
  obs_main_freq <- as.data.frame(table(dat[main_q]))
  colnames(obs_main_freq) <- c("match", "Freq")
  obs_main_freq <- obs_main_freq %>% arrange(match)

  f_main <- perm_j %>%
    left_join(obs_main_freq) %>%
    mutate(Freq = ifelse(is.na(Freq), 0, Freq)) %>%
    arrange(match)

  # Empirical PMF of ranking in the main question
  D <- f_main$Freq / N
  assert_that(sum(D) == 1)
  # Estimated PMF of errors in the anchor
  E <- f_random
  assert_that(sum(E) == 1)

  f_true <- (D - ((1 - B) * E)) / B

  # Precision issue
  assert_that(dplyr::near(sum(f_true), 1))

  # Weight vector: note that this produces different outcomes from imprr
  # and suffers from negative weight currently due to N and J balance
  w <- f_true / D

  w_frame <- tibble(
    !!as.name(main_q) := perm_j$match,
    weight = w
  )

  dat_w <- dat %>%
    left_join(w_frame) %>%
    mutate(p_non_random = p_non_random)

  return(dat_w)
}
