.infer_compact_permutation_size <- function(label_nchar) {
  if (!is.numeric(label_nchar) || length(label_nchar) != 1 ||
      is.na(label_nchar) || label_nchar < 1 ||
      label_nchar != as.integer(label_nchar)) {
    stop("label_nchar must be a single positive integer.")
  }

  label_nchar <- as.integer(label_nchar)
  J <- 0L
  total_nchar <- 0L

  while (total_nchar < label_nchar) {
    J <- J + 1L
    total_nchar <- total_nchar + nchar(as.character(J))
  }

  if (total_nchar != label_nchar) {
    stop(
      "Could not infer J from compact permutation labels. ",
      "Supply J explicitly and use an unambiguous encoding for multi-digit positions."
    )
  }

  J
}

.parse_compact_permutation <- function(label, J) {
  tokens <- as.character(seq_len(J))
  token_nchar <- nchar(tokens)
  label_nchar <- nchar(label)
  n_solutions <- 0L
  solution <- NULL

  recurse <- function(pos, remaining_idx, acc) {
    if (n_solutions > 1L) {
      return(invisible(NULL))
    }

    remaining_nchar <- if (length(remaining_idx) > 0) {
      sum(token_nchar[remaining_idx])
    } else {
      0L
    }
    if ((label_nchar - pos + 1L) != remaining_nchar) {
      return(invisible(NULL))
    }

    if (length(remaining_idx) == 0L) {
      if (pos == label_nchar + 1L) {
        n_solutions <<- n_solutions + 1L
        solution <<- acc
      }
      return(invisible(NULL))
    }

    for (i in seq_along(remaining_idx)) {
      idx <- remaining_idx[[i]]
      token <- tokens[[idx]]
      token_end <- pos + token_nchar[[idx]] - 1L

      if (token_end <= label_nchar &&
          substring(label, pos, token_end) == token) {
        recurse(
          token_end + 1L,
          remaining_idx[-i],
          c(acc, idx)
        )
      }
    }

    invisible(NULL)
  }

  recurse(1L, seq_len(J), integer())

  if (n_solutions == 0L) {
    stop("Could not parse compact permutation label '", label, "' for J = ", J, ".")
  }
  if (n_solutions > 1L) {
    stop(
      "Compact permutation label '", label, "' is ambiguous for J = ", J, ". ",
      "Use a delimiter-separated or zero-padded format instead."
    )
  }

  solution
}

.parse_permutation_label <- function(label, J = NULL) {
  if (!is.character(label) || length(label) != 1 || is.na(label) || !nzchar(label)) {
    stop("Each permutation label must be a non-empty character string.")
  }

  has_separator <- grepl("[^0-9]", label)

  if (has_separator) {
    token_matches <- gregexpr("[0-9]+", label, perl = TRUE)
    tokens <- regmatches(label, token_matches)[[1]]
    if (length(tokens) == 0) {
      stop("Could not parse permutation label '", label, "'.")
    }

    values <- as.integer(tokens)
    if (is.null(J)) {
      J <- length(values)
    }

    separator_matches <- gregexpr("[^0-9]+", label, perl = TRUE)
    separators <- regmatches(label, separator_matches)[[1]]
    separator <- if (length(separators) > 0 && length(unique(separators)) == 1L) {
      separators[[1]]
    } else {
      ","
    }

    return(list(
      values = values,
      J = J,
      style = "delimited",
      separator = separator
    ))
  }

  if (is.null(J)) {
    J <- .infer_compact_permutation_size(nchar(label))
  }

  J <- as.integer(J)
  width <- nchar(as.character(J))
  if (J > 9L && nchar(label) == J * width) {
    starts <- seq.int(1L, nchar(label), by = width)
    ends <- starts + width - 1L
    values <- as.integer(substring(label, starts, ends))
    if (!anyNA(values) && identical(sort(values), seq_len(J))) {
      return(list(
        values = values,
        J = J,
        style = "fixed_width",
        width = width
      ))
    }
  }

  if (J <= 9L) {
    values <- as.integer(strsplit(label, "", fixed = TRUE)[[1]])
    return(list(
      values = values,
      J = J,
      style = "compact"
    ))
  }

  list(
    values = .parse_compact_permutation(label, J),
    J = J,
    style = "compact_multi"
  )
}

.validate_permutation_values <- function(values, J, label) {
  if (length(values) != J) {
    stop("Permutation label '", label, "' must contain exactly ", J, " positions.")
  }
  if (anyNA(values)) {
    stop("Permutation label '", label, "' contains non-numeric positions.")
  }
  if (!identical(sort(values), seq_len(J))) {
    stop("Permutation label '", label, "' must be a permutation of 1:", J, ".")
  }
}

.infer_ranking_size <- function(labels) {
  labels <- as.character(labels)
  labels <- unique(labels[!is.na(labels)])

  if (length(labels) == 0L) {
    stop("No non-missing ranking labels available to infer J.")
  }

  parsed <- lapply(labels, .parse_permutation_label, J = NULL)
  implied_J <- vapply(parsed, function(x) x$J, integer(1))

  if (length(unique(implied_J)) != 1L) {
    stop("All ranking labels must imply the same J.")
  }

  J <- implied_J[[1]]
  for (i in seq_along(parsed)) {
    .validate_permutation_values(parsed[[i]]$values, J, labels[[i]])
  }

  J
}

.default_ranking_format <- function(J, separator = "|") {
  J <- as.integer(J)

  if (J <= 9L) {
    return(list(style = "compact", J = J))
  }

  list(style = "delimited", J = J, separator = separator)
}

.format_permutation_values <- function(values, format = NULL) {
  values <- suppressWarnings(as.integer(as.character(values)))
  J <- length(values)

  if (is.null(format)) {
    format <- .default_ranking_format(J)
  }

  style <- format$style

  if (style %in% c("compact_multi", "compact_multi_inferred")) {
    style <- .default_ranking_format(J)$style
  }

  if (identical(style, "fixed_width")) {
    width <- format$width
    if (is.null(width)) {
      width <- nchar(as.character(J))
    }
    return(paste0(sprintf(paste0("%0", width, "d"), values), collapse = ""))
  }

  if (identical(style, "delimited")) {
    separator <- format$separator
    if (is.null(separator) || !nzchar(separator)) {
      separator <- "|"
    }
    return(paste(values, collapse = separator))
  }

  paste0(values, collapse = "")
}

.parse_ranking_vector <- function(labels, J = NULL) {
  labels <- as.character(labels)

  if (length(labels) == 0L) {
    if (is.null(J)) {
      stop("No ranking labels supplied.")
    }
    return(list(
      values = matrix(integer(), nrow = 0L, ncol = as.integer(J)),
      J = as.integer(J)
    ))
  }

  if (is.null(J)) {
    J <- .infer_ranking_size(labels)
  }
  J <- as.integer(J)

  values <- matrix(NA_integer_, nrow = length(labels), ncol = J)

  for (i in seq_along(labels)) {
    label <- labels[[i]]
    if (is.na(label)) {
      next
    }

    parsed <- .parse_permutation_label(label, J = J)
    .validate_permutation_values(parsed$values, J, label)
    values[i, ] <- parsed$values
  }

  list(values = values, J = J)
}

.collapse_ranking_columns <- function(data, ranking_cols, J) {
  pieces <- lapply(data[ranking_cols], as.character)

  if (J <= 9L) {
    return(do.call(paste0, pieces))
  }

  do.call(paste, c(pieces, sep = "|"))
}

#' Augmenting Permutation Patterns
#'
#' In some distribution of ranking data, not all possible permutation patterns
#' may be realized due to the sample size or skewed distribution of
#' preferences.
#'
#' This function augments the given table with all possible observed
#' permutation patterns with a frequency of zero for unrealized patterns.
#' Currently, this only takes full rankings into account, as opposed to partial
#' rankings.
#'
#' @param tab A table of observed permutation patterns.
#' @param J The length of the reference choice set. Defaults to `NULL`, in
#'   which case it is inferred from the permutation labels. For `J > 9`,
#'   delimiter-separated or zero-padded labels are unambiguous. Compact labels
#'   such as `"12345678910"` are parsed when possible, and the augmented output
#'   is returned in an unambiguous canonical format.
#'
#' @importFrom tibble enframe deframe
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map
#' @importFrom combinat permn
#'
#' @returns A table of observed permutation patterns augmented with all possible
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
  if (!is.null(J) &&
      (!is.numeric(J) || length(J) != 1 || is.na(J) ||
       J < 1 || J != as.integer(J))) {
    stop("J must be a single positive integer.")
  }
  if (!is.null(J)) {
    J <- as.integer(J)
  }

  labels <- names(tab)
  if (is.null(labels) || length(labels) == 0 || anyNA(labels) || any(!nzchar(labels))) {
    stop("tab must have non-missing, non-empty names.")
  }

  parsed <- lapply(labels, .parse_permutation_label, J = J)
  if (is.null(J)) {
    J <- parsed[[1]]$J
  }
  if (!all(vapply(parsed, function(x) identical(x$J, J), logical(1)))) {
    stop("All permutation labels must imply the same J.")
  }

  parsed_values <- lapply(seq_along(parsed), function(i) {
    values <- parsed[[i]]$values
    .validate_permutation_values(values, J, labels[[i]])
    values
  })

  has_separator <- grepl("[^0-9]", labels)
  all_fixed_width <- all(vapply(parsed, function(x) identical(x$style, "fixed_width"), logical(1)))

  format_spec <- if (any(has_separator)) {
    parsed[[which(has_separator)[1]]]
  } else if (all_fixed_width) {
    parsed[[1]]
  } else if (J <= 9L) {
    .default_ranking_format(J)
  } else {
    .default_ranking_format(J)
  }

  canonical_names <- vapply(
    parsed_values,
    .format_permutation_values,
    character(1),
    format = format_spec
  )
  observed_counts <- tapply(as.numeric(tab), canonical_names, sum)

  sample_space <- vapply(
    permn(seq_len(J)),
    .format_permutation_values,
    character(1),
    format = format_spec
  )
  sample_space <- sort(sample_space)

  out <- stats::setNames(rep(0, length(sample_space)), sample_space)
  out[names(observed_counts)] <- as.numeric(observed_counts)

  out
}
