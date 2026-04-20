#' Recover the Recorded Responses Given that Ranking Items were Randomized
#'
#' This function, using the order of the items that was presented to the
#' respondent as well as the true responses to the ranking question,
#' recovers the recorded responses, or the responses that the respondent
#' actually provided, ignoring the order in which the items were
#' presented.
#'
#' This is to see if behavior such as diagonalization occurred.
#' Most survey software will take the recorded response and translate it into
#' the true ranking of the respondent (observed ranking),
#' but not providing the recorded response.
#'
#' For example, given a reference choice set of three items, A, B, and C,
#' the respondent may have been presented with the items in the order
#' C, B, A, and may have responded with 3-2-1 as a recorded response.
#' The true order of the items is A, B, C, so the observed/true ranking is
#' 1-2-3. This function takes C, B, A and 1-2-3 as inputs and returns 3-2-1.
#'
#' @param presented_order A string representing the order of the items that
#' were presented to the respondent.
#' @param true_order A string representing the true ranking of the respondent
#' with respect to the reference choice set.
#' @param df The input data frame. Defaults to NULL.
#' If NULL, the function expects inputs as simple strings such as "321" or
#' "312".
#' @param reference Optional reference choice-set order. This is only needed
#' when mixing numeric position codes with item-label inputs. It can be
#' supplied as a character vector such as \code{c("A", "B", "C", "D")} or as
#' a single compact/delimited string such as \code{"ABCD"} or
#' \code{"A|B|C|D"}.
#'
#' @importFrom dplyr `%>%`
#' @importFrom purrr map
#'
#' @returns If \code{df = NULL}, a character string giving the recovered
#' recorded response. Otherwise, the original data frame augmented with a
#' column containing the recovered recorded response.
#'
#' @examples
#'
#' ## This respondent's true ranking reported is A-B-C-D.
#' ## However, the items were presented in the order B-A-D-C.
#' ## Therefore, the respondent's recorded response is 2-1-4-3.
#' recover_recorded_responses(true_order = "1234", "2143") ## Output: "2143"
#'
#' ## This respondent's true ranking is reported as D-C-B-A.
#' ## However, the items were presented in the order A-B-C-D.
#' ## Therefore, the respondent's recorded response is 4-3-2-1.
#' recover_recorded_responses(true_order = "4321", "1234") ## Output: "4321"
#'
#' ## This respondent's true ranking is reported as C-A-D-B.
#' ## However, the items were presented in the order D-C-B-A.
#' ## Therefore, the respondent's recorded response is 3-1-4-2.
#' recover_recorded_responses(true_order = "2413", "4321") ## Output: "3142"
#'
#' ## The same example using item labels directly.
#' recover_recorded_responses("CADB", "DCBA") ## Output: "3142"
#'
#' ## You can also mix numeric rankings with labeled presentation order
#' ## if the reference choice set is supplied explicitly.
#' recover_recorded_responses("2413", "D|C|B|A", reference = c("A", "B", "C", "D"))
#'
#' @export

recover_recorded_responses <- function(
    true_order,
    presented_order,
    df = NULL,
    reference = NULL) {
  parse_reference_labels <- function(reference) {
    if (is.null(reference)) {
      return(NULL)
    }

    parse_label_tokens(reference, "reference")
  }

  parse_label_tokens <- function(x, arg_name) {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    if (!is.character(x) || length(x) < 1) {
      stop(
        arg_name,
        " must be a character vector or string of item labels."
      )
    }
    if (length(x) == 1L) {
      if (is.na(x) || !nzchar(x)) {
        stop(arg_name, " must contain only non-missing item labels.")
      }
      if (grepl("|", x, fixed = TRUE)) {
        x <- strsplit(x, "|", fixed = TRUE)[[1]]
      } else if (grepl(",", x, fixed = TRUE)) {
        x <- strsplit(x, ",", fixed = TRUE)[[1]]
      } else {
        x <- strsplit(x, "", fixed = TRUE)[[1]]
      }
    }

    x <- trimws(as.character(x))
    if (anyNA(x) || any(!nzchar(x))) {
      stop(arg_name, " must contain only non-missing item labels.")
    }
    if (anyDuplicated(x)) {
      stop(arg_name, " must not contain duplicate item labels.")
    }

    x
  }

  parse_numeric_input <- function(x, arg_name, J = NULL) {
    if (is.factor(x)) {
      x <- as.character(x)
    }

    if (length(x) > 1L) {
      if (!is.numeric(x)) {
        stop(arg_name, " must contain only numeric position codes.")
      }
      values <- suppressWarnings(as.integer(x))
      if (is.null(J)) {
        J <- length(values)
      }
      .validate_permutation_values(values, as.integer(J), paste(x, collapse = ","))
      return(list(
        values = values,
        J = as.integer(J),
        format = .default_ranking_format(as.integer(J))
      ))
    }

    x_chr <- as.character(x)
    if (length(x_chr) != 1L || is.na(x_chr) || !nzchar(x_chr)) {
      stop(arg_name, " must contain only numeric position codes.")
    }

    parsed <- .parse_permutation_label(x_chr, J = J)
    .validate_permutation_values(parsed$values, parsed$J, x_chr)

    format <- parsed
    if (format$style %in% c("compact_multi", "compact_multi_inferred")) {
      format <- .default_ranking_format(parsed$J)
    }

    list(
      values = parsed$values,
      J = parsed$J,
      format = format
    )
  }

  parse_presented_positions <- function(x, J) {
    if (length(x) > 1L) {
      if (!is.numeric(x)) {
        stop("presented_order must contain only numeric position codes.")
      }
      values <- suppressWarnings(as.integer(x))
    } else {
      x_chr <- as.character(x)
      if (length(x_chr) != 1L || is.na(x_chr) || !nzchar(x_chr)) {
        stop("presented_order must contain only numeric position codes.")
      }
      if (grepl("[0-9]", x_chr) && grepl("[A-Za-z]", x_chr)) {
        stop("presented_order must contain only numeric position codes.")
      }

      parsed <- tryCatch(
        .parse_permutation_label(x_chr, J = J),
        error = function(e) NULL
      )
      if (is.null(parsed)) {
        stop("presented_order must contain only numeric position codes.")
      }
      values <- suppressWarnings(as.integer(parsed$values))
    }

    if (length(values) != J) {
      stop("presented_order and true_order must have the same length.")
    }
    if (any(is.na(values))) {
      stop("presented_order must contain only numeric position codes.")
    }
    if (any(values < 1L | values > J)) {
      stop("presented_order contains indices outside the range of true_order.")
    }
    if (!identical(sort(values), seq_len(J))) {
      stop("presented_order must be a permutation of positions in true_order.")
    }

    values
  }

  looks_numeric_like <- function(x) {
    if (length(x) > 1L) {
      return(is.numeric(x))
    }

    x_chr <- as.character(x)
    if (length(x_chr) != 1L || is.na(x_chr) || !nzchar(x_chr)) {
      return(FALSE)
    }

    grepl("[0-9]", x_chr)
  }

  validate_label_permutation <- function(tokens, reference_labels, arg_name) {
    if (length(tokens) != length(reference_labels)) {
      stop(arg_name, " and reference must have the same length.")
    }
    if (!setequal(tokens, reference_labels)) {
      stop(arg_name, " must be a permutation of the reference choice set.")
    }
  }

  reference_labels <- parse_reference_labels(reference)

  recover_one <- function(true_label, presented_label, reference_labels = NULL) {
    if (is.na(true_label) || is.na(presented_label)) {
      return(NA_character_)
    }

    true_numeric <- tryCatch(
      parse_numeric_input(true_label, "true_order"),
      error = function(e) NULL
    )
    if (is.null(true_numeric) && looks_numeric_like(true_label)) {
      true_numeric <- parse_numeric_input(true_label, "true_order")
    }
    if (is.null(true_numeric)) {
      true_tokens <- parse_label_tokens(true_label, "true_order")
      J <- length(true_tokens)
      true_ranks <- seq_len(J)
      output_format <- .default_ranking_format(J)
      true_mode <- "labels"
    } else {
      J <- true_numeric$J
      true_ranks <- true_numeric$values
      output_format <- true_numeric$format
      true_mode <- "numeric"
    }

    if (looks_numeric_like(presented_label)) {
      presented_indices <- parse_presented_positions(presented_label, J = J)
      presented_mode <- "numeric"
    } else if (
        is.character(presented_label) &&
        length(presented_label) == 1L &&
        grepl("[A-Za-z]", presented_label) &&
        grepl("[0-9]", presented_label)
    ) {
      stop("presented_order must contain only numeric position codes.")
    } else {
      presented_tokens <- parse_label_tokens(presented_label, "presented_order")
      presented_mode <- "labels"
    }

    if (true_mode == "numeric" && presented_mode == "numeric") {
      return(
        .format_permutation_values(
          true_ranks[presented_indices],
          format = output_format
        )
      )
    }

    if (is.null(reference_labels)) {
      if (true_mode == "labels" && presented_mode == "labels") {
        reference_labels <- true_tokens
      } else {
        stop(
          "reference must be supplied when mixing numeric position codes ",
          "with item-label inputs."
        )
      }
    }

    if (length(reference_labels) != J) {
      stop("reference must have the same length as true_order.")
    }

    if (true_mode == "labels") {
      validate_label_permutation(true_tokens, reference_labels, "true_order")
      true_ranks <- match(reference_labels, true_tokens)
    }

    if (presented_mode == "labels") {
      validate_label_permutation(
        presented_tokens,
        reference_labels,
        "presented_order"
      )
      presented_indices <- match(presented_tokens, reference_labels)
    }

    if (length(presented_indices) != length(true_ranks)) {
      stop("presented_order and true_order must have the same length.")
    }

    .format_permutation_values(
      true_ranks[presented_indices],
      format = output_format
    )
  }

  if (is.null(df)) {
    return(recover_one(true_order, presented_order, reference_labels))
  } else {
    if (!(presented_order %in% names(df))) {
      stop("Presented order variable is not in the dataframe.")
    }
    if (!(true_order %in% names(df))) {
      stop("Response order variable is not in the dataframe.")
    }

    variable_name <- if (grepl("_row_rnd$", presented_order)) {
      sub("_row_rnd$", "_recorded", presented_order)
    } else {
      paste0(presented_order, "_recorded")
    }
    if (variable_name %in% names(df)) {
      stop(
        "Output column '", variable_name, "' already exists in the dataframe. ",
        "Please rename or drop it before calling recover_recorded_responses()."
      )
    }
    presented_values <- df[[presented_order]]
    true_values <- df[[true_order]]

    recovered_order <- vapply(
      seq_along(true_values),
      function(i) {
        tryCatch(
          recover_one(
            true_values[[i]],
            presented_values[[i]],
            reference_labels = reference_labels
          ),
          error = function(e) {
            stop(
              sprintf("Row %d: %s", i, conditionMessage(e)),
              call. = FALSE
            )
          }
        )
      },
      FUN.VALUE = character(1)
    )

    df[[variable_name]] <- recovered_order
    return(df)
  }
}
