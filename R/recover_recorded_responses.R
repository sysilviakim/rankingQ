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
#'
#' @importFrom dplyr `%>%`
#' @importFrom purrr map
#'
#' @return A data frame with the true ranking of the items per respondent.
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
#' @export

recover_recorded_responses <- function(
    true_order,
    presented_order,
    df = NULL) {
  recover_one <- function(true_label, presented_label) {
    if (is.na(true_label) || is.na(presented_label)) {
      return(NA_character_)
    }

    true_parsed <- .parse_permutation_label(true_label, J = NULL)
    .validate_permutation_values(
      true_parsed$values,
      true_parsed$J,
      true_label
    )

    if (grepl("[A-Za-z]", presented_label)) {
      stop("presented_order must contain only numeric position codes.")
    }

    presented_parsed <- tryCatch(
      .parse_permutation_label(
        presented_label,
        J = true_parsed$J
      ),
      error = function(e) {
        stop("presented_order must contain only numeric position codes.")
      }
    )

    indices <- suppressWarnings(as.integer(presented_parsed$values))

    if (length(indices) != true_parsed$J) {
      stop("presented_order and true_order must have the same length.")
    }
    if (any(is.na(indices))) {
      stop("presented_order must contain only numeric position codes.")
    }
    if (any(indices < 1L | indices > true_parsed$J)) {
      stop("presented_order contains indices outside the range of true_order.")
    }
    if (!identical(sort(indices), seq_len(true_parsed$J))) {
      stop("presented_order must be a permutation of positions in true_order.")
    }

    output_format <- true_parsed
    if (output_format$style %in% c("compact_multi", "compact_multi_inferred")) {
      output_format <- .default_ranking_format(true_parsed$J)
    }

    .format_permutation_values(
      true_parsed$values[indices],
      format = output_format
    )
  }

  if (is.null(df)) {
    return(recover_one(true_order, presented_order))
  } else {
    if (!(presented_order %in% names(df))) {
      stop("Presented order variable is not in the dataframe.")
    }
    if (!(true_order %in% names(df))) {
      stop("Response order variable is not in the dataframe.")
    }

    variable_name <- gsub("_row_rnd", "_recorded", presented_order)
    presented_values <- df[[presented_order]]
    true_values <- df[[true_order]]

    recovered_order <- vapply(
      seq_along(true_values),
      function(i) {
        tryCatch(
          recover_one(true_values[[i]], presented_values[[i]]),
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
