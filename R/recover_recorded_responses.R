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

recover_recorded_responses <- function(true_order, presented_order, df = NULL) {
  if (is.null(df)) {
    presented_order <- strsplit(presented_order, "")[[1]]
    true_order <- strsplit(true_order, "")[[1]]
    recovered_order <- vector("character", length = length(true_order))

    # Iterate through each character in the respondent's response string
    for (i in seq(length(presented_order))) {
      recovered_order[i] <- true_order[[as.numeric(presented_order[i])]]
    }

    # Concatenate the characters
    # to form a string representing the recovered order
    return(paste(recovered_order, collapse = ""))
  } else {
    if (!(presented_order %in% names(df))) {
      stop("Presented order variable is not in the dataframe.")
    }
    if (!(true_order %in% names(df))) {
      stop("Response order variable is not in the dataframe.")
    }

    variable_name <- gsub("_row_rnd", "_recorded", presented_order)
    presented_order <- df[[presented_order]] %>%
      map(~ strsplit(.x, "")[[1]])
    true_order <- df[[true_order]] %>%
      map(~ strsplit(.x, "")[[1]])

    recovered_order <- seq(length(true_order)) %>%
      map(
        ~ {
          out <- vector("character", length = length(true_order[[.x]]))
          if (any(is.na(true_order[[.x]]))) {
            out <- NA
            return(out)
          } else {
            for (i in seq(length(presented_order[[.x]]))) {
              out[i] <- true_order[[.x]][[as.numeric(presented_order[[.x]][i])]]
            }
            return(paste(out, collapse = ""))
          }
        }
      ) %>%
      unlist()

    df[[variable_name]] <- recovered_order
    return(df)
  }
}
