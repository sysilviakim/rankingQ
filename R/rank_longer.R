#' Turn Column(s) Recording Rankings in Wide Format into a Long Data Format
#'
#' This function takes a data frame in wide format with columns recording
#' rankings into a long data format.
#'
#' If the data frame has more than one columns specified in the \code{cols}
#' argument, they will be translated as the first, second, third, etc. items in
#' the reference choice set. For example, if the first column records 2, the
#' second column records 1, and the third column records 3, then the function
#' will interpret that this respondent prefers the second item the most,
#' then the first item, then the third item.
#'
#' If the data frame has only one column specified in the \code{cols} argument,
#' it will be parsed by character length. For example, if the column records
#' "213", then the function will interpret that this respondent prefers the
#' second item the most, then the first, and then the third item.
#'
#' Currently, this function depends on \code{tidyverse} functions.
#' Eventually, a \code{data.table} option will be added for large datasets.
#'
#' @param x A data frame in wide format with columns recording rankings.
#' @param cols A character vector of column names that record rankings.
#' If there are multiple columns, the order of the columns should be the same
#' as the order of the reference choice set. If there is a single column, the
#' order in which the numbers appear in respondent-level character response
#' should be the same as the order of the reference set.
#' @param id The column that uniquely identify the respondent.
#' @param reference If you wish to specify the reference choice set,
#' you can provide a character vector.
#'
#' @importFrom dplyr arrange left_join select
#' @importFrom tidyr pivot_longer separate
#' @importFrom magrittr `%>%`
#' @importFrom tidyselect any_of all_of
#'
#' @return A data frame in long format with columns recording rankings.
#' The first column is the id variable that has been pre-specified.
#' The second and third columns record what item is being ranked.
#' The final column records the ranking of the item.
#'
#' @examples
#' x <- data.frame(
#'   apple = c(2, 1, 3),
#'   orange = c(1, 3, 2),
#'   banana = c(3, 2, 1)
#' )
#' rank_longer(x)
#'
#' y <- data.frame(
#'   id = c("Bernie", "Yuki", "Silvia"),
#'   rank = c("123", "321", "213")
#' )
#' rank_longer(y, cols = "rank", id = "id")
#' rank_longer(
#'   y,
#'   cols = "rank", id = "id",
#'   reference = c("Money", "Power", "Respect")
#' )
#'
#' @export

rank_longer <- function(x, cols = NULL, id = NULL, reference = NULL) {
  ## Suppress "no visible binding for global variable" warnings
  . <- ranking <- item_name <- reference_no <- NULL

  ## Originally the `pivot_sim` function, expanded
  ## Sanity check on `x` argument
  if (!("data.frame" %in% class(x))) {
    stop("The x argument must be a data frame.")
  }

  ## Sanity check on `id` argument
  if (is.null(id)) {
    message("No ID column specified. Using row number.")
    x[["id"]] <- seq(nrow(x))
    id <- "id"
  }
  if (any(duplicated(x[[id]]))) {
    stop("The id argument does not uniquely identify the respondent.")
  }

  ## If NULL, select all columns except for the ID column
  if (is.null(cols)) {
    cols <- setdiff(names(x), id)
  }

  ## Sanity check on `cols` argument
  if (length(cols) < 1) {
    stop(
      "The cols argument must be a character vector of length 1 or greater."
    )
  } else if (length(cols) == 1) {
    message("One column selected. Parsing column by character length.")
    max_ranking <- max(nchar(x[[cols]]))

    if (is.null(reference)) {
      message("No reference choice set specified. Using general column names.")
    } else {
      if (max_ranking > length(reference)) {
        stop(
          paste0(
            "The max_ranking argument is greater than ",
            "the length of the reference choice set."
          )
        )
      }
    }
  } else {
    message("Multiple columns selected.")
    message("Using input column order as ranking order.")
    max_ranking <- length(cols)
  }

  ## Are there more than two rankings?
  if (max_ranking == 1) {
    stop(
      "The max_ranking argument must be greater than 1."
    )
  }

  ## Generate a crosswalk data frame for the reference choice set
  if (length(cols) == 1) {
    if (is.null(reference)) {
      ## Overwrite the reference argument with a general column name
      reference <- paste0("V", 1:max_ranking)
    }
    crosswalk_df <- data.frame(
      reference_no = 1:max_ranking,
      item_name = reference
    )
  } else {
    crosswalk_df <- data.frame(
      reference_no = 1:max_ranking,
      item_name = cols
    )
  }

  ## Pasted ranking column case
  if (length(cols) == 1) {
    ## Split the column into N = max_ranking columns
    x <- x %>%
      separate(cols, into = reference, sep = 1:max_ranking) %>%
      mutate(across(all_of(reference), as.numeric))
    cols <- reference
  }

  ## Now turn the separate ranking columns into a long format
  out <- x %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "item_name",
      values_to = "ranking"
    ) %>%
    left_join(., crosswalk_df)

  ## Arrange the output
  out <- out %>%
    select(any_of(id), reference_no, item_name, ranking) %>%
    arrange(!!as.name(id), reference_no)

  ## Return the output
  return(out)
}
