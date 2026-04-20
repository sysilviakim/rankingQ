#' Convert Ranking Columns from Wide to Long Format
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
#' @importFrom dplyr arrange left_join select mutate across `%>%`
#' @importFrom tidyr pivot_longer pivot_wider separate_wider_position
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
    message("One column selected. Parsing encoded ranking values.")
    parsed_rankings <- .parse_ranking_vector(x[[cols]])
    max_ranking <- parsed_rankings$J

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
      reference <- paste0("V", seq_len(max_ranking))
    }
    crosswalk_df <- data.frame(
      reference_no = seq_len(max_ranking),
      item_name = reference
    )
  } else {
    crosswalk_df <- data.frame(
      reference_no = seq_len(max_ranking),
      item_name = cols
    )
  }

  ## Pasted ranking column case
  if (length(cols) == 1) {
    source_col <- cols
    rank_df <- as.data.frame(parsed_rankings$values, stringsAsFactors = FALSE)
    names(rank_df) <- reference
    x <- cbind(
      x[, setdiff(names(x), source_col), drop = FALSE],
      rank_df
    )
    cols <- reference
  }

  ## Now turn the separate ranking columns into a long format
  out <- x %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "item_name",
      values_to = "ranking"
    ) %>%
    left_join(crosswalk_df, by = "item_name")

  ## Arrange the output
  out <- out |>
    select(any_of(id), reference_no, item_name, ranking) |>
    arrange(!!as.name(id), reference_no)

  ## Return the output
  return(out)
}

#' Turn Long Ranking Data into a Wide Format
#'
#' This function takes ranking data in long format and returns a wide-format
#' data frame with one row per respondent. It can return either one column per
#' ranked item or a single pasted ranking string.
#'
#' @param x A data frame in long format with respondent identifiers, item names,
#' and ranks.
#' @param id The column that uniquely identifies the respondent.
#' @param item The column that contains item names. Defaults to
#' \code{"item_name"}.
#' @param rank The column that contains rank values. Defaults to
#' \code{"ranking"}.
#' @param output The desired output format: \code{"multiple"} for one column
#' per item or \code{"single"} for one pasted ranking string. Defaults to
#' \code{"multiple"}.
#' @param reference Optional character vector giving the reference choice-set
#' order. If omitted and \code{reference_no} is present in \code{x}, item order
#' is inferred from that column.
#' @param ranking_name The name of the output column when
#' \code{output = "single"}. Defaults to \code{"ranking"}.
#'
#' @return A data frame in wide format with one row per respondent.
#'
#' @importFrom dplyr `%>%` arrange select
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect all_of any_of
#'
#' @examples
#' x <- data.frame(
#'   id = c(1, 1, 1, 2, 2, 2),
#'   item_name = c("A", "B", "C", "A", "B", "C"),
#'   ranking = c(1, 2, 3, 3, 2, 1)
#' )
#'
#' rank_wider(x, id = "id")
#' rank_wider(
#'   x,
#'   id = "id",
#'   output = "single",
#'   reference = c("A", "B", "C")
#' )
#'
#' @export

rank_wider <- function(x,
                       id,
                       item = "item_name",
                       rank = "ranking",
                       output = c("multiple", "single"),
                       reference = NULL,
                       ranking_name = "ranking") {
  reference_no <- NULL

  if (!("data.frame" %in% class(x))) {
    stop("The x argument must be a data frame.")
  }
  if (!is.character(id) || length(id) != 1) {
    stop("The id argument must be a character vector of length 1.")
  }
  if (!is.character(item) || length(item) != 1) {
    stop("The item argument must be a character vector of length 1.")
  }
  if (!is.character(rank) || length(rank) != 1) {
    stop("The rank argument must be a character vector of length 1.")
  }
  output <- match.arg(output)

  required_cols <- c(id, item, rank)
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "The following required columns are missing from x: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  dat <- x %>%
    select(all_of(required_cols), any_of("reference_no"))

  if (any(is.na(dat[[id]]))) {
    stop("The id column cannot contain missing values.")
  }
  if (any(is.na(dat[[item]]))) {
    stop("The item column cannot contain missing values.")
  }
  if (any(is.na(dat[[rank]]))) {
    stop("The rank column cannot contain missing values.")
  }

  dat[[item]] <- as.character(dat[[item]])

  if (any(duplicated(dat[c(id, item)]))) {
    stop("Each respondent-item pair must appear at most once.")
  }
  if (any(duplicated(dat[c(id, rank)]))) {
    stop("Each respondent must have unique rank values.")
  }

  if (!is.null(reference)) {
    if (!is.character(reference)) {
      stop("The reference argument must be a character vector.")
    }
    if (any(duplicated(reference))) {
      stop("The reference argument cannot contain duplicates.")
    }
    if (!all(unique(dat[[item]]) %in% reference)) {
      stop("All observed items must be contained in the reference argument.")
    }
    item_levels <- reference
  } else if ("reference_no" %in% names(dat)) {
    ref_map <- unique(dat[c(item, "reference_no")])
    if (any(duplicated(ref_map[[item]]))) {
      stop("Each item must map to a unique reference_no.")
    }
    ref_map <- ref_map[order(ref_map[["reference_no"]]), , drop = FALSE]
    item_levels <- ref_map[[item]]
  } else {
    item_levels <- unique(dat[[item]])
  }

  item_counts <- table(dat[[id]])
  if (!all(item_counts == length(item_levels))) {
    stop("Each respondent must have a complete ranking over the same items.")
  }

  per_id_items <- split(dat[[item]], dat[[id]])
  if (!all(vapply(
    per_id_items,
    function(z) setequal(z, item_levels),
    logical(1)
  ))) {
    stop("Each respondent must have the same set of ranked items.")
  }

  dat[[item]] <- factor(dat[[item]], levels = item_levels)

  wide <- dat %>%
    arrange(!!as.name(id), !!as.name(item)) %>%
    pivot_wider(
      id_cols = all_of(id),
      names_from = all_of(item),
      values_from = all_of(rank)
    ) %>%
    as.data.frame()

  if (output == "multiple") {
    return(wide)
  }

  rank_cols <- item_levels[item_levels %in% names(wide)]
  rank_mat <- wide[rank_cols]
  rank_format <- .default_ranking_format(length(rank_cols))
  rank_strings <- apply(
    rank_mat,
    1,
    function(z) .format_permutation_values(z, format = rank_format)
  )

  out <- wide[c(id)]
  out[[ranking_name]] <- rank_strings
  return(out)
}
