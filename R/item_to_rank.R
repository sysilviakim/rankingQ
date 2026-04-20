#' Return Rankings with Items as Columns
#'
#' This function takes a ranking dataset with rankings as columns and
#' returns a dataset with items as columns and rankings as cell values.
#' This function is useful for converting rankings to a format that allows for
#' average ranking calculations.
#'
#' @param item_rank A data frame with rankings as columns,
#' with items being ranked as cell values.
#' @param format_input Character string indicating the format of the
#'   data input,
#' namely "ordering" or "ranking".
#' The function returns the corresponding inverse representation.
#' @param reference A character vector of item names to be used for renaming
#' the columns. If not specified, will use the first 26 letters of the
#' alphabet.
#' Default is `NULL`.
#' @param long Whether to return the output in a long data format.
#' Default is `FALSE`.
#'
#' @returns A data frame with items that are being ranked as columns,
#' with rankings in cell values.
#'
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' true_pref <- data.frame(
#'   first = c("b", "a", "c"),
#'   second = c("c", "b", "b"),
#'   third = c("a", "c", "a")
#' )
#' item_to_rank(true_pref)
#' item_to_rank(true_pref, long = TRUE)
#'
#' @export

item_to_rank <- function(item_rank,
                         format_input = "ordering",
                         reference = NULL,
                         long = FALSE) {
  if (!is.data.frame(item_rank)) {
    stop("item_rank must be a data frame.")
  }
  if (!is.character(format_input) || length(format_input) != 1 || is.na(format_input)) {
    stop("format_input must be either 'ordering' or 'ranking'.")
  }
  if (!(format_input %in% c("ordering", "ranking"))) {
    stop("format_input must be either 'ordering' or 'ranking'.")
  }
  format_input <- match.arg(format_input, c("ordering", "ranking"))
  if (!is.logical(long) || length(long) != 1 || is.na(long)) {
    stop("The 'long' argument must be either TRUE or FALSE.")
  }

  if (!is.null(reference)) {
    if (!is.character(reference)) {
      stop("reference must be a character vector.")
    }
    if (length(reference) < ncol(item_rank)) {
      stop(
        "reference must have at least as many labels as there are ranked items."
      )
    }
    item_crosswalk <- data.frame(
      name = reference,
      item_no = paste0("Item_", seq_len(length(reference)))
    )
  } else {
    default_names <- unique_alphabets(ncol(item_rank))
    item_crosswalk <- data.frame(
      name = default_names,
      item_no = paste0("Item_", seq_len(length(default_names)))
    )
  }

  item_rank_mat <- .item_to_rank_matrix(
    item_rank = item_rank,
    format_input = format_input,
    labels = item_crosswalk$name[seq_len(ncol(item_rank))]
  )
  out <- as.data.frame(item_rank_mat)

  ## Using item_crosswalk, perform renaming
  colnames(out) <- item_crosswalk$name[seq_len(ncol(out))]

  ## If output_long is specified, return the long format
  ## Use pivot_longer
  if (isTRUE(long)) {
    out <- pivot_longer(
      out, cols = everything(), names_to = "item", values_to = "rank"
    )
  }

  return(out)
}

.item_to_rank_matrix <- function(item_rank, format_input, labels) {
  mat <- as.matrix(item_rank)
  J <- ncol(mat)

  if (format_input == "ordering") {
    if (is.numeric(mat)) {
      input <- matrix(as.integer(mat), nrow = nrow(mat), ncol = J)
    } else {
      input_chr <- matrix(as.character(mat), nrow = nrow(mat), ncol = J)
      input <- matrix(
        match(input_chr, labels),
        nrow = nrow(input_chr),
        ncol = J
      )
    }
  } else {
    input <- matrix(as.integer(mat), nrow = nrow(mat), ncol = J)
  }

  if (anyNA(input)) {
    if (format_input == "ordering") {
      stop("Ordering input must contain only ranked item labels or indices.")
    }
    stop("Ranking input must contain only integer ranks.")
  }

  out <- matrix(NA_integer_, nrow = nrow(input), ncol = J)

  for (i in seq_len(nrow(input))) {
    vals <- input[i, ]
    if (!setequal(vals, seq_len(J)) || anyDuplicated(vals)) {
      stop(
        "Each row of item_rank must be a complete permutation of the ranked items."
      )
    }
    out[i, vals] <- seq_len(J)
  }

  out
}
