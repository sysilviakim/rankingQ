#' Return Rankings with Items as Columns
#'
#' This function takes a ranking dataset with rankings as columns and
#' returns a dataset with items as columns and rankings as cell values.
#' This function is useful for converting rankings to a format that allows for
#' average ranking calculations.
#'
#' @param item_rank A data frame with rankings as columns,
#' with items being ranked as cell values.
#' @param format_input Character string indicating the format of the data input,
#' namely "ordering" or "ranking".
#' Used for \code{PLMIX::rank_ord_switch}.
#' @param reference A character vector of item names to be used for renaming
#' the columns. If not specified, will use the first 26 letters of the alphabet.
#' Default is `NULL`.
#' @param long Whether to return the output in a long data format.
#' Unless `TRUE`, will not be. Default is `NULL`.
#'
#' @return A data frame with items that are being ranked as columns,
#' with rankings in cell values.
#'
#' @importFrom PLMIX rank_ord_switch
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
                         long = NULL) {
  if (!is.null(reference)) {
    item_crosswalk <- data.frame(
      name = reference, item_no = paste0("Item_", seq(length(letters)))
    )
  } else {
    item_crosswalk <- data.frame(
      name = letters, item_no = paste0("Item_", seq(length(letters)))
    )
  }

  out <- PLMIX::rank_ord_switch(
    data = item_rank, format_input = format_input
  )
  out <- as.data.frame(out)

  ## Using item_crosswalk, perform renaming
  colnames(out) <- item_crosswalk$name[1:ncol(out)]

  ## If output_long is specified, return the long format
  ## Use pivot_longer
  if (!is.null(long) & isTRUE(long)) {
    out <- pivot_longer(
      out, cols = everything(), names_to = "item", values_to = "rank"
    )
  }

  return(out)
}
