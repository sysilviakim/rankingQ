#' Bootstrap the Data Frames in a List
#'
#' This function takes a data frame and returns a list of data frames that are
#' bootstrapped from the original data frame.
#'
#' @param x The original data frame.
#' @param n The number of bootstrapped data frames to return.
#' @param rows The number of rows to sample from the original data frame.
#' @param dfidx If TRUE, use the dfidx::dfidx function to transform into mlogit
#' data format, i.e., a dataframe with indexes. Default is FALSE.
#' @param ... Additional arguments to pass to dfidx::dfidx.
#'
#' @return A list of data frames.
#'
#' @examples
#' df <- data.frame(no = seq(length(state.abb)), stabb = state.abb)
#' bootstrap_list(df)
#'
#' @export

bootstrap_list <- function(x, n = 1000, rows = 1000,
                           dfidx = FALSE, ...) {
  df_list <- vector("list", n)
  for (i in 1:n) {
    df_list[[i]] <- x[sample(nrow(x), replace = TRUE), ]
    ## If dfidx is TRUE,
    ## use the dfidx::dfidx function to transform into mlogit format.
    if (dfidx == TRUE) {
      df_list[[i]] <- dfidx::dfidx(df_list[[i]], ...)
    }
  }
  return(df_list)
}
