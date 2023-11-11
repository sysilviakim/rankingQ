#' Bootstrap the Data Frames in a List
#'
#' This function takes a data frame and returns a list of data frames that are
#' bootstrapped from the original data frame.
#'
#' @param x The original data frame.
#' @param n The number of bootstrapped data frames to return.
#' @param rows The number of rows to sample from the original data frame.
#'
#' @return A list of data frames.
#'
#' @examples
#' df <- data.frame(no = seq(length(state.abb)), stabb = state.abb)
#' bootstrap_list(df)
#'
#' @export

bootstrap_list <- function(x, n = 1000, rows = 1000) {
  df_list <- vector("list", n)
  for (i in 1:n) {
    df_list[[i]] <- x[sample(nrow(x), replace = TRUE), ]
  }
  return(df_list)
}
