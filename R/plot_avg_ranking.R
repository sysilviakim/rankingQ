#' Plot Average Rank Results
#'
#' This function takes the output from the `imprr_direct` function
#' and plots the average rank results with confidence intervals.
#' As long as the mean and the confidence intervals are provided,
#' this function will plot other quantities of interest such as
#' marginal, pairwise, top-k rankings.
#'
#' @param data The results data from the `imprr_direct` function.
#' If an external data frame, make sure that the column names
#' are the same as the output from the `imprr_direct` function.
#' @param qoi_filter The quantity of interest (QOI) to filter for.
#' Defaults to "average rank".
#' @param xlab The x-axis label. Defaults to NULL.
#' If you'd like it to be empty, specify an empty string.
#' @param ylab The y-axis label. Defaults to an empty string.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_linerange theme_bw xlab ylab
#'
#' @export
#'
plot_average_rank <- function(data,
                              qoi_filter = "average rank",
                              xlab = NULL,
                              ylab = "") {
  item <- lower <- upper <- NULL

  if (!is.null(qoi_filter)) {
    data <- data[data$qoi == qoi_filter, ]
    if (is.null(xlab)) {
      ## Simple capitalization
      s <- strsplit(qoi_filter, " ")[[1]]
      xlab <- paste(
        toupper(substring(s, 1, 1)),
        substring(s, 2), sep = "", collapse = " "
      )
    } else {
      xlab <- ""
    }
  }

  # Level/label setting should be done outside the function
  # Generate the plot
  p <- data %>%
    ggplot(aes(x = mean, y = item)) +
    geom_point() +
    geom_linerange(aes(xmin = lower, xmax = upper)) +
    theme_bw() +
    xlab(xlab) +
    ylab(ylab)

  return(p)
}
