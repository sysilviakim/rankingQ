#' Plot the Distribution of Rankings Over the Permutation Space
#'
#' This function takes a table in which the frequencies of ranking patterns
#' are recorded and plots it over the permutation space of rankings, using
#' the \code{ggplot2} package.
#'
#' @importFrom ggplot2 ggplot geom_col scale_fill_manual scale_y_continuous
#' @importFrom ggplot2 geom_hline geom_text aes xlab ylab theme
#'
#' @param tab A table in which the frequencies of ranking patterns are
#'   recorded.
#' @param x Name of the column that contains permutation patterns.
#' @param y Name of the column that contains the plotted values
#' (for example, proportions or frequencies).
#' @param ylim The upper limit of the y-axis.
#' @param fill The color of the bars.
#' @param xlab The label of the x-axis.
#' Defaults to "Recorded Responses".
#' @param family The font family of the text.
#' @param vjust The vertical justification of the text.
#' @param size The size of the text in `geom_text`.
#' @param linetype The linetype in `geom_hline`.
#' @param h_color The color in `geom_hline`.
#' @param h_alpha The transparency in `geom_hline`.
#'
#' @returns A ggplot2 object.
#'
#' @examples
#' tab <- lapply(combinat::permn(seq(3)), paste0, collapse = "") |>
#'   sample(30, replace = TRUE) |>
#'   unlist() |>
#'   table() |>
#'   table_to_tibble()
#' plot_dist_ranking(tab, ylim = 0.5)
#'
#' @export

plot_dist_ranking <- function(tab,
                              x = "ranking",
                              y = "prop",
                              ylim = 0.315,
                              fill = "firebrick4",
                              xlab = "Recorded Responses",
                              family = NULL,
                              vjust = -0.5,
                              size = 3,
                              linetype = "dashed",
                              h_color = "black",
                              h_alpha = 0.5) {
  ## Suppress "no visible binding for global variable" warnings
  .tmp_label <- NULL

  ## The size of the reference choice set.
  J <- .infer_ranking_size(tab[[x]])
  tab[[".tmp_label"]] <- if (identical(y, "prop")) {
    paste0(round(tab[[y]] * 100, digits = 1), "%")
  } else {
    as.character(tab[[y]])
  }
  y_scale <- if (identical(y, "prop")) {
    scale_y_continuous(
      labels = function(x) sprintf("%.0f%%", x * 100),
      limits = c(0, ylim)
    )
  } else {
    scale_y_continuous(limits = c(0, ylim))
  }

  ## ggplot2
  p <- ggplot(tab, aes(x = !!as.name(x), y = !!as.name(y), fill = "1")) +
    geom_col() +
    scale_fill_manual(values = fill) +
    xlab(xlab) +
    ylab("") +
    y_scale +
    geom_hline(
      yintercept = 1 / factorial(J),
      linetype = linetype, color = h_color, alpha = h_alpha
    ) +
    geom_text(
      aes(label = .tmp_label, family = family),
      vjust = vjust,
      size = size,
      color = fill
    ) +
    theme(legend.position = "none")

  return(p)
}
