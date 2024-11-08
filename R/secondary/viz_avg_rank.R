#' Visualize Average Rankings
#'
#' @param data The input data frame with ranking data to calculate
#' average ranks from.
#' @param order How to order the items displayed in the visualization.
#' If "est", the items will be ordered by the estimated average rank,
#' from highest to lowest (i.e., small number to large number).
#' If "fixed", the items will be ordered as specified in the data frame
#' (i.e., an ordered factor), and the padding argument will be ignored.
#' If other non-NULL value than "est" or "fixed", the items will be ordered as
#' specified in the "order."
#' If other, such as NULL, the items will be ordered by alphabetical names.
#' Defaults to "est".
#' @param J The number of items to compare. Defaults to NULL, which will
#' assume that the data frame has two groups of items to compare (using both
#' raw and debiased data), and will take half of the number of rows.
#' If J is specified, the function will assume that
#' the data frame has J items to compare.
#' @param compare A logical value indicating whether to compare the raw and
#' debiased data. Defaults to TRUE. If FALSE, it is assumed that the data only
#' contains either raw or debiased data.
#' @param varname_type The variable name that contains whether the row
#' is debiased or raw data. Defaults to "imp". This is only relevant when
#' compare is TRUE.
#' @param color_list A vector of colors to use in the visualization.
#' Defaults to c("#b0015a", "#999999"). The first color is used for the
#' debiased data, and the second color is used for the raw data.
#' @param label_pad_width The width of the padding for the labels.
#' Defaults to 2.
#' @param imp_pad_width The width of the padding for the legend labels.
#' Defaults to 15.
#' @param geom_point_size The size of the points in the visualization.
#' Defaults to 2.
#'
#' @importFrom dplyr case_when rowwise ungroup mutate rename `%>%`
#' @importFrom stringr str_pad
#' @import ggplot2
#'
#' @return A ggplot object.
#' @examples
#'
#' ## Sample output using imprr function
#' avg_df <- data.frame(
#'   name = c(
#'     "strawberry", "strawberry", "blueberry", "blueberry",
#'     "apricot", "apricot"
#'   ),
#'   est = c(1.56, 1.73, 2.01, 2.01, 2.43, 2.27),
#'   low = c(1.48, 1.68, 1.94, 1.96, 2.35, 2.22),
#'   up = c(1.63, 1.78, 2.08, 2.06, 2.51, 2.31),
#'   imp = c(
#'     "debiased data", "raw data", "debiased data",
#'     "raw data", "debiased data", "raw data"
#'   )
#' )
#'
#' viz_avg_rank(avg_df)
#' viz_avg_rank(avg_df, order = NULL)
#' viz_avg_rank(avg_df, order = c("blueberry", "apricot", "strawberry"))
#'
#' @export

viz_avg_rank <- function(data,
                         order = "est",
                         J = NULL,
                         compare = TRUE,
                         varname_type = "imp",
                         color_list = c("#b0015a", "#999999"),
                         label_pad_width = 2,
                         imp_pad_width = 15,
                         geom_point_size = 2) {
  ## Expecting an input dataset that looks like the following:
  # A tibble: 6 × 5
  #   name      est   low    up imp
  #   <chr>   <dbl> <dbl> <dbl> <chr>
  # 1 policy   1.56  1.48  1.63 debiased data
  # 2 policy   1.73  1.68  1.77 raw data
  # 3 pork     2.01  1.94  2.09 debiased data
  # 4 pork     2.01  1.96  2.05 raw data
  # 5 service  2.43  2.35  2.51 debiased data
  # 6 service  2.27  2.22  2.31 raw data

  ## Suppress error: Undefined global functions or variables
  Type <- est <- imp <- low <- up <- name <- NULL

  if (!is.null(J) & compare == FALSE) {
    stop("The `J` argument is only relevant when `compare` is TRUE.")
  }
  if (!(compare %in% c(TRUE, FALSE))) {
    stop("The `compare` argument must be either TRUE or FALSE.")
  }
  if (!is.null(J) & !is.numeric(J)) {
    stop("The `J` argument must be numeric.")
  }
  if (!is.null(order) & !is.character(order)) {
    stop("The `order` argument must be a character.")
  }
  if (!is.character(varname_type)) {
    stop("The `varname_type` argument must be a character.")
  }
  if (!is.character(color_list)) {
    stop("The `color_list` argument must be a character.")
  }

  if (compare) {
    if (is.null(J) & nrow(data) %% 2 == 0) {
      J <- nrow(data) / 2
    } else {
      stop("The number of rows is not even. Please check the input data.")
    }
    data <- data %>%
      rowwise() %>%
      mutate(
        imp = simple_cap(imp),
        imp = str_pad(imp, width = imp_pad_width, side = "right")
      ) %>%
      ungroup() %>%
      rename(Type = imp)
  } else {
    ## Only raw or debiased estimates are presented
    data <- data %>%
      mutate(name = str_pad(name, width = label_pad_width))
  }

  if (!is.null(order)) {
    if (all(order == "est")) {
      data <- data %>%
        mutate(name = str_pad(name, width = label_pad_width))
      data$name <- factor(
        data$name,
        levels = unique(data$name[order(data$est)])
      )
    } else if (all(order == "fixed")) {
      ## Leave it alone
    } else if (!is.null(order) & length(order) == J) {
      data <- data %>%
        mutate(name = str_pad(name, width = label_pad_width)) %>%
        mutate(
          name = factor(
            name,
            levels = str_pad(order, width = label_pad_width)
          )
        )
    } else {
      data <- data %>%
        mutate(name = str_pad(name, width = label_pad_width)) %>%
        mutate(
          name = factor(
            name,
            levels = str_pad(
              unique(sort(name, decreasing = FALSE)),
              width = label_pad_width
            )
          )
        )
    }
  } else {
    data <- data %>%
      mutate(name = str_pad(name, width = label_pad_width)) %>%
      mutate(
        name = factor(
          name,
          levels = str_pad(
            unique(sort(name, decreasing = FALSE)),
            width = label_pad_width
          )
        )
      )
  }

  ## If not factor, leave it alone
  if (is.factor(data$name)) {
    data$name <- factor(data$name, levels = rev(levels(data$name)))
  }
  if (compare) {
    p <- ggplot(data, aes(x = name, y = est, color = Type)) +
      geom_point(
        aes(shape = Type),
        size = geom_point_size,
        position = position_dodge(width = 0.5)
      )
  } else {
    p <- ggplot(data, aes(x = name, y = est, color = Type)) +
      geom_point(size = geom_point_size, position = position_dodge(width = 0.5))
  }

  p <- p +
    # Reorder by point estimate
    geom_linerange(
      aes(ymin = low, ymax = up),
      lwd = 1, position = position_dodge(width = 0.5)
    ) +
    scale_color_manual(values = color_list) +
    theme_bw() +
    coord_flip() +
    xlab("") +
    ylab("") +
    scale_y_continuous(limits = c(1, J), breaks = seq(J)) +
    geom_hline(yintercept = (J + 1) / 2, linetype = "dashed")

  return(
    p +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(-0.5, 0, 0, 0, unit = "cm"),
        legend.spacing.x = unit(0, "cm"),
        legend.spacing.y = unit(0, "cm"),
        plot.title = element_text(face = "bold")
      )
  )
}
