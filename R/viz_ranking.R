#' Visualize Quantities of Interest Using Ranking Data
#'
#' @description \code{vis_ranking} visualizes four quantities of interest from
#' ranking data: average rankings, pair rankings over a single target item over
#' other items, top-k ranking of the target item, and the marginal ranking of
#' the target item.
#'
#' @importFrom generics tidy
#' @importFrom dplyr mutate
#' @importFrom dplyr `%>%`
#' @importFrom dplyr rowwise
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @importFrom stringr str_pad
#'
#' @importFrom estimatr lm_robust
#' @importFrom ggpubr ggarrange
#' @import ggplot2
#'
#' @param dat The input dataset with ranking data.
#' @param target_item A string for the target item's variable name
#' @param other_items A set of strings for variable names corresponding to
#' other items that were available as ranking options
#' @param treat The treatment indicator variable.
#' Defaults to NULL.
#' @param single_plot If TRUE, returns a single plot.
#' If FALSE, returns a list of plots that will compose the combined plot.
#' Defaults to TRUE.
#' @param color_palette The color palette to be used.
#' @param weight The weight vector to be used.
#' @param font The font to be used. Defaults to NULL.
#'
#' @return A ggplot that visualizes all treatment effects.
#' If single_plot is TRUE, it will return a single ggplot.
#' If FALSE, it will return four ggplot objects in a list:
#' average ranks, pairwise ranks, top-k ranks, and marginal ranks.
#'
#' @export

viz_ranking <- function(dat,
                        target_item,
                        other_items,
                        treat = NULL,
                        single_plot = TRUE,
                        weight = rep(1, dim(dat)[1]),
                        color_palette = c(
                          "black",
                          "#b0015a",
                          "#128ba0",
                          "gray"
                        ),
                        font = NULL) {
  ## Suppress global variable visible binding error
  . <- outcome <- desc <- estimate <- target <-
    conf.low <- conf.high <- term <- weight <- NULL

  # Avoiding overwriting utils::data and stats::dt
  # Avoiding reliance on tidyverse as much as possible

  # Check the validity of the input arguments.
  if (!(target_item %in% names(dat))) {
    stop("The target item is not a valid column name in the given datset.")
  }
  if (!all(other_items %in% names(dat))) {
    stop(
      paste0(
        "One or more of other items is not a valid column name",
        " in the given dataset."
      )
    )
  }
  if (!is.null(treat)) {
    if (!(treat %in% names(dat))) {
      stop("Treatment variable is not present in the given dataset.")
    }
  }
  if (length(target_item) != 1) {
    stop("There is no or more than one target variable.")
  }

  # Set names to subject to purrr::map
  other_items <- set_names(other_items, nm = other_items)

  # Highlight and benchmark color for average rankings and such
  use_col <- c(color_palette[2], color_palette[1])

  # Create a label for items: if there are underbars, turn to spaces
  # Capitalize the first letters
  label <- simple_cap(gsub("_", " ", target_item))

  # Size of the dataset
  N <- nrow(dat)
  J <- length(other_items) + 1

  # Treatment indicator
  if (!is.null(treat)) {
    D <- dat[[treat]]
  }

  # Process the raw ranking data +  store the original data in a separate object
  dat_raw <- dat

  # Prepare a vector and a list to extract quantities of interest in lists
  Y_rank_target <- dat[[target_item]]
  Y_rank_others <- other_items %>% map(~ dat[[.x]])

  # pair ranking probability compared to others?
  Y_pair <- other_items %>%
    map(~ ifelse(Y_rank_target < Y_rank_others[[.x]], 1, 0))

  # Is the item in top-k?
  Y_topk <- seq(J) %>%
    map(~ ifelse(Y_rank_target <= .x, 1, 0))

  # Is the item's rank exactly k?
  Y_marg <- seq(J) %>%
    map(~ ifelse(Y_rank_target == .x, 1, 0))

  label_capitalize <- function(x) {
    x %>%
      rowwise() %>%
      mutate(outcome = simple_cap(gsub("_", " ", outcome))) %>%
      ungroup()
  }

  rev_outcome <- function(x) {
    x %>%
      mutate(outcome = factor(outcome, levels = rev(unique(outcome)))) %>%
      arrange(outcome)
  }

  # Collect estimated means: without treatment
  if (is.null(treat)) {
    # Estimate baseline outcome values via OLS
    m_rank_target <- lm_robust(Y_rank_target ~ 1, weights = weight) %>% tidy()

    m_rank_others <- other_items %>%
      map(~ lm_robust(Y_rank_others[[.x]] ~ 1, weights = weight) %>% tidy())

    m_topk <- seq(J) %>%
      map(~ lm_robust(Y_topk[[.x]] ~ 1, weights = weight) %>% tidy())

    m_pair <- other_items %>%
      imap(
        ~ lm_robust(Y_pair[[.x]] ~ 1, weights = weight) %>%
          tidy() %>%
          mutate(outcome = .y)
      )

    m_marg <- seq(J) %>%
      map(~ lm_robust(Y_marg[[.x]] ~ 1, weights = weight) %>% tidy())

    m_rank_catch <- do.call(rbind.data.frame, m_rank_others) %>%
      mutate(
        outcome = paste0(other_items),
        target = "B"
      )

    m_rank <- m_rank_target %>%
      mutate(
        outcome = paste0(target_item),
        target = "A"
      )

    gg_avg <- rbind(m_rank, m_rank_catch) %>%
      label_capitalize() %>%
      unify_label_length()

    gg_pair <- do.call(rbind.data.frame, m_pair) %>%
      label_capitalize() %>%
      unify_label_length()

    gg_marg <- do.call(rbind.data.frame, m_marg) %>%
      mutate(outcome = paste("Ranked", seq(J))) %>%
      rev_outcome() %>%
      unify_label_length()

    gg_topk <- do.call(rbind.data.frame, m_topk) %>%
      mutate(outcome = paste0("Top-", seq(J))) %>%
      .[seq(J - 1), ] %>%
      rev_outcome() %>%
      unify_label_length()

    # Visualize all effects
    gg_avg$outcome <- factor(
      gg_avg$outcome,
      levels = gg_avg$outcome[order(-gg_avg$estimate)]
    )
    p_avg <- ggplot(gg_avg, aes(outcome, y = estimate)) +
      geom_point(aes(color = target), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = target),
        linewidth = 1
      )
    p_avg <- vis_helper(p_avg, "avg", J, use_col, label, treat)

    gg_pair$outcome <- factor(
      gg_pair$outcome,
      levels = gg_pair$outcome[order(-gg_pair$estimate)]
    )
    p_pair <- ggplot(gg_pair, aes(outcome, y = estimate)) +
      geom_point(size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high),
        linewidth = 1
      )
    p_pair <- vis_helper(p_pair, "pair", J, use_col, label, treat)

    gg_topk$outcome <- factor(
      gg_topk$outcome,
      levels = sort(unique(gg_topk$outcome))
    )
    p_topk <- ggplot(gg_topk, aes(outcome, y = estimate)) +
      geom_point(size = 2) +
      geom_linerange(
        aes(ymin = conf.low, ymax = conf.high),
        linewidth = 1
      )
    p_topk <- vis_helper(p_topk, "topk", J, use_col, label, treat)

    gg_marg$outcome <- factor(
      gg_marg$outcome,
      sort(unique(gg_marg$outcome))
    )
    p_marg <- ggplot(gg_marg, aes(outcome, y = estimate)) +
      geom_point(size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high),
        linewidth = 1
      )
    p_marg <- vis_helper(p_marg, "marg", J, use_col, label, treat)

    if (single_plot == TRUE) {
      if (!is.null(font)) {
        p_avg <- plot_add_font(p_avg, font) +
          theme(legend.position = "none")
        p_pair <- plot_add_font(p_pair, font) +
          theme(legend.position = "none")
        p_topk <- plot_add_font(p_topk, font) +
          theme(legend.position = "none")
        p_marg <- plot_add_font(p_marg, font) +
          theme(legend.position = "none")
      }
      return(
        ggarrange(p_avg, p_pair, p_topk, p_marg) +
          theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            legend.margin = margin(-0.5, 0, 0, 0, unit = "cm"),
            legend.spacing.x = unit(0, "cm"),
            legend.spacing.y = unit(0, "cm"),
            plot.title = element_text(face = "bold")
          )
      )
    } else {
      return(
        list(p_avg = p_avg, p_pair = p_pair, p_topk = p_topk, p_marg = p_marg)
      )
    }
  } else {
    # Visualization when there is a treatment
    # Prep for visualization
    # SK: I need to be walked through what's happening here with av_scenario
    av_scenario <- scenario <- list(
      c("Not_significant", "Negative", "Positive"),
      c("Not_significant", "Negative"),
      c("Not_significant", "Positive"),
      c("Negative", "Positive"),
      c("Not_significant"),
      c("Negative"),
      c("Positive")
    )
    scena_col <- list(
      c(color_palette[4], color_palette[2], color_palette[3]),
      c(color_palette[4], color_palette[2]),
      c(color_palette[4], color_palette[3]),
      c(color_palette[2], color_palette[3]),
      c(color_palette[4]),
      c(color_palette[2]),
      c(color_palette[3])
    )

    av_scena_col <- list(
      c(color_palette[4], color_palette[3], color_palette[2]),
      c(color_palette[4], color_palette[3]),
      c(color_palette[4], color_palette[2]),
      c(color_palette[3], color_palette[2]),
      c(color_palette[4]),
      c(color_palette[3]),
      c(color_palette[2])
    )

    color_significance <- function(x) {
      x$col <- ifelse(x$conf.low > 0, "Positive", "Not_significant")
      x$col <- ifelse(x$conf.high < 0, "Negative", x$col)
      return(x)
    }

    # Estimate ATEs with Difference-in-means via OLS
    m_rank_target <- lm_robust(Y_rank_target ~ D) %>% tidy()

    m_topk <- seq(J) %>%
      map(~ lm_robust(Y_topk[[.x]] ~ D) %>% tidy())

    m_pair <- other_items %>%
      imap(
        ~ lm_robust(Y_pair[[.x]] ~ 1) %>%
          tidy() %>%
          mutate(outcome = .y)
      )

    m_marg <- seq(J) %>%
      map(~ lm_robust(Y_marg[[.x]] ~ D) %>% tidy())

    m_rank <- m_rank_target %>%
      filter(term == "D") %>%
      mutate(
        outcome = paste0(target_item),
        target = "A"
      )

    gg_avg <- m_rank %>%
      label_capitalize() %>%
      color_significance() %>%
      unify_label_length()

    gg_pair <- do.call(rbind.data.frame, m_pair) %>%
      label_capitalize() %>%
      color_significance() %>%
      unify_label_length()

    gg_marg <- do.call(rbind.data.frame, m_marg) %>%
      filter(term == "D") %>%
      mutate(outcome = paste("Ranked", seq(J))) %>%
      rev_outcome() %>%
      color_significance() %>%
      unify_label_length()

    gg_topk <- do.call(rbind.data.frame, m_topk) %>%
      filter(term == "D") %>%
      mutate(outcome = paste0("Top-", seq(J))) %>%
      .[seq(J - 1), ] %>%
      rev_outcome() %>%
      color_significance() %>%
      unify_label_length()

    # Visualize all effects
    names(av_scena_col) <- av_scenario
    pattern <- unique(gg_avg$col) # Observed pattern
    use_col <- av_scena_col[pattern] # Use this color palette

    gg_avg$outcome <- factor(
      gg_avg$outcome,
      levels = gg_avg$outcome[order(-gg_avg$estimate)]
    )
    p_avg <- ggplot(gg_avg, aes(outcome, y = estimate)) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        linewidth = 1
      )
    p_avg <- vis_helper(p_avg, "avg", J, use_col, label, treat)

    names(scena_col) <- scenario
    pattern <- unique(gg_pair$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color palette

    gg_pair$outcome <- factor(
      gg_pair$outcome,
      levels = gg_pair$outcome[order(-gg_pair$estimate)]
    )
    p_pair <- ggplot(gg_pair, aes(outcome, y = estimate)) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        linewidth = 1
      )
    p_pair <- vis_helper(p_pair, "pair", J, use_col, label, treat)

    names(scena_col) <- scenario
    pattern <- unique(gg_topk$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color pallet

    gg_topk$outcome <- factor(
      gg_topk$outcome,
      levels = sort(unique(gg_topk$outcome))
    )
    p_topk <- ggplot(gg_topk, aes(outcome, y = estimate)) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        linewidth = 1
      )
    p_topk <- vis_helper(p_topk, "topk", J, use_col, label, treat)

    names(scena_col) <- scenario
    pattern <- unique(gg_marg$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color pallet

    gg_marg$outcome <- factor(
      gg_marg$outcome,
      levels = sort(unique(gg_marg$outcome))
    )
    p_marg <- ggplot(gg_marg, aes(outcome, y = estimate)) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        linewidth = 1
      )
    p_marg <- vis_helper(p_marg, "marg", J, use_col, label, treat)

    if (single_plot == TRUE) {
      if (!is.null(font)) {
        p_avg <- plot_add_font(p_avg, font) +
          theme(legend.position = "none")
        p_pair <- plot_add_font(p_pair, font) +
          theme(legend.position = "none")
        p_topk <- plot_add_font(p_topk, font) +
          theme(legend.position = "none")
        p_marg <- plot_add_font(p_marg, font) +
          theme(legend.position = "none")
      }
      return(
        ggarrange(p_avg, p_pair, p_topk, p_marg) +
          theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            legend.margin = margin(-0.5, 0, 0, 0, unit = "cm"),
            legend.spacing.x = unit(0, "cm"),
            legend.spacing.y = unit(0, "cm"),
            plot.title = element_text(face = "bold")
          )
      )
    } else {
      return(
        list(p_avg = p_avg, p_pair = p_pair, p_topk = p_topk, p_marg = p_marg)
      )
    }
  }
}

simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  output <- paste(
    toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " "
  )
  return(output)
}

vis_helper <- function(p, type, J, use_col, label, treat) {
  p <- p +
    ylab("") +
    xlab("") +
    coord_flip() +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
      text = element_text(size = 10),
      plot.title = element_text(size = 10)
    )
  if (tolower(type) == "avg") {
    p <- p +
      scale_colour_manual(values = use_col) +
      ggtitle(paste0("A. Average Ranks\n"))
    if (is.null(treat)) {
      p <- p +
        ylim(1, J) +
        geom_hline(yintercept = (J + 1) / 2, linetype = "dashed")
    }
  } else if (tolower(type) == "pair") {
    p <- p +
      scale_colour_manual(values = use_col) +
      ggtitle(
        paste0("B. Pair Ranking of\n", label, " ", "Over Others")
      )
    if (is.null(treat)) {
      p <- p +
        ylim(0, 1) +
        geom_hline(yintercept = 0.5, linetype = "dashed")
    }
  } else if (tolower(type) == "topk") {
    p <- p +
      scale_colour_manual(values = use_col) +
      ggtitle(paste0("C. Top-k Ranking\nof", " ", label))
    if (is.null(treat)) {
      p <- p +
        ylim(0, 1) +
        geom_hline(yintercept = 0.5, linetype = "dashed")
    }
  } else if (tolower(type) == "marg") {
    p <- p +
      scale_colour_manual(values = use_col) +
      ggtitle(paste0("D. Marginal Ranking\nof", " ", label))
    if (is.null(treat)) {
      p <- p +
        ylim(0, 1) +
        geom_hline(
          yintercept = seq(1 / J, 1 - 1 / J, by = 1 / J), linetype = "dashed"
        )
    }
  }

  return(p)
}

unify_label_length <- function(x, width = 20) {
  outcome <- NULL
  x %>%
    mutate(
      outcome = str_pad(outcome, width = width)
    )
}

plot_add_font <- function(p, font) {
  ## pdf_default + plot_nolegend
  out <- p +
    theme_bw() +
    theme(
      plot.title = element_text(family = font),
      text = element_text(family = font),
      axis.text.x = element_text(family = font),
      axis.text.y = element_text(family = font),
      legend.text = element_text(family = font)
    )
  return(out)
}
