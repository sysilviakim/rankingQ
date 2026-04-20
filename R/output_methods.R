.rankingq_null_coalesce <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

.rankingq_structure_output <- function(x,
                                       class,
                                       method_tables = NULL,
                                       metadata = list()) {
  structure(
    x,
    class = unique(c(class, "rankingQ_output", "list")),
    rankingQ_method_tables = method_tables,
    rankingQ_meta = metadata
  )
}

.rankingq_get_method_tables <- function(x) {
  tables <- attr(x, "rankingQ_method_tables", exact = TRUE)
  if (!is.null(tables)) {
    return(tables)
  }

  meta <- .rankingq_get_meta(x)
  primary_method <- meta$primary_method %||% "direct"
  if (!is.null(x$results)) {
    return(setNames(list(x$results), primary_method))
  }

  NULL
}

.rankingq_get_meta <- function(x) {
  attr(x, "rankingQ_meta", exact = TRUE) %||% list()
}

`%||%` <- .rankingq_null_coalesce

.rankingq_available_methods <- function(x) {
  names(.rankingq_get_method_tables(x))
}

.rankingq_default_method <- function(x) {
  meta <- .rankingq_get_meta(x)
  primary_method <- meta$primary_method
  if (!is.null(primary_method)) {
    return(primary_method)
  }

  available <- .rankingq_available_methods(x)
  if (length(available) == 0L) {
    return(NULL)
  }

  available[[1]]
}

.rankingq_match_method <- function(x, method = NULL) {
  available <- .rankingq_available_methods(x)
  default_method <- .rankingq_default_method(x)

  if (is.null(method)) {
    if (is.null(default_method)) {
      stop("No standardized method tables are attached to this object.")
    }
    return(default_method)
  }

  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("method must be a single character string.")
  }

  normalized <- tolower(gsub("[^a-z0-9]+", "_", method))
  if (!(normalized %in% available)) {
    stop(
      "method must be one of: ",
      paste(available, collapse = ", "),
      "."
    )
  }

  normalized
}

.rankingq_match_type <- function(type = NULL, allow_null = TRUE) {
  if (is.null(type)) {
    if (allow_null) {
      return(NULL)
    }
    return("average_rank")
  }

  if (!is.character(type) || length(type) < 1L || anyNA(type)) {
    stop("type must be a character vector.")
  }

  normalized <- tolower(gsub("[^a-z0-9]+", "_", type))
  key <- c(
    average_rank = "average_rank",
    average = "average_rank",
    pairwise = "pairwise",
    pairwise_ranking = "pairwise",
    top_k = "top_k",
    top = "top_k",
    marginal = "marginal",
    marginal_ranking = "marginal"
  )
  out <- unname(key[normalized])
  if (anyNA(out)) {
    stop(
      "type must be one of: average_rank, pairwise, top_k, marginal."
    )
  }

  unique(out)
}

.rankingq_type_from_qoi <- function(qoi) {
  qoi <- tolower(as.character(qoi))
  map <- c(
    "average rank" = "average_rank",
    "pairwise ranking" = "pairwise",
    "top-k ranking" = "top_k",
    "marginal ranking" = "marginal"
  )
  out <- unname(map[qoi])
  ifelse(is.na(out), "unknown", out)
}

.rankingq_pretty_type <- function(type) {
  switch(
    type,
    average_rank = "Average Rank",
    pairwise = "Pairwise Probability",
    top_k = "Top-k Probability",
    marginal = "Marginal Probability",
    type
  )
}

.rankingq_pretty_method <- function(method) {
  switch(
    method,
    raw = "Raw",
    direct = "Direct",
    ipw = "IPW",
    method
  )
}

.rankingq_estimate_to_summary_table <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  data.frame(
    item = as.character(x$item),
    qoi = as.character(x$qoi),
    outcome = as.character(x$outcome),
    mean = as.numeric(x$estimate),
    lower = NA_real_,
    upper = NA_real_,
    stringsAsFactors = FALSE
  )
}

.rankingq_standardize_qoi_table <- function(x, method) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!all(c("item", "qoi", "outcome") %in% names(x))) {
    stop("Standardized estimate tables must contain item, qoi, and outcome.")
  }

  estimate <- if ("mean" %in% names(x)) x$mean else x$estimate
  lower <- if ("lower" %in% names(x)) x$lower else rep(NA_real_, nrow(x))
  upper <- if ("upper" %in% names(x)) x$upper else rep(NA_real_, nrow(x))

  out <- data.frame(
    method = rep(method, nrow(x)),
    type = vapply(x$qoi, .rankingq_type_from_qoi, character(1)),
    item = as.character(x$item),
    term = rep(NA_character_, nrow(x)),
    outcome = as.character(x$outcome),
    comparison_item = rep(NA_character_, nrow(x)),
    k = rep(NA_integer_, nrow(x)),
    estimate = as.numeric(estimate),
    conf.low = as.numeric(lower),
    conf.high = as.numeric(upper),
    stringsAsFactors = FALSE
  )

  avg_idx <- out$type == "average_rank"
  out$term[avg_idx] <- out$item[avg_idx]

  pair_idx <- out$type == "pairwise"
  out$comparison_item[pair_idx] <- sub("^v\\.\\s+", "", out$outcome[pair_idx])
  out$term[pair_idx] <- out$comparison_item[pair_idx]

  top_idx <- out$type == "top_k"
  out$k[top_idx] <- as.integer(sub("^Top-", "", out$outcome[top_idx]))
  out$term[top_idx] <- paste0("Top ", out$k[top_idx])

  marginal_idx <- out$type == "marginal"
  out$k[marginal_idx] <- as.integer(sub("^Ranked\\s+", "", out$outcome[marginal_idx]))
  out$term[marginal_idx] <- paste0("Rank ", out$k[marginal_idx])

  tibble::as_tibble(out)
}

.rankingq_get_estimates <- function(x, method = NULL, type = NULL, item = NULL) {
  method <- .rankingq_match_method(x, method)
  tables <- .rankingq_get_method_tables(x)
  out <- .rankingq_standardize_qoi_table(tables[[method]], method = method)

  type <- .rankingq_match_type(type, allow_null = TRUE)
  if (!is.null(type)) {
    out <- out[out$type %in% type, , drop = FALSE]
  }

  if (!is.null(item)) {
    if (!is.character(item) || anyNA(item)) {
      stop("item must be a character vector when supplied.")
    }
    out <- out[out$item %in% item, , drop = FALSE]
  }

  if (nrow(out) == 0L) {
    stop("No estimates matched the requested method/type/item combination.")
  }

  tibble::as_tibble(out)
}

.rankingq_tidy_p_random <- function(x) {
  raw <- x$est_p_random

  if (is.null(raw)) {
    return(tibble::tibble(
      term = "p_random",
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_
    ))
  }

  if (is.numeric(raw) && length(raw) == 1L) {
    return(tibble::tibble(
      term = "p_random",
      estimate = as.numeric(raw),
      conf.low = NA_real_,
      conf.high = NA_real_
    ))
  }

  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  tibble::tibble(
    term = "p_random",
    estimate = as.numeric(raw$mean[[1]]),
    conf.low = if ("lower" %in% names(raw)) as.numeric(raw$lower[[1]]) else NA_real_,
    conf.high = if ("upper" %in% names(raw)) as.numeric(raw$upper[[1]]) else NA_real_
  )
}

.rankingq_round_numeric_columns <- function(x, digits = 3L) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  num_cols <- vapply(x, is.numeric, logical(1))
  x[num_cols] <- lapply(x[num_cols], round, digits = digits)
  x
}

#' Tidy rankingQ estimator outputs
#'
#' @param x A rankingQ estimator output object.
#' @param component Which part of the object to tidy. Defaults to
#'   \code{"estimates"}.
#' @param method Which estimator to return. Supported values are
#'   \code{"raw"}, \code{"direct"}, and \code{"ipw"} when available for the
#'   object.
#' @param type Estimate type filter. Supported values are
#'   \code{"average_rank"}, \code{"pairwise"}, \code{"top_k"}, and
#'   \code{"marginal"}.
#' @param item Optional item filter.
#' @param conf.int If \code{FALSE}, confidence-interval columns are omitted from
#'   the returned tibble.
#' @param ... Unused.
#'
#' @return A tibble with a standardized layout for ranking estimates.
#'
#' @importFrom generics tidy
#' @export
tidy.rankingQ_output <- function(x,
                                 component = c("estimates", "p_random"),
                                 method = NULL,
                                 type = NULL,
                                 item = NULL,
                                 conf.int = TRUE,
                                 ...) {
  component <- match.arg(component)

  if (component == "p_random") {
    return(.rankingq_tidy_p_random(x))
  }

  out <- .rankingq_get_estimates(
    x,
    method = method,
    type = type,
    item = item
  )

  if (!isTRUE(conf.int)) {
    out <- out[, setdiff(names(out), c("conf.low", "conf.high")), drop = FALSE]
  }

  tibble::as_tibble(out)
}

#' Summarize rankingQ estimator outputs
#'
#' @param object A rankingQ estimator output object.
#' @param method Which estimator to summarize. Defaults to the object's primary
#'   method.
#' @param type Estimate type to display. Defaults to \code{"average_rank"}.
#' @param item Optional item filter.
#' @param n Number of rows to preview in the printed summary.
#' @param ... Unused.
#'
#' @return A summary object with a compact human-readable overview.
#'
#' @export
summary.rankingQ_output <- function(object,
                                    method = NULL,
                                    type = "average_rank",
                                    item = NULL,
                                    n = 6L,
                                    ...) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1) {
    stop("n must be a single integer >= 1.")
  }

  method <- .rankingq_match_method(object, method)
  type <- .rankingq_match_type(type, allow_null = FALSE)[[1]]
  estimates <- .rankingq_get_estimates(
    object,
    method = method,
    type = type,
    item = item
  )
  meta <- .rankingq_get_meta(object)

  structure(
    list(
      call = meta$call,
      method = method,
      type = type,
      estimates = estimates,
      p_random = .rankingq_tidy_p_random(object),
      n = as.integer(n),
      J = meta$J,
      n_bootstrap = meta$n_bootstrap,
      n_obs = meta$n_obs,
      population = meta$population,
      assumption = meta$assumption
    ),
    class = "summary.rankingQ_output"
  )
}

#' @rdname summary.rankingQ_output
#' @param digits Number of digits to print.
#'
#' @export
print.summary.rankingQ_output <- function(x, digits = 3L, ...) {
  if (!is.null(x$call)) {
    cat("Call:\n")
    print(x$call)
    cat("\n")
  }

  cat(.rankingq_pretty_method(x$method), " estimates\n", sep = "")
  cat("Type: ", .rankingq_pretty_type(x$type), "\n", sep = "")
  if (!is.null(x$J)) {
    cat("Items: ", x$J, "\n", sep = "")
  }
  if (!is.null(x$n_obs)) {
    cat("Observations: ", x$n_obs, "\n", sep = "")
  }
  if (!is.null(x$n_bootstrap)) {
    cat("Bootstrap draws: ", x$n_bootstrap, "\n", sep = "")
  }
  if (!is.null(x$population)) {
    cat("Population: ", x$population, "\n", sep = "")
  }
  if (!is.null(x$assumption) && identical(x$population, "all")) {
    cat("Assumption: ", x$assumption, "\n", sep = "")
  }

  cat("\nEstimated random response rate:\n")
  print(.rankingq_round_numeric_columns(x$p_random, digits = digits), row.names = FALSE)

  preview <- utils::head(x$estimates, x$n)
  preview <- as.data.frame(preview, stringsAsFactors = FALSE)
  if ("term" %in% names(preview) && all(preview$term == preview$item)) {
    preview$term <- NULL
  }
  if (all(is.na(preview$conf.low)) && all(is.na(preview$conf.high))) {
    preview$conf.low <- NULL
    preview$conf.high <- NULL
  }

  cat("\nEstimates:\n")
  print(.rankingq_round_numeric_columns(preview, digits = digits), row.names = FALSE)
  if (nrow(x$estimates) > x$n) {
    cat("... ", nrow(x$estimates) - x$n, " more rows\n", sep = "")
  }

  invisible(x)
}

#' Plot rankingQ estimator outputs
#'
#' @param x A rankingQ estimator output object.
#' @param y Ignored.
#' @param type Estimate type to plot. Defaults to \code{"average_rank"}.
#' @param method Which estimator to plot. Defaults to the object's primary
#'   method.
#' @param item Optional item filter.
#' @param xlab X-axis label. If \code{NULL}, a sensible default is used.
#' @param ylab Y-axis label. Defaults to an empty string.
#' @param ... Passed through to \code{autoplot()}.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 autoplot
#' @export
plot.rankingQ_output <- function(x,
                                 y = NULL,
                                 type = "average_rank",
                                 method = NULL,
                                 item = NULL,
                                 xlab = NULL,
                                 ylab = "",
                                 ...) {
  autoplot(
    x,
    type = type,
    method = method,
    item = item,
    xlab = xlab,
    ylab = ylab,
    ...
  )
}

#' @rdname plot.rankingQ_output
#' @param conf.int If \code{TRUE}, confidence intervals are drawn when
#'   available.
#'
#' @export
autoplot.rankingQ_output <- function(object,
                                     type = "average_rank",
                                     method = NULL,
                                     item = NULL,
                                     conf.int = TRUE,
                                     xlab = NULL,
                                     ylab = "",
                                     ...) {
  type <- .rankingq_match_type(type, allow_null = FALSE)[[1]]
  plot_data <- tidy(
    object,
    component = "estimates",
    method = method,
    type = type,
    item = item,
    conf.int = TRUE
  )
  method <- unique(plot_data$method)[[1]]

  if (type == "average_rank") {
    plot_data$display_term <- plot_data$item
  } else {
    plot_data$display_term <- plot_data$term
  }
  plot_data$display_term <- factor(
    plot_data$display_term,
    levels = rev(unique(plot_data$display_term))
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = estimate, y = display_term)
  )

  if (isTRUE(conf.int) &&
      all(c("conf.low", "conf.high") %in% names(plot_data)) &&
      any(!is.na(plot_data$conf.low) & !is.na(plot_data$conf.high))) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(xmin = conf.low, xmax = conf.high),
      na.rm = TRUE
    )
  }

  p <- p +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab)

  if (is.null(xlab)) {
    xlab <- .rankingq_pretty_type(type)
  }
  p <- p + ggplot2::xlab(xlab)

  if (type != "average_rank" && length(unique(plot_data$item)) > 1L) {
    p <- p + ggplot2::facet_wrap(~item, scales = "free_y")
  }

  p + ggplot2::labs(subtitle = paste("Method:", .rankingq_pretty_method(method)))
}
