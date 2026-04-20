.extract_name_vector_from_expr <- function(expr) {
  if (is.null(expr) || identical(expr, quote(NULL))) {
    return(NULL)
  }

  if (is.symbol(expr)) {
    return(as.character(expr))
  }

  if (is.character(expr)) {
    return(expr)
  }

  if (is.call(expr)) {
    fn <- expr[[1]]

    if (is.symbol(fn) && identical(as.character(fn), "(")) {
      return(.extract_name_vector_from_expr(expr[[2]]))
    }

    if (is.symbol(fn) && identical(as.character(fn), "c")) {
      parts <- lapply(as.list(expr)[-1], .extract_name_vector_from_expr)
      if (length(parts) == 0L || any(vapply(parts, is.null, logical(1)))) {
        return(NULL)
      }
      return(unlist(parts, use.names = FALSE))
    }
  }

  NULL
}

.resolve_name_vector_input <- function(expr,
                                       env,
                                       arg_name,
                                       allow_multiple = TRUE) {
  value <- try(eval(expr, env), silent = TRUE)

  if (!inherits(value, "try-error") && is.null(value)) {
    return(NULL)
  }

  if (!inherits(value, "try-error") && is.character(value)) {
    names_vec <- unname(value)
  } else {
    names_vec <- .extract_name_vector_from_expr(expr)
  }

  if (is.null(names_vec) || !is.character(names_vec) ||
      anyNA(names_vec) || any(!nzchar(names_vec))) {
    if (!allow_multiple) {
      stop(arg_name, " must be a single column name.")
    }
    stop(
      arg_name,
      " must be a column name, a character vector of column names, ",
      "or an unquoted c(...) expression."
    )
  }

  if (!allow_multiple && length(names_vec) != 1L) {
    stop(arg_name, " must be a single column name.")
  }

  names_vec
}

.validate_J_input <- function(J) {
  if (!is.numeric(J) || length(J) != 1 || is.na(J) || !is.finite(J) ||
      J < 2 || J != as.integer(J)) {
    stop("J must be a single integer >= 2.")
  }

  as.integer(J)
}

.resolve_main_q_columns <- function(data, main_q_expr, env, J) {
  main_q <- .resolve_name_vector_input(
    main_q_expr,
    env,
    "main_q",
    allow_multiple = TRUE
  )

  if (length(main_q) == 1L) {
    prefix <- main_q[[1]]
    has_full_ranking_col <- prefix %in% names(data)

    if (is.null(J)) {
      if (!has_full_ranking_col) {
        stop(
          "When J is NULL, main_q must exist as a column in data so J can be inferred."
        )
      }
      J <- .infer_ranking_size(data[[prefix]])
    }

    J <- .validate_J_input(J)
    ranking_cols <- paste0(prefix, "_", seq_len(J))
    missing_ranking_cols <- setdiff(ranking_cols, names(data))
    if (length(missing_ranking_cols) > 0) {
      stop(
        "Missing ranking columns for main_q: ",
        paste(missing_ranking_cols, collapse = ", ")
      )
    }

    return(
      list(
        main_q = main_q,
        ranking_cols = ranking_cols,
        J = J,
        has_full_ranking_col = has_full_ranking_col
      )
    )
  }

  if (is.null(J)) {
    J <- length(main_q)
  }
  J <- .validate_J_input(J)

  if (length(main_q) != J) {
    stop(
      "When main_q is supplied as ranking column names, its length must equal J."
    )
  }

  missing_ranking_cols <- setdiff(main_q, names(data))
  if (length(missing_ranking_cols) > 0) {
    stop(
      "main_q columns not found in data: ",
      paste(missing_ranking_cols, collapse = ", ")
    )
  }

  list(
    main_q = main_q,
    ranking_cols = main_q,
    J = J,
    has_full_ranking_col = FALSE
  )
}

.resolve_weight_input <- function(expr, env) {
  value <- try(eval(expr, env), silent = TRUE)

  if (!inherits(value, "try-error")) {
    if (is.null(value)) {
      return(NULL)
    }
    if (is.numeric(value)) {
      return(value)
    }
    if (is.character(value) && length(value) == 1L &&
        !is.na(value) && nzchar(value)) {
      return(unname(value))
    }
  }

  weight_name <- .extract_name_vector_from_expr(expr)
  if (!is.null(weight_name) && length(weight_name) == 1L &&
      !is.na(weight_name) && nzchar(weight_name)) {
    return(weight_name)
  }

  stop("weight must be NULL, a numeric vector, or a single column name in data.")
}
