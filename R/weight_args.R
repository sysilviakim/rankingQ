.resolve_weight_vector <- function(data, weight, N) {
  validate_weight_values <- function(weight_vec) {
    if (anyNA(weight_vec) || any(!is.finite(weight_vec))) {
      stop("weight values must be finite and non-missing.")
    }
    if (any(weight_vec < 0)) {
      stop("weight values cannot be negative.")
    }
    if (sum(weight_vec) <= 0) {
      stop("weight values must sum to a positive number.")
    }

    as.numeric(weight_vec)
  }

  if (is.null(weight)) {
    return(rep(1, N))
  }

  if (is.numeric(weight)) {
    if (length(weight) != N) {
      stop("weight vector must have the same length as the number of rows in data.")
    }
    return(validate_weight_values(weight))
  }

  if (!is.character(weight) || length(weight) != 1 || is.na(weight)) {
    stop("weight must be NULL, a numeric vector, or a single column name in data.")
  }

  weight_vec <- data[[weight]]
  if (is.null(weight_vec)) {
    stop("weight column not found in data.")
  }
  if (!is.numeric(weight_vec)) {
    stop("weight column must be numeric.")
  }
  if (length(weight_vec) != N) {
    stop(
      "weight column must have the same length as the number of rows in data."
    )
  }
  validate_weight_values(weight_vec)
}
