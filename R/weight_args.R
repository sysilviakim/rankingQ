.resolve_weight_vector <- function(data, weight, N) {
  if (is.null(weight)) {
    return(rep(1, N))
  }

  if (is.numeric(weight)) {
    if (length(weight) != N) {
      stop("weight vector must have the same length as the number of rows in data.")
    }
    return(weight)
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

  weight_vec
}
