.resolve_weight_vector <- function(data, weight, N) {
  if (is.null(weight)) {
    message(
      "No weight column supplied; using equal weights for all observations."
    )
    return(rep(1, N))
  }

  if (!is.character(weight) || length(weight) != 1 || is.na(weight)) {
    stop("weight must be NULL or a single column name in data.")
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
