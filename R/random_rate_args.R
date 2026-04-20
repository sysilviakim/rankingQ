.validate_p_random <- function(p_random) {
  if (is.null(p_random)) {
    return(NULL)
  }

  if (!is.numeric(p_random) || length(p_random) != 1 || is.na(p_random) ||
      !is.finite(p_random) || p_random < 0 || p_random > 1) {
    stop("p_random must be NULL or a single finite number between 0 and 1.")
  }

  as.numeric(p_random)
}

.resolve_random_response_inputs <- function(data,
                                           anc_correct,
                                           p_random,
                                           emit_default_message = TRUE) {
  p_random <- .validate_p_random(p_random)

  if (!is.null(p_random)) {
    if (!is.null(anc_correct)) {
      message(
        "p_random supplied; ignoring anc_correct and using the supplied ",
        "fixed random-response rate."
      )
    }
    return(
      list(
        method = "fixed",
        anc_correct = NULL,
        p_random = p_random
      )
    )
  }

  if (!is.null(anc_correct)) {
    if (!is.character(anc_correct) || length(anc_correct) != 1 ||
        is.na(anc_correct)) {
      stop("anc_correct must be a single column name.")
    }
    if (!(anc_correct %in% names(data))) {
      stop("anc_correct column not found in data.")
    }

    return(
      list(
        method = "anchor",
        anc_correct = anc_correct,
        p_random = NULL
      )
    )
  }

  if (emit_default_message) {
    message(
      "No anc_correct or p_random supplied; assuming everyone passes the ",
      "anchor (p_random = 0), so no correction is applied."
    )
  }

  list(
    method = "fixed",
    anc_correct = NULL,
    p_random = 0
  )
}

.estimate_p_non_random_from_anchor <- function(anc_correct, weight, J) {
  keep <- !is.na(anc_correct) & !is.na(weight)
  if (!any(keep)) {
    return(NA_real_)
  }

  prop_correct <- sum(anc_correct[keep] * weight[keep]) / sum(weight[keep])
  (prop_correct - 1 / factorial(J)) / (1 - 1 / factorial(J))
}
