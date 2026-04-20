.normalize_population_args <- function(population, assumption) {
  allowed_population <- c("non-random", "all")
  allowed_assumption <- c("contaminated", "uniform")
  population_map <- c(
    "nonrandom" = "non-random",
    "all" = "all"
  )
  assumption_map <- c(
    "contaminated" = "contaminated",
    "contaminate" = "contaminated",
    "contaminatedsampling" = "contaminated",
    "uniform" = "uniform",
    "uniformpreference" = "uniform"
  )

  normalize_key <- function(x) {
    gsub("[^a-z]+", "", tolower(trimws(x)))
  }

  if (!is.character(population) || length(population) != 1 ||
      is.na(population)) {
    stop(
      "population must be one of: ",
      paste(allowed_population, collapse = ", "),
      "."
    )
  }
  population_key <- normalize_key(population)
  if (!(population_key %in% names(population_map))) {
    stop(
      "population must be one of: ",
      paste(allowed_population, collapse = ", "),
      "."
    )
  }
  population <- unname(population_map[[population_key]])

  if (!is.character(assumption) || length(assumption) != 1 ||
      is.na(assumption)) {
    stop(
      "assumption must be one of: ",
      paste(allowed_assumption, collapse = ", "),
      "."
    )
  }
  assumption_key <- normalize_key(assumption)
  if (!(assumption_key %in% names(assumption_map))) {
    stop(
      "assumption must be one of: ",
      paste(allowed_assumption, collapse = ", "),
      "."
    )
  }
  assumption <- unname(assumption_map[[assumption_key]])

  if (population == "non-random" && assumption != "contaminated") {
    stop(
      "assumption is only used when population = 'all'; ",
      "for population = 'non-random', use assumption = 'contaminated' ",
      "or leave the default."
    )
  }

  list(population = population, assumption = assumption)
}
