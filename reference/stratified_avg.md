# Stratified Estimate of Average Ranks

This function estimates the average ranks based on stratification.

## Usage

``` r
stratified_avg(
  data,
  var_stratum,
  J = NULL,
  main_q,
  anc_correct,
  labels = NULL,
  seed = 1234,
  weight = NULL,
  n_bootstrap = 200,
  ipw = FALSE,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame containing the ranking data as well as the stratifying
  variable.

- var_stratum:

  The name of the stratifying variable.

- J:

  The number of items in the ranking question. Defaults to NULL, in
  which case it will be inferred from the data.

- main_q:

  Column name for the main ranking question to be analyzed.

- anc_correct:

  Indicator for passing the anchor question.

- labels:

  A vector of labels for the items being ranked. Defaults to NULL.

- seed:

  Seed for `set.seed` for reproducibility.

- weight:

  Either a numeric vector of weights with length `nrow(data)` or the
  name of a weight column in `data`. Defaults to `NULL`.

- n_bootstrap:

  Number of bootstraps. Defaults to 200.

- ipw:

  Indicator for using inverse probability weighting. Defaults to FALSE,
  in which case direct bias estimation will be employed.

- verbose:

  Indicator for verbose output. Defaults to FALSE.

## Value

A data frame with the bootstrap-estimated average ranks.
