# Tidy rankingQ estimator outputs

Tidy rankingQ estimator outputs

## Usage

``` r
# S3 method for class 'rankingQ_output'
tidy(
  x,
  component = c("estimates", "p_random"),
  method = NULL,
  type = NULL,
  item = NULL,
  conf.int = TRUE,
  ...
)
```

## Arguments

- x:

  A rankingQ estimator output object.

- component:

  Which part of the object to tidy. Defaults to `"estimates"`.

- method:

  Which estimator to return. Supported values are `"raw"`, `"direct"`,
  and `"ipw"` when available for the object.

- type:

  Estimate type filter. Supported values are `"average_rank"`,
  `"pairwise"`, `"top_k"`, and `"marginal"`.

- item:

  Optional item filter.

- conf.int:

  If `FALSE`, confidence-interval columns are omitted from the returned
  tibble.

- ...:

  Unused.

## Value

A tibble with a standardized layout for ranking estimates.
