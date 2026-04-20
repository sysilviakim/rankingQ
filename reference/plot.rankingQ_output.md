# Plot rankingQ estimator outputs

Plot rankingQ estimator outputs

## Usage

``` r
# S3 method for class 'rankingQ_output'
plot(
  x,
  y = NULL,
  type = "average_rank",
  method = NULL,
  item = NULL,
  xlab = NULL,
  ylab = "",
  ...
)

# S3 method for class 'rankingQ_output'
autoplot(
  object,
  type = "average_rank",
  method = NULL,
  item = NULL,
  conf.int = TRUE,
  xlab = NULL,
  ylab = "",
  ...
)
```

## Arguments

- x:

  A rankingQ estimator output object.

- y:

  Ignored.

- type:

  Estimate type to plot. Defaults to `"average_rank"`.

- method:

  Which estimator to plot. Defaults to the object's primary method.

- item:

  Optional item filter.

- xlab:

  X-axis label. If `NULL`, a sensible default is used.

- ylab:

  Y-axis label. Defaults to an empty string.

- ...:

  Passed through to `autoplot()`.

- conf.int:

  If `TRUE`, confidence intervals are drawn when available.

## Value

A ggplot object.
