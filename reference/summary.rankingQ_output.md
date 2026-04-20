# Summarize rankingQ estimator outputs

Summarize rankingQ estimator outputs

## Usage

``` r
# S3 method for class 'rankingQ_output'
summary(object, method = NULL, type = "average_rank", item = NULL, n = 6L, ...)

# S3 method for class 'summary.rankingQ_output'
print(x, digits = 3L, ...)
```

## Arguments

- object:

  A rankingQ estimator output object.

- method:

  Which estimator to summarize. Defaults to the object's primary method.

- type:

  Estimate type to display. Defaults to `"average_rank"`.

- item:

  Optional item filter.

- n:

  Number of rows to preview in the printed summary.

- ...:

  Unused.

- digits:

  Number of digits to print.

## Value

A summary object with a compact human-readable overview.
