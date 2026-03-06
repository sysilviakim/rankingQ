# Weighted one-way and two-way frequency tables.

Copied from questionr::wtd.table. Not exported

## Usage

``` r
wtd.table(
  x,
  y = NULL,
  weights = NULL,
  digits = 3,
  normwt = FALSE,
  useNA = c("no", "ifany", "always"),
  na.rm = TRUE,
  na.show = FALSE,
  exclude = NULL
)
```

## Arguments

- x:

  a vector

- y:

  another optional vector for a two-way frequency table. Must be the
  same length as x

- weights:

  vector of weights, must be the same length as x

- digits:

  Number of significant digits.

- normwt:

  if TRUE, normalize weights so that the total weighted count is the
  same as the unweighted one

- useNA:

  whether to include NA values in the table

- na.rm:

  (deprecated) if TRUE, remove NA values before computation

- na.show:

  (deprecated) if TRUE, show NA count in table output

- exclude:

  values to remove from x and y. To exclude NA, use na.rm argument.
