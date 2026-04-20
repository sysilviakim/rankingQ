# Stratified Estimate of Average Ranks

This function estimates the average ranks based on stratification.

## Usage

``` r
stratified_avg(
  data,
  var_stratum,
  J = NULL,
  main_q,
  anc_correct = NULL,
  labels = NULL,
  seed = 1234,
  weight = NULL,
  n_bootstrap = 200,
  ipw = FALSE,
  verbose = FALSE,
  p_random = NULL
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

  Main ranking question specification. This can be a single column name
  or unquoted symbol such as \`app_identity\`, in which case the
  function looks for \`app_identity_1\`, \`app_identity_2\`, and so on.
  You may also supply \`main_q\` directly as a character vector or
  unquoted \`c(...)\` expression of ranking columns.

- anc_correct:

  Optional indicator for passing the anchor question. If \`NULL\`,
  \`p_random\` is used when supplied; otherwise the function defaults to
  \`p_random = 0\` and applies no correction.

- labels:

  A vector of labels for the items being ranked. Defaults to NULL.

- seed:

  Seed for `set.seed` for reproducibility.

- weight:

  Either a numeric vector of weights with length \`nrow(data)\`, the
  name of a weight column in \`data\`, or an unquoted weight column
  name. Defaults to \`NULL\`.

- n_bootstrap:

  Number of bootstraps. Defaults to 200.

- ipw:

  Indicator for using inverse probability weighting. Defaults to FALSE,
  in which case direct bias estimation will be employed.

- verbose:

  Indicator for verbose output. Defaults to FALSE.

- p_random:

  Optional fixed proportion of random/inattentive respondents. When
  supplied, this overrides \`anc_correct\` and a message is shown if
  both are provided.

## Value

A data frame with the bootstrap-estimated average ranks.

## Examples

``` r
identity2 <- identity
identity2$stratum <- rep(c("group1", "group2"), length.out = nrow(identity2))
out <- suppressMessages(stratified_avg(
  identity2,
  var_stratum = "stratum",
  main_q = "app_identity",
  p_random = 0,
  n_bootstrap = 1,
  seed = 123
))
head(out)
#>       mean           item
#> 1 3.052680 app_identity_1
#> 2 2.536044 app_identity_2
#> 3 1.949168 app_identity_3
#> 4 2.462107 app_identity_4
```
