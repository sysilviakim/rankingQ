# Implements Plug-in Bias-Corrected Estimators for Ranking Data (Rcpp)

This function implements the bias correction of the ranking distribution
using a paired anchor question. This is a fast Rcpp-based implementation
that is approximately 200-300x faster than the tidyverse version.

## Usage

``` r
imprr_direct_rcpp(
  data,
  J = NULL,
  main_q,
  anc_correct = NULL,
  population = "non-random",
  assumption = "contaminated",
  n_bootstrap = 200,
  seed = 123456,
  weight = NULL,
  verbose = FALSE,
  p_random = NULL
)
```

## Arguments

- data:

  The input dataset with ranking data.

- J:

  The number of items in the ranking question. Defaults to NULL, in
  which case it will be inferred from the data.

- main_q:

  Ranking question to be analyzed. When \`main_q\` is a single column
  name or unquoted symbol such as \`app_identity\`, the function looks
  for \`app_identity_1\`, \`app_identity_2\`, \`app_identity_3\`, and so
  on. You may also supply \`main_q\` directly as a character vector or
  unquoted \`c(...)\` expression of ranking columns such as \`c(party,
  gender, race, religion)\`.

- anc_correct:

  Optional indicator for passing the anchor question. If \`NULL\`,
  \`p_random\` is used when supplied; otherwise the function defaults to
  \`p_random = 0\` and applies no correction.

- population:

  Choice of the target population out of non-random respondents
  (default) or all respondents.

- assumption:

  Choice of identifying assumption when \`population = "all"\`:
  \`uniform\` assumes random respondents would have uniform
  counterfactual preferences, while \`contaminated\` assumes their
  counterfactual preferences match those of non-random respondents.

- n_bootstrap:

  Number of bootstraps. Defaults to 200.

- seed:

  Seed for `set.seed` for reproducibility.

- weight:

  The name of the weight column in \`data\`. Defaults to \`NULL\`, which
  uses equal weights. This can also be supplied as a numeric vector or
  as an unquoted column name.

- verbose:

  Indicator for verbose output. Defaults to FALSE.

- p_random:

  Optional fixed proportion of random/inattentive respondents. When
  supplied, this overrides \`anc_correct\` and a message is shown if
  both are provided.

## Value

A list with two elements:

- est_p_random:

  Summary statistics for the estimated proportion of random respondents
  (mean, lower, upper)

- results:

  A tibble with bias-corrected estimates for all items, including
  average ranks, pairwise probabilities, top-k probabilities, and
  marginal probabilities

## Examples

``` r
out <- imprr_direct_rcpp(
  identity,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 1,
  seed = 123
)
#> No weight column supplied; using equal weights for all observations.
out$est_p_random
#> # A tibble: 1 × 3
#>    mean lower upper
#>   <dbl> <dbl> <dbl>
#> 1 0.322 0.322 0.322
head(out$results)
#> # A tibble: 6 × 6
#>   item           qoi              outcome               mean  lower  upper
#>   <chr>          <chr>            <chr>                <dbl>  <dbl>  <dbl>
#> 1 app_identity_1 average rank     Avg: app_identity_1 3.30   3.30   3.30  
#> 2 app_identity_1 pairwise ranking v. app_identity_2   0.349  0.349  0.349 
#> 3 app_identity_1 pairwise ranking v. app_identity_3   0.0924 0.0924 0.0924
#> 4 app_identity_1 pairwise ranking v. app_identity_4   0.256  0.256  0.256 
#> 5 app_identity_1 top-k ranking    Top-1               0.0298 0.0298 0.0298
#> 6 app_identity_1 top-k ranking    Top-2               0.171  0.171  0.171 
```
