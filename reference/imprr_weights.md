# Computes Bias-Correction Weights for Ranking Data

This function implements the bias correction of the ranking distribution
using a paired anchor question, using the IPW estimator.

## Usage

``` r
imprr_weights(
  data,
  J = NULL,
  main_q,
  anc_correct = NULL,
  population = "non-random",
  assumption = "contaminated",
  weight = NULL,
  ranking = "ranking",
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

- weight:

  The name of the weight column in \`data\`. Defaults to \`NULL\`, which
  uses equal weights. This can also be supplied as a numeric vector or
  as an unquoted column name.

- ranking:

  The name of the column that will store the full ranking profile.
  Defaults to "ranking". If \`main_q\` exists in the data, the produced
  column should be identical to \`main_q\`. However, the function
  defaults to creating another column by combining marginal rankings,
  just in case.

- p_random:

  Optional fixed proportion of random/inattentive respondents. When
  supplied, this overrides \`anc_correct\` and a message is shown if
  both are provided.

## Value

A list with three elements:

- est_p_random:

  A numeric value representing the estimated proportion of random
  responses.

- results:

  A data frame with the original data augmented with a `weights` column
  containing inverse probability weights and a `ranking` column with
  unified ranking patterns.

- rankings:

  A data frame with ranking patterns, observed proportions (`prop_obs`),
  bias-corrected proportions (`prop_bc`), and inverse probability
  weights (`weights`) for each permutation.

## Examples

``` r
out <- imprr_weights(
  identity,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)
#> No weight column supplied; using equal weights for all observations.
head(out$results)
#> # A tibble: 6 × 18
#>   weights s_weight app_identity app_identity_1 app_identity_2 app_identity_3
#>     <dbl>    <dbl> <chr>                 <dbl>          <dbl>          <dbl>
#> 1    1.02    0.844 1423                      1              4              2
#> 2    1.02    0.886 1423                      1              4              2
#> 3    1.27    2.96  3412                      3              4              1
#> 4    1.02    0.987 1423                      1              4              2
#> 5    1.10    1.76  4132                      4              1              3
#> 6    1.02    0.469 3124                      3              1              2
#> # ℹ 12 more variables: app_identity_4 <dbl>, anc_identity <chr>,
#> #   anc_identity_1 <dbl>, anc_identity_2 <dbl>, anc_identity_3 <dbl>,
#> #   anc_identity_4 <dbl>, anc_correct_identity <dbl>,
#> #   app_identity_recorded <chr>, anc_identity_recorded <chr>,
#> #   app_identity_row_rnd <chr>, anc_identity_row_rnd <chr>, ranking <chr>
head(out$rankings)
#>   ranking  n    prop_obs     prop_bc   weights   prop_bc_raw prop_bc_adj
#> 1    1234 14 0.012939002 0.000000000 0.0000000 -0.0003526508 0.000000000
#> 2    1243 11 0.010166359 0.000000000 0.0000000 -0.0044081345 0.000000000
#> 3    1324 14 0.012939002 0.000000000 0.0000000 -0.0003526508 0.000000000
#> 4    1342  7 0.006469501 0.000000000 0.0000000 -0.0098154461 0.000000000
#> 5    1423 50 0.046210721 0.046944603 1.0158812  0.0483131539 0.048313154
#> 6    1432 20 0.018484288 0.007538549 0.4078355  0.0077583167 0.007758317
```
