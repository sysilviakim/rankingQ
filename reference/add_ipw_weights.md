# Add IPW Weights to the Original Data

This function is a thin convenience wrapper around
[`imprr_weights()`](https://sysilviakim.com/rankingQ/reference/imprr_weights.md)
for users who want respondent-level inverse probability weights attached
to the original data and do not necessarily need the full
ranking-profile output.

## Usage

``` r
add_ipw_weights(
  data,
  J = NULL,
  main_q,
  anc_correct = NULL,
  population = "non-random",
  assumption = "contaminated",
  weight = NULL,
  weight_col = "ipw_weights",
  keep_ranking = FALSE,
  ranking_col = "ranking",
  keep_rankings = FALSE,
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

  Ranking question to be analyzed. When `main_q` is a single column name
  or unquoted symbol such as `app_identity`, the function looks for
  `app_identity_1`, `app_identity_2`, `app_identity_3`, and so on. You
  may also supply `main_q` directly as a character vector or unquoted
  `c(...)` expression of ranking columns such as
  `c(party, gender, race, religion)`.

- anc_correct:

  Optional indicator for passing the anchor question. If `NULL`,
  `p_random` is used when supplied; otherwise the function defaults to
  `p_random = 0` and applies no correction.

- population:

  Choice of the target population out of non-random respondents
  (default) or all respondents.

- assumption:

  Choice of identifying assumption when `population = "all"`: `uniform`
  assumes random respondents would have uniform counterfactual
  preferences, while `contaminated` assumes their counterfactual
  preferences match those of non-random respondents.

- weight:

  Optional weight specification for the estimation step. This can be the
  name of a weight column in `data`, a numeric vector with one weight
  per row, or an unquoted column name. Defaults to `NULL`, which uses
  equal weights.

- weight_col:

  Name of the respondent-level IPW weight column to add to the returned
  data. Defaults to `"ipw_weights"`.

- keep_ranking:

  Logical; if `TRUE`, keep the unified ranking-profile column in the
  returned augmented data. Defaults to `FALSE`.

- ranking_col:

  Name of the unified ranking-profile column used in the augmented data
  and ranking summary. Defaults to `"ranking"`.

- keep_rankings:

  Logical; if `TRUE`, also return the permutation- level ranking summary
  and estimated random-response rate. Defaults to `FALSE`.

- p_random:

  Optional fixed proportion of random/inattentive respondents. When
  supplied, this overrides `anc_correct` and a message is shown if both
  are provided.

## Value

If `keep_rankings = FALSE`, a data frame equal to the original data
augmented with `weight_col`. If `keep_rankings = TRUE`, a list with
three elements:

- data:

  The augmented original data with respondent-level IPW weights.

- rankings:

  The permutation-level ranking summary returned by
  [`imprr_weights()`](https://sysilviakim.com/rankingQ/reference/imprr_weights.md).

- est_p_random:

  The estimated proportion of random responses.

## Examples

``` r
dat_w <- add_ipw_weights(
  identity,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)
#> No weight column supplied; using equal weights for all observations.
head(dat_w)
#> # A tibble: 6 × 17
#>   s_weight app_identity app_identity_1 app_identity_2 app_identity_3
#>      <dbl> <chr>                 <dbl>          <dbl>          <dbl>
#> 1    0.844 1423                      1              4              2
#> 2    0.886 1423                      1              4              2
#> 3    2.96  3412                      3              4              1
#> 4    0.987 1423                      1              4              2
#> 5    1.76  4132                      4              1              3
#> 6    0.469 3124                      3              1              2
#> # ℹ 12 more variables: app_identity_4 <dbl>, anc_identity <chr>,
#> #   anc_identity_1 <dbl>, anc_identity_2 <dbl>, anc_identity_3 <dbl>,
#> #   anc_identity_4 <dbl>, anc_correct_identity <dbl>,
#> #   app_identity_recorded <chr>, anc_identity_recorded <chr>,
#> #   app_identity_row_rnd <chr>, anc_identity_row_rnd <chr>, ipw_weights <dbl>

out <- add_ipw_weights(
  identity,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  keep_rankings = TRUE
)
#> No weight column supplied; using equal weights for all observations.
head(out$data)
#> # A tibble: 6 × 17
#>   s_weight app_identity app_identity_1 app_identity_2 app_identity_3
#>      <dbl> <chr>                 <dbl>          <dbl>          <dbl>
#> 1    0.844 1423                      1              4              2
#> 2    0.886 1423                      1              4              2
#> 3    2.96  3412                      3              4              1
#> 4    0.987 1423                      1              4              2
#> 5    1.76  4132                      4              1              3
#> 6    0.469 3124                      3              1              2
#> # ℹ 12 more variables: app_identity_4 <dbl>, anc_identity <chr>,
#> #   anc_identity_1 <dbl>, anc_identity_2 <dbl>, anc_identity_3 <dbl>,
#> #   anc_identity_4 <dbl>, anc_correct_identity <dbl>,
#> #   app_identity_recorded <chr>, anc_identity_recorded <chr>,
#> #   app_identity_row_rnd <chr>, anc_identity_row_rnd <chr>, ipw_weights <dbl>
head(out$rankings)
#>   ranking  n    prop_obs     prop_bc   weights   prop_bc_raw prop_bc_adj
#> 1    1234 14 0.012939002 0.000000000 0.0000000 -0.0003526508 0.000000000
#> 2    1243 11 0.010166359 0.000000000 0.0000000 -0.0044081345 0.000000000
#> 3    1324 14 0.012939002 0.000000000 0.0000000 -0.0003526508 0.000000000
#> 4    1342  7 0.006469501 0.000000000 0.0000000 -0.0098154461 0.000000000
#> 5    1423 50 0.046210721 0.046944603 1.0158812  0.0483131539 0.048313154
#> 6    1432 20 0.018484288 0.007538549 0.4078355  0.0077583167 0.007758317
```
