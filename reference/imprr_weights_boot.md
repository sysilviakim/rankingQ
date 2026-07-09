# Bootstrap IPW-Based Bias-Corrected Estimates for Ranking Data

This function repeatedly resamples respondents, reruns
[`imprr_weights()`](https://sysilviakim.com/rankingQ/reference/imprr_weights.md),
and summarizes downstream quantities of interest such as average ranks,
pairwise probabilities, top-k probabilities, and marginal rank
probabilities. It provides bootstrap uncertainty estimates for the IPW
workflow in a format parallel to
[`imprr_direct()`](https://sysilviakim.com/rankingQ/reference/imprr_direct.md).

## Usage

``` r
imprr_weights_boot(
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
  name or unquoted symbol such as \`my_ranking\`, the function looks for
  \`my_ranking_1\`, \`my_ranking_2\`, \`my_ranking_3\`, and so on. You
  may also supply \`main_q\` directly as a character vector or unquoted
  \`c(...)\` expression of ranking columns such as \`c(party, religion,
  gender, race)\`.

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

  Number of bootstrap resamples. Defaults to 200.

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

  A data frame with summary statistics for the estimated proportion of
  random respondents, including columns `mean`, `lower`, and `upper`
  (95% confidence interval).

- results:

  A data frame with bootstrap summaries for the IPW-based bias-corrected
  quantities of interest, grouped by `item`, `qoi`, and `outcome`, with
  columns `mean`, `lower`, and `upper`.

## Examples

``` r
out <- imprr_weights_boot(
  identity,
  main_q = c("party", "religion", "gender", "race"),
  anc_correct = "anc_correct_identity",
  n_bootstrap = 2,
  seed = 123
)
#> No weight column supplied; using equal weights for all observations.
out$est_p_random
#>        mean     lower     upper
#> 1 0.3172868 0.3108736 0.3237001
head(out$results)
#> # A tibble: 6 × 6
#>   item   qoi              outcome       mean  lower  upper
#>   <chr>  <chr>            <chr>        <dbl>  <dbl>  <dbl>
#> 1 gender average rank     Avg: gender 1.70   1.69   1.70  
#> 2 gender marginal ranking Ranked 1    0.490  0.490  0.491 
#> 3 gender marginal ranking Ranked 2    0.359  0.359  0.359 
#> 4 gender marginal ranking Ranked 3    0.113  0.111  0.115 
#> 5 gender marginal ranking Ranked 4    0.0378 0.0350 0.0406
#> 6 gender pairwise ranking v. party    0.879  0.877  0.882 
```
