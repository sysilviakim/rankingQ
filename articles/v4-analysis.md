# 4. Analysis of Bias-corrected Ranking Data

``` r
library(rankingQ)
library(estimatr)
data(identity)
```

The IPW workflow is easiest to think of in two steps. First,
[`imprr_weights()`](https://sysilviakim.com/rankingQ/reference/imprr_weights.md)
estimates respondent-level correction weights. Those weights can then be
used for point estimation in downstream analyses.

``` r
out_weights <- imprr_weights(
  identity,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)
#> No weight column supplied; using equal weights for all observations.

out_weights$est_p_random
#> [1] 0.3163224
```

For example, to estimate the average rank of party as a point estimate,
one can leverage weighted linear regression as follows:

``` r
lm_robust(
  app_identity_1 ~ 1,
  data = out_weights$results,
  weights = out_weights$results$weights
) |>
  tidy()
#>          term estimate  std.error statistic p.value conf.low conf.high   df
#> 1 (Intercept) 3.220388 0.02790142  115.4202       0 3.165641  3.275135 1081
#>          outcome
#> 1 app_identity_1
```

That gives a valid point estimate, but its standard error treats the
estimated IPW weights as fixed. For built-in ranking summaries, use
[`imprr_weights_boot()`](https://sysilviakim.com/rankingQ/reference/imprr_weights_boot.md),
which resamples respondents, reruns
[`imprr_weights()`](https://sysilviakim.com/rankingQ/reference/imprr_weights.md)
inside each resample, and summarizes the resulting quantities of
interest.

## Computing Average Ranks

The `avg_rank` function remains a convenient way to compute point
estimates from the IPW-adjusted respondent-level data:

``` r
items_df <- data.frame(
  variable = paste0("app_identity_", 1:4),
  item = c("Party", "Religion", "Gender", "Race")
)

avg_rank(out_weights$results, items = items_df, weight = "weights", raw = FALSE)
#>       item          qoi     mean         se    lower    upper method
#> 1    Party Average Rank 3.220388 0.02790142 3.165641 3.275135    IPW
#> 2 Religion Average Rank 2.609023 0.04051924 2.529518 2.688529    IPW
#> 3   Gender Average Rank 1.706054 0.02448379 1.658013 1.754095    IPW
#> 4     Race Average Rank 2.464535 0.02616016 2.413204 2.515865    IPW
```

For bootstrap uncertainty on the same quantities,
[`imprr_weights_boot()`](https://sysilviakim.com/rankingQ/reference/imprr_weights_boot.md)
returns summaries in the same general format as
[`imprr_direct()`](https://sysilviakim.com/rankingQ/reference/imprr_direct.md):

``` r
out_boot <- imprr_weights_boot(
  identity,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 10,
  seed = 123
)
#> No weight column supplied; using equal weights for all observations.

out_boot$est_p_random
#>        mean     lower     upper
#> 1 0.3157438 0.2982159 0.3334646
subset(out_boot$results, qoi == "average rank")
#> # A tibble: 4 × 6
#>   item           qoi          outcome              mean lower upper
#>   <chr>          <chr>        <chr>               <dbl> <dbl> <dbl>
#> 1 app_identity_1 average rank Avg: app_identity_1  3.21  3.15  3.30
#> 2 app_identity_2 average rank Avg: app_identity_2  2.61  2.52  2.71
#> 3 app_identity_3 average rank Avg: app_identity_3  1.70  1.66  1.73
#> 4 app_identity_4 average rank Avg: app_identity_4  2.47  2.41  2.55
```

The same object also contains bootstrap summaries for pairwise, top-k,
and marginal ranking quantities:

``` r
subset(out_boot$results, qoi == "pairwise ranking")
#> # A tibble: 12 × 6
#>    item           qoi              outcome            mean lower upper
#>    <chr>          <chr>            <chr>             <dbl> <dbl> <dbl>
#>  1 app_identity_1 pairwise ranking v. app_identity_2 0.381 0.348 0.426
#>  2 app_identity_1 pairwise ranking v. app_identity_3 0.133 0.107 0.158
#>  3 app_identity_1 pairwise ranking v. app_identity_4 0.275 0.242 0.307
#>  4 app_identity_2 pairwise ranking v. app_identity_1 0.619 0.574 0.652
#>  5 app_identity_2 pairwise ranking v. app_identity_3 0.342 0.320 0.369
#>  6 app_identity_2 pairwise ranking v. app_identity_4 0.429 0.388 0.468
#>  7 app_identity_3 pairwise ranking v. app_identity_1 0.867 0.842 0.893
#>  8 app_identity_3 pairwise ranking v. app_identity_2 0.658 0.631 0.680
#>  9 app_identity_3 pairwise ranking v. app_identity_4 0.770 0.740 0.792
#> 10 app_identity_4 pairwise ranking v. app_identity_1 0.725 0.693 0.758
#> 11 app_identity_4 pairwise ranking v. app_identity_2 0.571 0.532 0.612
#> 12 app_identity_4 pairwise ranking v. app_identity_3 0.230 0.208 0.260
```

If you want uncertainty for an arbitrary downstream weighted analysis,
the same principle applies: resample respondents and rerun
[`imprr_weights()`](https://sysilviakim.com/rankingQ/reference/imprr_weights.md)
within each resample before recomputing the target estimand.
