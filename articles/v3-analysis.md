# 3. Analysis of Bias-corrected Ranking Data

``` r
library(rankingQ)
library(estimatr)
data(identity_w)
```

The estimated weights from `imprr_weights` can be used to perform any
analyses. For example, to estimate the average rank of party, one can
leverage linear regression as follows:

``` r
lm_robust(
  app_identity_1 ~ 1,
  data = identity_w,
  weights = identity_w$weights
) |>
  tidy()
#>          term estimate  std.error statistic p.value conf.low conf.high   df
#> 1 (Intercept) 3.220388 0.02790142  115.4202       0 3.165641  3.275135 1081
#>          outcome
#> 1 app_identity_1
```

While this illustrative example provides a valid point estimate, its
confidence interval does not account for the estimation uncertainty
around the estimated weights. Thus, in practice, `imprr_weights` must be
used along with bootstrapping, such as the one available in `rsample`
([example](https://declaredesign.org/r/estimatr/articles/estimatr-in-the-tidyverse.html#bootstrap-using-rsample)).

## Computing Average Ranks

The `avg_rank` function provides a convenient way to compute average
ranks for all items:

``` r
# Raw average ranks (without bias correction)
avg_rank(identity_w,
  rankings = "app_identity",
  items = c("Party", "Religion", "Gender", "Race")
)
#>       item          qoi     mean         se    lower    upper   method
#> 1    Party Average Rank 3.024954 0.03094894 2.964294 3.085614 Raw Data
#> 2 Religion Average Rank 2.572089 0.03745515 2.498677 2.645501 Raw Data
#> 3   Gender Average Rank 1.912200 0.02922013 1.854928 1.969471 Raw Data
#> 4     Race Average Rank 2.490758 0.02883352 2.434244 2.547272 Raw Data
```

For bias-corrected estimates using IPW weights, use the marginal ranking
columns:

``` r
# IPW-corrected average ranks
items_df <- data.frame(
  variable = paste0("app_identity_", 1:4),
  item = c("Party", "Religion", "Gender", "Race")
)
avg_rank(identity_w, items = items_df, weight = "weights", raw = FALSE)
#> Joining with `by = join_by(variable)`
#>       item          qoi     mean         se    lower    upper method
#> 1    Party Average Rank 3.220388 0.02790142 3.165641 3.275135    IPW
#> 2 Religion Average Rank 2.609023 0.04051924 2.529518 2.688529    IPW
#> 3   Gender Average Rank 1.706054 0.02448379 1.658013 1.754095    IPW
#> 4     Race Average Rank 2.464535 0.02616016 2.413204 2.515865    IPW
```
