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
lm_robust(app_identity_1 ~ 1, data = identity_w, weights = w) |> tidy()
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
