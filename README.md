# `rankingQ`: Design-Based Methods for Improving Ranking Questions <img src="man/figures/logo.png" align="right" width="150"/>

`rankingQ` implements design-based methods for correcting measurement errors in ranking questions due to random responses. `rankingQ` allows users to estimate various ranking-based quantities of interest both non-parametrically and parametrically. `rankingQ` also offers practical tools for detecting the bias and assessing the anchor-ranking question.\
\
For the underlying methodology, see [Atsusaka and Kim (2025)](https://doi.org/10.1017/pan.2024.33). "Addressing Measurement Errors in Ranking Questions for the Social Sciences." *Political Analysis* Volume 33 , Issue 4 , October 2025 , pp. 339 - 360. Please visit the [package site](https://sysilviakim.com/rankingQ/) for all vignettes and references.

## Installation

`rankingQ` can be installed using the following code:

``` r
remotes::install_github(
  "sysilviakim/rankingQ",
  dependencies = TRUE
)
```

## Example

The following is a simplified example of how this package can be used. For a full demonstration, visit <https://sysilviakim.com/rankingQ/>.

``` r
data("identity")

head(identity)
# # A tibble: 6 × 16
#   s_weight app_identity app_identity_1 app_identity_2 app_identity_3 app_identity_4
#      <dbl> <chr>                 <dbl>          <dbl>          <dbl>          <dbl>
# 1    0.844 1423                      1              4              2              3
# 2    0.886 1423                      1              4              2              3
# 3    2.96  3412                      3              4              1              2
# 4    0.987 1423                      1              4              2              3
# 5    1.76  4132                      4              1              3              2
# 6    0.469 3124                      3              1              2              4


# Perform bias correction via plug-in estimator
out_direct <- imprr_direct(
  data = identity,
  J = 4, 
  main_q = "app_identity", 
  anc_correct = "anc_correct_identity"
)

out_direct$results
# # A tibble: 44 × 6
# # Groups:   item, qoi [16]
#    item           qoi            outcome   mean  lower  upper
#    <chr>          <chr>          <chr>    <dbl>  <dbl>  <dbl>
#  1 app_identity_1 average rank   Avg: a… 3.27   3.24   3.33  
#  2 app_identity_1 marginal rank… Ranked… 0.0407 0.0232 0.0496
#  3 app_identity_1 marginal rank… Ranked… 0.150  0.137  0.163 
#  4 app_identity_1 marginal rank… Ranked… 0.305  0.275  0.336 
#  5 app_identity_1 marginal rank… Ranked… 0.504  0.475  0.541 
#  6 app_identity_1 pairwise rank… v. app… 0.357  0.333  0.374 
#  7 app_identity_1 pairwise rank… v. app… 0.108  0.0739 0.136 
#  8 app_identity_1 pairwise rank… v. app… 0.262  0.238  0.284 
#  9 app_identity_1 top-k ranking  Top-1   0.0407 0.0232 0.0496
# 10 app_identity_1 top-k ranking  Top-2   0.306  0.291  0.339


# Perform bias correction via IPW
out_weights <- imprr_weights(
  data = identity,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)

head(out_weights$rankings)
#   ranking  n    prop_obs     prop_bc   weights   prop_bc_raw prop_bc_adj
# 1    1234 14 0.012939002 0.000000000 0.0000000 -0.0003526508 0.000000000
# 2    1243 11 0.010166359 0.000000000 0.0000000 -0.0044081345 0.000000000
# 3    1324 14 0.012939002 0.000000000 0.0000000 -0.0003526508 0.000000000
# 4    1342  7 0.006469501 0.000000000 0.0000000 -0.0098154461 0.000000000
# 5    1423 50 0.046210721 0.046944603 1.0158812  0.0483131539 0.048313154
# 6    1432 20 0.018484288 0.007538549 0.4078355  0.0077583167 0.007758317
```
