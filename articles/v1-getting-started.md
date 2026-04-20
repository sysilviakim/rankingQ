# 1. Getting Started

This vignette introduces the main `rankingQ` workflow using the
`identity` dataset. The package is designed for ranking questions that
may contain random responses and uses an anchor-ranking question to
estimate and correct the resulting measurement error.

``` r
library(rankingQ)
library(dplyr)

data(identity)
```

## The Example Data

The `identity` dataset contains a main ranking question about four
sources of identity and an anchor-ranking question with a known correct
ordering. It also includes the survey weight `s_weight`.

``` r
identity |>
  select(
    s_weight,
    app_identity,
    starts_with("app_identity_"),
    anc_correct_identity
  ) |>
  head()
#> # A tibble: 6 × 9
#>   s_weight app_identity app_identity_1 app_identity_2 app_identity_3
#>      <dbl> <chr>                 <dbl>          <dbl>          <dbl>
#> 1    0.844 1423                      1              4              2
#> 2    0.886 1423                      1              4              2
#> 3    2.96  3412                      3              4              1
#> 4    0.987 1423                      1              4              2
#> 5    1.76  4132                      4              1              3
#> 6    0.469 3124                      3              1              2
#> # ℹ 4 more variables: app_identity_4 <dbl>, app_identity_recorded <chr>,
#> #   app_identity_row_rnd <chr>, anc_correct_identity <dbl>
```

The `app_identity` columns describe the ranking question of interest,
while `anc_correct_identity` indicates whether each respondent answered
the anchor question correctly.

## Direct Bias Correction

The `imprr_direct` function directly estimates bias-corrected quantities
of interest such as average ranks, pairwise ranking probabilities, top-k
probabilities, and marginal rank probabilities.

``` r
out_direct <- imprr_direct(
  data = identity,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = "s_weight"
)
```

The first output summarizes the estimated proportion of random
responses.

``` r
out_direct$est_p_random
#>        mean     lower     upper
#> 1 0.3512825 0.3077923 0.3977214
```

The second output contains several corrected ranking-based quantities of
interest. For a first pass, average ranks are often the easiest place to
start.

``` r
out_direct$results |>
  filter(qoi == "average rank")
#> # A tibble: 4 × 6
#>   item           qoi          outcome              mean lower upper
#>   <chr>          <chr>        <chr>               <dbl> <dbl> <dbl>
#> 1 app_identity_1 average rank Avg: app_identity_1  3.27  3.13  3.40
#> 2 app_identity_2 average rank Avg: app_identity_2  2.58  2.43  2.75
#> 3 app_identity_3 average rank Avg: app_identity_3  1.66  1.52  1.81
#> 4 app_identity_4 average rank Avg: app_identity_4  2.49  2.37  2.59
```

## Inverse-Probability Weighting

The `imprr_weights` function instead produces respondent-level weights
that can be used in downstream analyses.

``` r
out_weights <- imprr_weights(
  data = identity,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = "s_weight"
)
```

One output gives the bias-correction weight assigned to each possible
ranking profile.

``` r
out_weights$rankings |>
  select(ranking, weights) |>
  head()
#>   ranking   weights
#> 1    1234 0.5275266
#> 2    1243 0.0000000
#> 3    1324 0.4410833
#> 4    1342 0.0000000
#> 5    1423 0.9897016
#> 6    1432 0.3828315
```

The respondent-level output keeps the original data and appends a
`weights` column along with a unified `ranking` column.

``` r
out_weights$results |>
  select(weights, s_weight, app_identity, ranking) |>
  head()
#> # A tibble: 6 × 4
#>   weights s_weight app_identity ranking
#>     <dbl>    <dbl> <chr>        <chr>  
#> 1   0.990    0.844 1423         1423   
#> 2   0.990    0.886 1423         1423   
#> 3   1.28     2.96  3412         3412   
#> 4   0.990    0.987 1423         1423   
#> 5   1.13     1.76  4132         4132   
#> 6   0.976    0.469 3124         3124
```

## Using the IPW Weights

The IPW-adjusted respondent-level data can be passed to downstream
helpers such as `avg_rank`.

``` r
items_df <- data.frame(
  variable = paste0("app_identity_", 1:4),
  item = c("Party", "Religion", "Gender", "Race")
)

avg_rank(
  out_weights$results,
  items = items_df,
  weight = "weights",
  raw = FALSE
)
#> Joining with `by = join_by(variable)`
#>       item          qoi     mean         se    lower    upper method
#> 1    Party Average Rank 3.203496 0.02782939 3.148891 3.258102    IPW
#> 2 Religion Average Rank 2.597869 0.04029251 2.518809 2.676930    IPW
#> 3   Gender Average Rank 1.726510 0.02499470 1.677466 1.775554    IPW
#> 4     Race Average Rank 2.472124 0.02569631 2.421704 2.522544    IPW
```

## Next Steps

The remaining vignettes go into more detail on specific parts of the
workflow:

1.  `2. Basic Setup` describes the expected input data structure.
2.  `3. Correcting Bias in Ranking Data` covers the correction methods
    in more depth.
3.  `4. Analysis of Bias-corrected Ranking Data` shows downstream
    analysis with corrected weights.
4.  `5. Visualizing Rankings` introduces the plotting helpers.
5.  `6. Uniformity Tests` covers diagnostics when anchor questions are
    unavailable or need validation.
6.  `7. Simulating Ranking Data` describes the data-generation helper
    `rpluce`.
