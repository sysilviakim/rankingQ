# 1. Overview

This vignette introduces the main `rankingQ` workflow using the
`identity` dataset. The package estimates various ranking-based
quantities from ranking data. It also allows researchers to correct for
measurement error caused by inattentive survey respondents.

``` r

library(rankingQ)
library(dplyr)

data(identity)
```

## Example Data

The `identity` dataset contains Americans’ rankings about four sources
of identity and an anchor-ranking question with a known correct
ordering. The four items include political party, religion, gender, and
race. The key theoretical concept is *relative partisanship*—the extent
to which people prioritize partisanship over other sources of identity.

``` r

identity |> 
  rename(party = app_identity_1,
         religion = app_identity_2,
         gender = app_identity_3,
         race = app_identity_4) |>
  select(
    app_identity, 
    party, religion, gender, race
  ) |>
  head()
#> # A tibble: 6 × 5
#>   app_identity party religion gender  race
#>   <chr>        <dbl>    <dbl>  <dbl> <dbl>
#> 1 1423             1        4      2     3
#> 2 1423             1        4      2     3
#> 3 3412             3        4      1     2
#> 4 1423             1        4      2     3
#> 5 4132             4        1      3     2
#> 6 3124             3        1      2     4
```

It also includes the survey weight `s_weight`.

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

## Estimate Ranking-Based Quantities

We begin by estimating various ranking-based quantities with no bias
correction. We include survey weights via the `weight` argument.

The `imprr_direct` function **impr**ove **r**anking analysis directly by
estimating bias-corrected quantities of interest such as average ranks,
pairwise ranking probabilities, top-k probabilities, and marginal rank
probabilities.

``` r

out_direct <- imprr_direct(
  data = identity,
  J = 4,
  main_q = "app_identity",
  weight = "s_weight"
)
#> No anc_correct or p_random supplied; assuming everyone passes the anchor (p_random = 0), so no correction is applied.
```

The first output summarizes the estimated proportion of random
responses. As expected, no random response was detected (no bias
correction).

``` r

out_direct$est_p_random
#>   mean lower upper
#> 1    0     0     0
```

The second output contains several corrected ranking-based quantities of
interest.

``` r

out_direct$results
#> # A tibble: 44 × 6
#>    item           qoi              outcome              mean  lower upper
#>    <chr>          <chr>            <chr>               <dbl>  <dbl> <dbl>
#>  1 app_identity_1 average rank     Avg: app_identity_1 3.00  2.91   3.08 
#>  2 app_identity_1 marginal ranking Ranked 1            0.121 0.0953 0.153
#>  3 app_identity_1 marginal ranking Ranked 2            0.173 0.142  0.205
#>  4 app_identity_1 marginal ranking Ranked 3            0.289 0.258  0.330
#>  5 app_identity_1 marginal ranking Ranked 4            0.416 0.383  0.457
#>  6 app_identity_1 pairwise ranking v. app_identity_2   0.410 0.374  0.447
#>  7 app_identity_1 pairwise ranking v. app_identity_3   0.245 0.210  0.283
#>  8 app_identity_1 pairwise ranking v. app_identity_4   0.344 0.303  0.381
#>  9 app_identity_1 top-k ranking    Top-1               0.121 0.0953 0.153
#> 10 app_identity_1 top-k ranking    Top-2               0.295 0.257  0.331
#> # ℹ 34 more rows
```

## Direct Bias Correction

Now, we compute the above quantities by detecting random responses and
applying bias correction. The `imprr_direct` function takes another
argument `anc_correct`, which is a dummy variable that takes 1 if a
respondent has the right answer for the anchor question and 0 otherwise.

``` r

out_direct <- imprr_direct(
  data = identity,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = "s_weight"
)
```

Now, the function returns the estimated proportion of random responses.
We find that about 35\\ \[31%-40%\] of respondents—a sizable share of
data—seem to provide random responses.

``` r

out_direct$est_p_random
#>        mean     lower     upper
#> 1 0.3512825 0.3077923 0.3977214
```

Finally, we obtain bias-corrected estimates of various quantities of
interest. Here, we focus on average rank. The estimated average ranks
are based on our plug-in bias-corrected estimator.

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

Instead of directly correcting for bias, the `imprr_weights` function
produces respondent-level (bias-correction) weights that can be used in
downstream analyses.

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
#> 1    1234 0.4413785
#> 2    1243 0.0000000
#> 3    1324 0.3402731
#> 4    1342 0.0000000
#> 5    1423 0.9819455
#> 6    1432 0.2721408
```

The respondent-level output keeps the original data and appends a
`weights` column along with a unified `ranking` column. To combine our
bias-correction weights with survey weights, users can simply create a
new variable that multiplies both weights.

``` r

out_weights$results |>
  select(weights, s_weight, app_identity, ranking) |>
  mutate(joint_weight = weights * s_weight) |>
  head()
#> # A tibble: 6 × 5
#>   weights s_weight app_identity ranking joint_weight
#>     <dbl>    <dbl> <chr>        <chr>          <dbl>
#> 1   0.982    0.844 1423         1423           0.829
#> 2   0.982    0.886 1423         1423           0.870
#> 3   1.32     2.96  3412         3412           3.91 
#> 4   0.982    0.987 1423         1423           0.969
#> 5   1.14     1.76  4132         4132           2.01 
#> 6   0.966    0.469 3124         3124           0.453
```

## Using the IPW Weights

The IPW-adjusted respondent-level data can be passed to downstream
helpers such as `avg_rank`.

``` r

items_df <- data.frame(
  variable = paste0("app_identity_", 1:4),
  item = c("Party", "Religion", "Gender", "Race")
)

ipw_df <- out_weights$results |>
  mutate(joint_weight = weights * s_weight)

avg_rank(
  ipw_df,
  items = items_df,
  weight = "joint_weight",
  raw = FALSE
)
#>       item          qoi     mean         se    lower    upper method
#> 1    Party Average Rank 3.215044 0.03702728 3.142391 3.287698    IPW
#> 2 Religion Average Rank 2.596829 0.05518234 2.488553 2.705106    IPW
#> 3   Gender Average Rank 1.734215 0.03487726 1.665780 1.802650    IPW
#> 4     Race Average Rank 2.453911 0.03337011 2.388434 2.519389    IPW
```

## Next Steps

The remaining vignettes go into more detail on specific parts of the
workflow:

1.  `2. Data` describes our example dataset.
2.  `3. Methods` covers the correction methods in more depth.
3.  `4. Analysis` shows downstream analysis with corrected weights.
4.  `5. Visualization` introduces the plotting helpers.
5.  `6. Test` covers diagnostics when anchor questions are unavailable
    or need validation.
