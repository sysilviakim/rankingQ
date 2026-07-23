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

The `identity` dataset contains data on how Americans rank four sources
of identity that are central to American politics. The four items
include political party, religion, gender, and race. The key theoretical
concept is *relative partisanship*—the extent to which people prioritize
partisanship over other sources of identity.  
  
Below, the `app_identity` column stores the full ranking profile, the
item columns (`party`, `religion`, `gender`, `race`) store the marginal
ranks.

``` r

identity |>
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

It also includes the survey weight `s_weight`. Additionally, the dataset
includes the binary variable for whether respondents provide the correct
answer to the anchor ranking question (see Data).

``` r

identity |>
  select(
    app_identity,
    party, religion, gender, race,
    s_weight,
    anc_correct_identity
  ) |>
  head()
#> # A tibble: 6 × 7
#>   app_identity party religion gender  race s_weight anc_correct_identity
#>   <chr>        <dbl>    <dbl>  <dbl> <dbl>    <dbl>                <dbl>
#> 1 1423             1        4      2     3    0.844                    1
#> 2 1423             1        4      2     3    0.886                    1
#> 3 3412             3        4      1     2    2.96                     1
#> 4 1423             1        4      2     3    0.987                    1
#> 5 4132             4        1      3     2    1.76                     0
#> 6 3124             3        1      2     4    0.469                    1
```

Here, `anc_correct_identity` indicates whether each respondent answered
the anchor question correctly.

Substantively, [Atsusaka and Kim
(2025)](https://doi.org/10.1017/pan.2024.33) are interested in the
extent to which *political party* is important when it comes to people’s
multidimensional identity.

## Estimate Ranking-Based Quantities

Now, let us demonstrate how to compute various ranking-based quantities
based on the data. We begin by estimating such quantities with no bias
correction. To make it realistic, however, we include survey weights via
the `weight` argument.

The `imprr_direct` function **impr**ove **r**anking analysis
**direct**ly (as in a plug-in way) by estimating bias-corrected
quantities of interest such as average ranks, pairwise ranking
probabilities, top-k probabilities, and marginal rank probabilities.
Here, `main_q` argument takes a vector of all items in the choice set.

``` r

out_direct <- imprr_direct(
  data = identity,
  J = 4,
  main_q = c("party", "religion", "gender", "race"),
  weight = "s_weight"
)
#> No anc_correct or p_random supplied; assuming everyone passes the anchor (p_random = 0), so no correction is applied.
```

`imprr_direct` returns two lists as an output.  
  
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
#>    item   qoi              outcome       mean  lower upper
#>    <chr>  <chr>            <chr>        <dbl>  <dbl> <dbl>
#>  1 gender average rank     Avg: gender 1.96   1.88   2.05 
#>  2 gender marginal ranking Ranked 1    0.408  0.364  0.444
#>  3 gender marginal ranking Ranked 2    0.324  0.286  0.365
#>  4 gender marginal ranking Ranked 3    0.173  0.144  0.206
#>  5 gender marginal ranking Ranked 4    0.0948 0.0748 0.121
#>  6 gender pairwise ranking v. party    0.755  0.717  0.790
#>  7 gender pairwise ranking v. race     0.682  0.642  0.724
#>  8 gender pairwise ranking v. religion 0.608  0.566  0.651
#>  9 gender top-k ranking    Top-1       0.408  0.364  0.444
#> 10 gender top-k ranking    Top-2       0.732  0.688  0.769
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
  main_q = c("party", "religion", "gender", "race"),
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
#>   item     qoi          outcome        mean lower upper
#>   <chr>    <chr>        <chr>         <dbl> <dbl> <dbl>
#> 1 gender   average rank Avg: gender    1.66  1.52  1.81
#> 2 party    average rank Avg: party     3.27  3.13  3.40
#> 3 race     average rank Avg: race      2.49  2.37  2.59
#> 4 religion average rank Avg: religion  2.58  2.43  2.75
```

## Inverse-Probability Weighting

Instead of directly correcting for bias, the `imprr_weights` function
produces respondent-level (bias-correction) weights that can be used in
downstream analyses.

``` r

out_weights <- imprr_weights(
  data = identity,
  J = 4,
  main_q = c("party", "religion", "gender", "race"),
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
  variable = c("party", "religion", "gender", "race"),
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
