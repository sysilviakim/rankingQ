# 1. Overview

This vignette introduces the main `rankingQ` workflow using the
`identity` dataset. The package estimates various ranking-based
quantities from ranking data. It also allows researchers to correct for
measurement error caused by inattentive survey respondents.

``` r

library(rankingQ)
library(dplyr)
```

## Example Data

The `identity` dataset contains data on how Americans rank four sources
of identity that are central to American politics. The four items
include political party, religion, gender, and race. The key theoretical
concept is *relative partisanship*—the extent to which people prioritize
partisanship over other sources of identity.  
  
Below, the `app_identity` column stores the full ranking profile.
Similarly, the item columns (`party`, `religion`, `gender`, `race`)
store the marginal ranks.

``` r

data(identity)

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

It also includes the survey weight `s_weight`. Additionally,
`anc_correct_identity` is the binary variable for whether respondents
provide the correct answer to the anchor ranking question.

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

Substantively, [Atsusaka and Kim
(2025)](https://doi.org/10.1017/pan.2024.33) are interested in the
extent to which *political party* is important when it comes to people’s
multidimensional identity.

## Estimate Ranking-Based Quantities

Now, let us demonstrate how to compute various ranking-based quantities
based on the data. We begin by estimating such quantities with no bias
correction. To make it realistic, however, we include survey weights via
the `weight` argument.

### Input

The `imprr_direct` function **impr**ove **r**anking analysis
**direct**ly (as in a plug-in way) by estimating bias-corrected
quantities of interest. These quantities include average ranks, pairwise
ranking probabilities, top-k probabilities, and marginal rank
probabilities.

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

### Output

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

Researchers can examine any classes of ranking-based quantities. For
example, they can filer out the results by focusing on top-k ranking
probabilities:

``` r

out_direct$results |>
  filter(qoi == "top-k ranking")
#> # A tibble: 12 × 6
#>    item     qoi           outcome  mean  lower upper
#>    <chr>    <chr>         <chr>   <dbl>  <dbl> <dbl>
#>  1 gender   top-k ranking Top-1   0.408 0.364  0.444
#>  2 gender   top-k ranking Top-2   0.732 0.688  0.769
#>  3 gender   top-k ranking Top-3   0.905 0.879  0.925
#>  4 party    top-k ranking Top-1   0.121 0.0953 0.153
#>  5 party    top-k ranking Top-2   0.295 0.257  0.331
#>  6 party    top-k ranking Top-3   0.584 0.543  0.617
#>  7 race     top-k ranking Top-1   0.156 0.131  0.184
#>  8 race     top-k ranking Top-2   0.520 0.487  0.560
#>  9 race     top-k ranking Top-3   0.834 0.804  0.857
#> 10 religion top-k ranking Top-1   0.315 0.274  0.355
#> 11 religion top-k ranking Top-2   0.454 0.415  0.491
#> 12 religion top-k ranking Top-3   0.678 0.640  0.716
```

## Apply Bias Correction

Now, suppose that we are concerned that the original data contain random
responses or satisficing answers. We worry that such responses would
introduce measurement error to our data.

The `rankingQ` package offers two ways to address such concern. Both
approaches allow us to compute bias-corrected estimates of our
quantities of interest.

### Plug-in Bias-corrected Estimator

The first approach is to account for the proportion of random responses
and directly bias correct our estimates.

To estimate the proportion of random responses, [Atsusaka and Kim
(2025)](https://doi.org/10.1017/pan.2024.33) advocated using an anchor
ranking question: an auxiliary ranking question whose correct answer is
known to researchers and respondents. To precisely measure the level of
satisficing responses in the target ranking question, we recommend that
researchers ask the anchor question right before or after the primary
ranking question.

The `imprr_direct` function takes an additional argument `anc_correct`,
which is a dummy variable that takes 1 if a respondent has the right
answer for the anchor question and 0 otherwise.

``` r

out_direct <- imprr_direct(
  data = identity,
  J = 4,
  main_q = c("party", "religion", "gender", "race"),
  weight = "s_weight",
  anc_correct = "anc_correct_identity" # additional input
)
```

In this example, the function returns the estimated proportion of random
responses. We find that about 35\\ \[31%-40%\] of respondents—a sizable
share of data—seem to provide random responses.

``` r

out_direct$est_p_random
#>        mean     lower     upper
#> 1 0.3512825 0.3077923 0.3977214
```

Our bias-corrected estimates are available in `results`. Here, we focus
on average rank. Again, the estimated average ranks are based on our
plug-in bias-corrected estimator.

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

#### Using Attention Checks or Other Methods

In some applications, random responses may also be detected by other
methods, including attention checks, screener questions, factual
manipulation checks, and response time.

**Our package can accommodate these alternative methods with no
problems.**

The only change is to use an alternative argument `p_random` and specify
the estimated proportion of random responses directly.

``` r

out_alternative <- imprr_direct(
  data = identity,
  J = 4,
  main_q = c("party", "religion", "gender", "race"),
  weight = "s_weight",
  p_random = 0.5 # estimated proportion of random responses
)
```

By definition, `est_p_random` returns the input value:

``` r

out_alternative$est_p_random
#>   mean lower upper
#> 1  0.5   0.5   0.5
```

The output format stays the same as before.

``` r

out_alternative$results |>
  filter(qoi == "average rank")
#> # A tibble: 4 × 6
#>   item     qoi          outcome        mean lower upper
#>   <chr>    <chr>        <chr>         <dbl> <dbl> <dbl>
#> 1 gender   average rank Avg: gender    1.41  1.25  1.61
#> 2 party    average rank Avg: party     3.50  3.33  3.66
#> 3 race     average rank Avg: race      2.48  2.33  2.61
#> 4 religion average rank Avg: religion  2.61  2.41  2.83
```

### Inverse-Probability Weighting

The second approach is to estimate bias-correction weights and use the
weights in downstream analyses.

The key idea is that some rankings are oversampled and others are
undersampled due to measurement error. Thus, for rankings that are
artificially overrepresented, we want to down weight them. For rankings
that are underrepresented, we want to weight them up.

This is known as the inverse-probability weighting.

For this strategy, the `imprr_weights` function allows us to estimate
bias-correction weights for each survey respondent.

``` r

out_weights <- imprr_weights(
  data = identity,
  J = 4,
  main_q = c("party", "religion", "gender", "race"),
  weight = "s_weight",
  anc_correct = "anc_correct_identity", # additional input
)
```

What this approach does is to assign a bias-corrected weight to each
possible ranking profile. The `rankings` list contains the comprehensive
list of rankings with bias-correction weights.

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
`weights` column along with a unified `ranking` column.

In many cases, we wish to account for two types of weights: survey
weights and bias-correction weights. We can do so by multiplying both
weights to create a single (joint) weight variable. To do so, users can
simply create a new variable that multiplies both weights.

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

The IPW-adjusted respondent-level data can be passed to any downstreatm
analyses, including descriptive and regression analyses.

We demonstrate it by using our in-house helper function `avg_rank`.

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
