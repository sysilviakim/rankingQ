# 2. Data

In this vignette, we show what the typical input data look like for the
`rankingQ` package using the `identity` dataset.

`rankingQ` assumes a dataset that contains (1) responses to ranking
questions with `J` items and (2) a binary indicator for whether each
respondent provides the correct answer to an anchor-ranking question, an
auxiliary ranking question whose correct answer(s) are known to
researchers. For example, the package features a dataset `identity`,
which stores real-world survey responses to a question that asks 1,082
respondents to rank four sources of identity (partisanship, race,
gender, and religion) based on their relative importance to them.

``` r

library(rankingQ)
data("identity")
```

## Target ranking question

Ranking data are expected to be in the wide format, where multiple
columns are used to represent different items and their values represent
the items’ marginal ranks.

For example, in `identity`, the four sources of identity are stored in
`party`, `religion`, `gender`, and `race`. The first respondent ranked
party first, gender second, race third, and religion fourth, so the full
ranking profile `app_identity` is `"1423"` given the reference choice
set of party-religion-gender-race.

``` r

head(identity)
#> # A tibble: 6 × 16
#>   s_weight app_identity party religion gender  race anc_identity household
#>      <dbl> <chr>        <dbl>    <dbl>  <dbl> <dbl> <chr>            <dbl>
#> 1    0.844 1423             1        4      2     3 1234                 1
#> 2    0.886 1423             1        4      2     3 1234                 1
#> 3    2.96  3412             3        4      1     2 1234                 1
#> 4    0.987 1423             1        4      2     3 1234                 1
#> 5    1.76  4132             4        1      3     2 1324                 1
#> 6    0.469 3124             3        1      2     4 1234                 1
#> # ℹ 8 more variables: neighborhood <dbl>, city <dbl>, state <dbl>,
#> #   anc_correct_identity <dbl>, app_identity_recorded <chr>,
#> #   anc_identity_recorded <chr>, app_identity_row_rnd <chr>,
#> #   anc_identity_row_rnd <chr>
```

## Anchor ranking question

To perform bias correction, the data must have what we call the anchor
ranking question. The anchor question is an auxiliary ranking question
that looks similar to the target question, whose “correct” answer is
known to researchers. For example, `identity` has responses to the
anchor question that asked respondents to rank four nested units:
household, neighborhood, city, and state. These responses are included
in `household`, `neighborhood`, `city`, and `state`. Here, the correct
answer is assumed to be `"1234"`. Based on these responses, we code an
indicator variable (`anc_correct_identity`) that takes 1 if respondents
offer the correct answer and 0 otherwise.

In the following dataset, the first and third respondents have provided
an incorrect answer for the anchor question, whereas the rest have
provided the correct answer.

``` r

identity[
  ,
  c("household", "neighborhood", "city", "state", "anc_correct_identity")
] |>
  tail()
#> # A tibble: 6 × 5
#>   household neighborhood  city state anc_correct_identity
#>       <dbl>        <dbl> <dbl> <dbl>                <dbl>
#> 1         3            1     2     4                    0
#> 2         1            2     3     4                    1
#> 3         4            3     2     1                    0
#> 4         1            2     3     4                    1
#> 5         1            2     3     4                    1
#> 6         1            2     3     4                    1
```
