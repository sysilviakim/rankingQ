# `rankingQ`: Design-Based Methods for Improving Ranking Questions

`rankingQ` implements design-based methods for correcting measurement errors in ranking questions due to random responses. `rankingQ` allows users to estimate various ranking-based quantities of interest both non-parametrically and parametrically. `rankingQ` also offers practical tools for detecting the bias and assessing the anchor-ranking question.\
\
For the underlying methodology, see Atsusaka and Kim (2024) (<https://osf.io/preprints/osf/3ys8x>).

## Installation

`rankingQ` can be installed using the following code:

``` r
remotes::install_github(
  "sysilviakim/rankingQ",
  INSTALL_opts = c("--no-multiarch"),
  dependencies = TRUE
)
```

## Example

`rankingQ` assumes a dataset that contains (1) responses to ranking questions with `J` items and (2) a binary indicator for whether each respondent provides the correct answer to the anchor-ranking question---auxiliary ranking question whose correct answer(s) are known to researchers. For example, the package features a dataset `identity_ranking`, which stores responses to a question that asks respondents to rank four sources of identity (partisanship, race, gender, and religion) based on their relative importance.
``` r
library(rankingQ)

load("data/identity_ranking.Rda")
head(identity_ranking)

#   app_party app_religion app_gender app_race
# 1         1            4          2        3
# 2         1            4          2        3
# 3         3            4          1        2
# 4         1            4          2        3
# 5         4            1          3        2
# 6         3            1          2        4
#   anc_federal anc_state anc_municipal anc_school
# 1           1         2             3          4
# 2           1         2             3          4
# 3           1         2             3          4
# 4           1         2             3          4
# 5           1         3             2          4
# 6           1         2             3          4
#   anc_correct_identity  s_weight
# 1                    1 0.8439999
# 2                    1 0.8861603
# 3                    1 2.9644222
# 4                    1 0.9866697
# 5                    0 1.7573136
# 6                    1 0.4692241
```

### Target ranking question

Typically, ranking data are in the wide format, where multiple columns are used to represent different items and their values represent the items' marginal ranks. In `identity_ranking`, the four sources of identity are app_party, app_religion, app_gender, and app_race. For example, the first respondent ranked party first, gender second, race third, and religion fourth (i.e., "1423" is the respondent's outcome).

### Anchor ranking question

To perform bias correction, the data must have what we call the anchor ranking question. The anchor question is an auxiliary ranking question that looks similar to the target question, whose "correct" answer is known to researchers. For example, `identity_ranking` has responses to the anchor question that asked respondents to rank order four levels of government: federal, state, municipal, and school board. These responses are included in anc_federal, anc_state, anc_municipal, and anc_school. Here, the correct answer is assumed to be "1234." Based on theses responses, we code an indicator variable (anc_correct_identity) that takes 1 if respondents offer the correct answer and 0 otherwise.


## Direct Bias Correction via `imprr_direct`

`rankingQ` has two primary functions to perform bias correction. First, `imprr_direct` **impr**oves **r**anking data by applying **direct** bias correction to four classes of quantities of interest, including:

1.  average ranks
2.  pairwise ranking probabilities
3.  top-k ranking probabilities
4.  marginal ranking probabilities

To apply the bias correction, we specify our dataset (`data`), the number of items (`J`), the prefix of column names that contain `J` items for the target ranking questions, and the prefix of column names for the anchor ranking questions.

``` r

identity_ranking <- identity_ranking %>%
  rename(app_identity_1 = app_party,
         app_identity_2 = app_religion,
         app_identity_3 = app_gender,
         app_identity_4 = app_race)

out_direct <- imprr_direct(
  data = identity_ranking,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)
```

When survey weights are available, they can be included by specifying `weight` in the function.

## Weighting-Based Bias Correction via `imprr_weight`

Next, `imprr_weight` **impr**oves **r**anking data by computing bias correction **weights**, which can be used to correct for the bias in the inverse-probability weighting framework.

``` r

out_weight <- imprr_weight(
  data = identity_ranking,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)
```

The output of `imprr_weight` contains the set of weights for all possible ranking profiles with `J` items. For example, when `J = 3`, the set has `{123, 132, 213, 231, 312, 321}` and each profile now has an estimated weight.

To perform subsequent analyses, we merge the bias-correction weights to our original data. After this process, we can perform any analyses. For example, to study the distribution of unique ranking profiles, we can call `questionr::wtd.table()` with the estimated weights:

``` r
library(questionr)

identity_ranking <- identity_ranking %>%
  left_join(out_weight$weights, by = "ranking") 


wtd.table(
  x = identity_ranking$app_identity,
  weights = identity_ranking$weights
  )
```

## References

Atsusaka, Y., & Kim, S.S. (2024). Addressing Measurement Errors in Ranking Questions for the Social Sciences. *Political Analysis* (conditionally accepted). <https://osf.io/preprints/osf/3ys8x>
