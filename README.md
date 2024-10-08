# `rankingQ`: Design-Based Methods for Ranking Questions

`rankingQ` implements design-based methods for correcting measurement errors in ranking questions due to random responses. `rankingQ` allows users to estimate various ranking-based quantities of interest both non-parametrically and parametrically. `rankingQ` also offers several for detecting the bias and assessing the anchor-ranking question.\
\
For the underlying methodology, see Atsusaks and Kim (2024) (<https://osf.io/preprints/osf/3ys8x>).

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

`rankingQ` assumes a dataset that contains (1) responses to ranking questions with `J` items and (2) a binary indicator for whether each respondent provides the correct answer to the anchor-ranking question---auxiliary ranking question whose correct answer(s) are known to researchers. For example, the package features a dataset `identity_ranking`, which store responses to a question that asks respondents to rank four sources of identity (partisanship, race, gender, and religion) based on their relative importance.

``` r
library(rankingQ)

data(identity_ranking)
head(identity_ranking)
```

## Direct Bias Correction via `imprr_direct`

`rankingQ` has two primary functions to perform bias correction. First, `imprr_direct` [**impr**]{.underline}oves [**r**]{.underline}anking data by applying [**direct**]{.underline} bias correction to four classes of quantities of interest, including:

1.  average ranks
2.  pairwise ranking probabilities
3.  top-k ranking probabilities
4.  marginal ranking probabilities

To apply the bias correction, we specify our dataset (`data`), the number of items (`J`), the prefix of column names that contain `J` items for the target ranking questions, and the prefix of column names for the anchor ranking questions.

``` r

out_direct <- imprr_direct(
  data = identity_ranking,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)
```

When survey weights are available, they can be included by specifying `weight` in the function.

## Weighting-Based Bias Correction via `imprr_weight`

Next, `imprr_weight` [**imp**]{.underline}roves [**r**]{.underline}anking data by computing bias correction [**weights**]{.underline}, which can be used to correct for the bias in the inverse-probability weighting framework.

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
