# Identity-ranking data with estimated weights based on inverse probability weighting

This data has two extra columns from the original \`identity\` data:
\`w\` and \`ranking\`, which are respectively the estimated weights
based on inverse probability weighting and the ranking pattern that the
respondent provided, united into a single column.

## Usage

``` r
identity_w
```

## Format

\## \`identity_w\` A data frame with 1,082 rows and 17 columns:

- w:

  Estimated weight based on inverse probability weighting.

- s_weight:

  Survey weight.

- app_identity:

  Full ranking profile for the main identity ranking question.

- app_identity_1:

  Marginal ranking for party (main identity ranking question).

- app_identity_2:

  Marginal ranking for religion (main identity ranking question).

- app_identity_3:

  Marginal ranking for gender (main identity ranking question).

- app_identity_4:

  Marginal ranking for race (main identity ranking question).

- anc_identity:

  Full ranking profile for the anchor ranking question.

- anc_identity_1:

  Marginal ranking for household (anchor question).

- anc_identity_2:

  Marginal ranking for neighborhood (anchor question).

- anc_identity_3:

  Marginal ranking for city (anchor question).

- anc_identity_4:

  Marginal ranking for state (anchor question).

- anc_correct_identity:

  Whether the respondent answered the anchor questions correctly. This
  is a binary variable that 1 if the respondent correctly answers the
  anchor ranking question and 0 if otherwise.

- app_identity_recorded:

  Recorded responses for the main identity ranking question.

- anc_identity_recorded:

  Recorded responses for the anchor ranking question.

- app_identity_row_rnd:

  The order in which the items were randomly presented for the
  respondent in the main ranking question.

- anc_identity_row_rnd:

  The order in which the items were randomly presented for the
  respondent in the anchor ranking question.

## Source

\<https://github.com/sysilviakim/ranking_error\>
