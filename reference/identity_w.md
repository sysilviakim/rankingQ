# Identity-ranking data with estimated weights based on inverse probability weighting

This data is the \`results\` element returned by applying
\`imprr_weights()\` to \`identity\`. It adds two columns to the original
\`identity\` data: \`weights\`, the estimated inverse probability
weights, and \`ranking\`, the pasted full ranking profile.

## Usage

``` r
identity_w
```

## Format

\## \`identity_w\` A data frame with 1,082 rows and 18 columns:

- weights:

  Estimated weights based on inverse probability weighting.

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

- ranking:

  Pasted full ranking profile reconstructed from the marginal ranking
  columns. In this dataset, it matches `app_identity`.

## Source

\<https://github.com/sysilviakim/ranking_error\>
