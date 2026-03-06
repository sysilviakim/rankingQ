# Identity-ranking data analyzed in Atsusaka and Kim (2024)

Full dataset from survey on relative partisanship used in Atsusaka,
Yuki, & Kim, Seo-young Silvia (2024). Addressing Measurement Errors in
Ranking Questions for the Social Sciences. Political Analysis
(conditionally accepted). https://osf.io/preprints/osf/3ys8x

This data contains Americans' rankings of four sources of their
identity, including political party, religion, gender, and race, for
1,082 respondents. The columns consist of marginal rankings to the main
identity ranking question and the corresponding anchor question, as well
as whether they have answered the anchor questions "correctly." The
anchor ranking question is used to estimate the proportion of random
responses and to correct for measurement error bias.

## Usage

``` r
identity
```

## Format

\## \`identity\` A data frame with 1,082 rows and 16 columns:

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

- s_weight:

  Survey weight.

## Source

\<https://github.com/sysilviakim/ranking_error\>
