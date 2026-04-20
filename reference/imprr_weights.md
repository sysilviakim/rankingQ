# Computes Bias-Correction Weights for Ranking Data

This function implements the bias correction of the ranking distribution
using a paired anchor question, using the IPW estimator.

## Usage

``` r
imprr_weights(
  data,
  J = NULL,
  main_q,
  anc_correct,
  population = "non-random",
  assumption = "contaminated",
  weight = NULL,
  ranking = "ranking"
)
```

## Arguments

- data:

  The input dataset with ranking data.

- J:

  The number of items in the ranking question. Defaults to NULL, in
  which case it will be inferred from the data, only if the column for
  \`main_q\` exists in the data.

- main_q:

  Column name for the main ranking question to be analyzed. Using this
  argument, the function automatically looks for columns with marginal
  rankings. For example, if \`main_q\` is \`app_identity\`, the function
  looks for \`app_identity_1\`, \`app_identity_2\`, \`app_identity_3\`,
  and so on, with an underbar separator followed by numbers.

- anc_correct:

  Indicator for passing the anchor question.

- population:

  Choice of the target population out of non-random respondents
  (default) or all respondents.

- assumption:

  Choice of identifying assumption when \`population = "all"\`:
  \`uniform\` assumes random respondents would have uniform
  counterfactual preferences, while \`contaminated\` assumes their
  counterfactual preferences match those of non-random respondents.

- weight:

  The name of the weight column in \`data\`. Defaults to \`NULL\`, which
  uses equal weights.

- ranking:

  The name of the column that will store the full ranking profile.
  Defaults to "ranking". If \`main_q\` exists in the data, the produced
  column should be identical to \`main_q\`. However, the function
  defaults to creating another column by combining marginal rankings,
  just in case.

## Value

A list with three elements:

- est_p_random:

  A numeric value representing the estimated proportion of random
  responses.

- results:

  A data frame with the original data augmented with a `weights` column
  containing inverse probability weights and a `ranking` column with
  unified ranking patterns.

- rankings:

  A data frame with ranking patterns, observed proportions (`prop_obs`),
  bias-corrected proportions (`prop_bc`), and inverse probability
  weights (`weights`) for each permutation.
