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
  seed = 123456,
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

  Choice of the identifying assumption if \`population\` is set to all

- seed:

  Seed for `set.seed` for reproducibility.

- weight:

  A vector of weights. Defaults to NULL.

- ranking:

  The name of the column that will store the full ranking profile.
  Defaults to "ranking". If \`main_q\` exists in the data, the produced
  column should be identical to \`main_q\`. However, the function
  defaults to creating another column by combining marginal rankings,
  just in case.

## Value

A list.
