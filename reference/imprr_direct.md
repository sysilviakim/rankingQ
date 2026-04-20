# Implements Plug-in Bias-Corrected Estimators for Ranking Data

This function implements the bias correction of the ranking distribution
using a paired anchor question.

## Usage

``` r
imprr_direct(
  data,
  J = NULL,
  main_q,
  anc_correct = NULL,
  population = "non-random",
  assumption = "contaminated",
  n_bootstrap = 200,
  seed = 123456,
  weight = NULL,
  verbose = FALSE,
  p_random = NULL
)
```

## Arguments

- data:

  The input dataset with ranking data.

- J:

  The number of items in the ranking question. Defaults to NULL, in
  which case it will be inferred from the data.

- main_q:

  Ranking question to be analyzed. When \`main_q\` is a single column
  name or unquoted symbol such as \`app_identity\`, the function looks
  for \`app_identity_1\`, \`app_identity_2\`, \`app_identity_3\`, and so
  on. You may also supply \`main_q\` directly as a character vector or
  unquoted \`c(...)\` expression of ranking columns such as \`c(party,
  gender, race, religion)\`.

- anc_correct:

  Optional indicator for passing the anchor question. If \`NULL\`,
  \`p_random\` is used when supplied; otherwise the function defaults to
  \`p_random = 0\` and applies no correction.

- population:

  Choice of the target population out of non-random respondents
  (default) or all respondents.

- assumption:

  Choice of identifying assumption when \`population = "all"\`:
  \`uniform\` assumes random respondents would have uniform
  counterfactual preferences, while \`contaminated\` assumes their
  counterfactual preferences match those of non-random respondents.

- n_bootstrap:

  Number of bootstraps. Defaults to 200.

- seed:

  Seed for `set.seed` for reproducibility.

- weight:

  The name of the weight column in \`data\`. Defaults to \`NULL\`, which
  uses equal weights. This can also be supplied as a numeric vector or
  as an unquoted column name.

- verbose:

  Indicator for verbose output. Defaults to FALSE.

- p_random:

  Optional fixed proportion of random/inattentive respondents. When
  supplied, this overrides \`anc_correct\` and a message is shown if
  both are provided.

## Value

A list with two elements:

- est_p_random:

  A data frame with summary statistics for the estimated proportion of
  random respondents, including columns `mean`, `lower`, and `upper`
  (95% confidence interval).

- results:

  A tibble with bias-corrected estimates grouped by `item`, `qoi`
  (quantity of interest), and `outcome`, including columns `mean`,
  `lower`, and `upper`.
