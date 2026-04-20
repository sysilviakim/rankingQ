# Implements Plug-in Bias-Corrected Estimators for Ranking Data

This function implements the bias correction of the ranking distribution
using a paired anchor question.

## Usage

``` r
imprr_direct(
  data,
  J = NULL,
  main_q,
  anc_correct,
  population = "non-random",
  assumption = "contaminated",
  n_bootstrap = 200,
  seed = 123456,
  weight = NULL,
  verbose = FALSE
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

- n_bootstrap:

  Number of bootstraps. Defaults to 200.

- seed:

  Seed for `set.seed` for reproducibility.

- weight:

  The name of the weight column in `data`. Defaults to `NULL`, which
  uses equal weights.

- verbose:

  Indicator for verbose output. Defaults to FALSE.

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
