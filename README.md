# `rankingQ`: Design-Based Methods for Ranking Questions

`rankingQ` implements design-based methods for correcting measurement errors in ranking questions due to random responses. `rankingQ` allows users to estimate various ranking-based quantities of interest, such as average ranks and pairwise ranking probabilities. Moreover, `rankingQ` also enables parametric modeling, such as in the Placket-Luce model, by including bias correction weights. For the underlying methodology, see Atsusaks and Kim (2024) (<https://osf.io/preprints/osf/3ys8x>).

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

`rankingQ` has two primary functions to perform bias correction. First, `imprr_direct` improves ranking data by applying direct bias correction for four classes of quantities of interest, including

1.  average ranks
2.  pairwise ranking probabilities
3.  top-k ranking probabilities
4.  marginal ranking probabilities

Direct bias correction is available as follows:

``` r
main_direct <- imprr_direct(
  data = identity_data,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 1000
)
```

## References

Atsusaka, Y., & Kim, S.S. (2024). Addressing Measurement Errors in Ranking Questions for the Social Sciences. *Political Analysis* (conditionally accepted). <https://osf.io/preprints/osf/3ys8x>
