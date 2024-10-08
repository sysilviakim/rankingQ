# rankingQ

`rankingQ` implements design-based methods for correcting measurement errors in ranking questions due to random responses. `rankingQ` allows users to estimate various ranking-based quantities of interest, such as average ranks and pairwise ranking probabilities. Moreover, `rankingQ` also enables parametric modeling, such as in the Placket-Luce model, by including bias correction weights. For the underlying methodology, see Atsusaks and Kim (2024) "Addressing Measurement Errors in Ranking Questions for the Social Sciences" available at <https://osf.io/preprints/osf/3ys8x> (conditionally accepted at *Political Analysis*).

For installation, use the following code:

``` r
remotes::install_github(
  "sysilviakim/rankingQ",
  INSTALL_opts = c("--no-multiarch"),
  dependencies = TRUE
)
```
