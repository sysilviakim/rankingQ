# rankingq

This package maintains a set of functions to handle measurement errors in ranking (rank-order) questions. More specifically, it focuses on producing quantities of interest from ranking survey questions after correcting for bias from random responses. For the underlying methodology, see preprint at [https://osf.io/preprints/osf/3ys8x](https://osf.io/preprints/osf/3ys8x) (conditionally accepted at *Poliitcal Analysis*).

For installation, use the following code:

```r
remotes::install_github(
  "sysilviakim/rankingQ",
  INSTALL_opts = c("--no-multiarch"),
  dependencies = TRUE
)
```
