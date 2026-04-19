# rankingQ 0.1.0

* Initial CRAN release.
* Implements design-based bias correction methods for ranking survey questions,
  based on Atsusaka and Kim (2025) <doi:10.1017/pan.2024.33>.
* Core functions for bias correction: `imprr_direct()`, `imprr_direct_rcpp()`,
  and `imprr_weights()`.
* Rcpp backend for bootstrap-based estimation.
* Visualization functions: `plot_average_rank()` and `plot_dist_ranking()`.
* Utility functions for ranking data manipulation: `rank_longer()`,
  `item_to_rank()`, `recover_recorded_responses()`, and `permn_augment()`.
* Statistical testing with `uniformity_test()`.
* Simulation support with `rpluce()` for Plackett-Luce sampling.
* Includes example dataset from an identity ranking survey.
