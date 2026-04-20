# rankingQ 0.2.0

* First CRAN submission. The package was previously available on GitHub
  under version 0.1.0.
* New exported function `rank_wider()` for converting long-format ranking
  data to wide format (one column per item or a single pasted ranking
  string).
* `imprr_direct()`: `anc_correct` is now optional. Users can instead
  supply `p_random` directly, or rely on anchor-based estimation of the
  non-random response rate.
* `imprr_direct_rcpp()`: correctly delegates to `imprr_direct()` when
  `method = "fixed"`, forwarding all population and assumption inputs.
* `rpluce()`: when all remaining Plackett-Luce weights become zero after
  earlier draws, the remaining items are now sampled uniformly at random
  rather than being ordered by input position.
* `rank_longer()`: stricter validation on the `reference` argument and
  clearer handling of single- vs multi-column input.
* Stronger input validation across the package: weights must be finite,
  non-negative, and sum to a positive number; `J` must be finite;
  permutation labels are parsed consistently across compact, delimited,
  and fixed-width formats (including J > 9).
* Permutation-label infrastructure rewritten to support rankings of
  arbitrary size.
* Documentation: switched to `\href{}{}` for external URLs; expanded
  examples and `\value{}` sections.

# rankingQ 0.1.0

* Initial GitHub release (not submitted to CRAN).
* Implements design-based bias correction methods for ranking survey
  questions, based on Atsusaka and Kim (2025)
  <doi:10.1017/pan.2024.33>.
* Core functions for bias correction: `imprr_direct()`,
  `imprr_direct_rcpp()`, and `imprr_weights()`.
* Rcpp backend for bootstrap-based estimation.
* Visualization functions: `plot_avg_ranking()` and
  `plot_dist_ranking()`.
* Utility functions for ranking data manipulation: `rank_longer()`,
  `item_to_rank()`, `recover_recorded_responses()`, and
  `permn_augment()`.
* Statistical testing with `uniformity_test()`.
* Simulation support with `rpluce()` for Plackett-Luce sampling.
* Includes example dataset from an identity ranking survey.
