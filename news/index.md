# Changelog

## rankingQ 0.2.0

- First CRAN submission. The package was previously available on GitHub
  under version 0.1.0.
- New exported function
  [`rank_wider()`](http://sysilviakim.com/rankingQ/reference/rank_wider.md)
  for converting long-format ranking data to wide format (one column per
  item or a single pasted ranking string).
- [`imprr_direct()`](http://sysilviakim.com/rankingQ/reference/imprr_direct.md):
  `anc_correct` is now optional. Users can instead supply `p_random`
  directly, or rely on anchor-based estimation of the non-random
  response rate.
- [`imprr_direct_rcpp()`](http://sysilviakim.com/rankingQ/reference/imprr_direct_rcpp.md):
  correctly delegates to
  [`imprr_direct()`](http://sysilviakim.com/rankingQ/reference/imprr_direct.md)
  when `method = "fixed"`, forwarding all population and assumption
  inputs.
- [`rpluce()`](http://sysilviakim.com/rankingQ/reference/rpluce.md):
  when all remaining Plackett-Luce weights become zero after earlier
  draws, the remaining items are now sampled uniformly at random rather
  than being ordered by input position.
- [`rank_longer()`](http://sysilviakim.com/rankingQ/reference/rank_longer.md):
  stricter validation on the `reference` argument and clearer handling
  of single- vs multi-column input.
- Stronger input validation across the package: weights must be finite,
  non-negative, and sum to a positive number; `J` must be finite;
  permutation labels are parsed consistently across compact, delimited,
  and fixed-width formats (including J \> 9).
- Permutation-label infrastructure rewritten to support rankings of
  arbitrary size.
- Documentation: switched to `\href{}{}` for external URLs; expanded
  examples and `\value{}` sections.

## rankingQ 0.1.0

- Initial GitHub release (not submitted to CRAN).
- Implements design-based bias correction methods for ranking survey
  questions, based on Atsusaka and Kim (2025) <doi:10.1017/pan.2024.33>.
- Core functions for bias correction:
  [`imprr_direct()`](http://sysilviakim.com/rankingQ/reference/imprr_direct.md),
  [`imprr_direct_rcpp()`](http://sysilviakim.com/rankingQ/reference/imprr_direct_rcpp.md),
  and
  [`imprr_weights()`](http://sysilviakim.com/rankingQ/reference/imprr_weights.md).
- Rcpp backend for bootstrap-based estimation.
- Visualization functions:
  [`plot_avg_ranking()`](http://sysilviakim.com/rankingQ/reference/plot_avg_ranking.md)
  and
  [`plot_dist_ranking()`](http://sysilviakim.com/rankingQ/reference/plot_dist_ranking.md).
- Utility functions for ranking data manipulation:
  [`rank_longer()`](http://sysilviakim.com/rankingQ/reference/rank_longer.md),
  [`item_to_rank()`](http://sysilviakim.com/rankingQ/reference/item_to_rank.md),
  [`recover_recorded_responses()`](http://sysilviakim.com/rankingQ/reference/recover_recorded_responses.md),
  and
  [`permn_augment()`](http://sysilviakim.com/rankingQ/reference/permn_augment.md).
- Statistical testing with
  [`uniformity_test()`](http://sysilviakim.com/rankingQ/reference/uniformity_test.md).
- Simulation support with
  [`rpluce()`](http://sysilviakim.com/rankingQ/reference/rpluce.md) for
  Plackett-Luce sampling.
- Includes example dataset from an identity ranking survey.
