# Changelog

## rankingQ 0.1.0

- Initial CRAN release.
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
