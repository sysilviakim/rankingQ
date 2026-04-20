# `rankingQ`: Design-Based Methods for Improving Ranking Questions
<img src="man/figures/logo.png" align="right" width="150"/>

[![DOI](https://img.shields.io/badge/DOI-10.1017%2Fpan.2024.33-blue)](https://doi.org/10.1017/pan.2024.33)

## Overview

Survey ranking questions are prone to random responses, where respondents answer carelessly or arbitrarily. `rankingQ` corrects this measurement error using anchor-ranking items, enabling unbiased estimates of ranking-based quantities such as average ranks, marginal rank probabilities, and pairwise preferences.

`rankingQ` implements design-based methods for correcting measurement error in survey ranking questions caused by random responding. It provides direct bias correction, inverse-probability weighting (IPW), visualization helpers, and diagnostics for assessing anchor-ranking questions.

For the underlying methodology, see [Atsusaka and Kim (2025)](https://doi.org/10.1017/pan.2024.33), "Addressing Measurement Errors in Ranking Questions for the Social Sciences," *Political Analysis*, 33(4), 339-360. Visit the [package site](https://sysilviakim.com/rankingQ/) for vignettes and references.

## Installation

Currently, you can install the development version from GitHub:

``` r
remotes::install_github("sysilviakim/rankingQ", dependencies = TRUE)
```

For a full walkthrough of an example and downstream analysis, see the [Getting Started vignette](https://sysilviakim.com/rankingQ/articles/v1-getting-started.html).

## Key Features

- **Bias correction via plug-in estimator** (`imprr_direct`): estimates average ranks, marginal rank probabilities, pairwise preferences, and top-k rankings with confidence intervals
- **Bias correction via IPW** (`imprr_weights`): reweights observed ranking distributions to correct for random responses
- **Visualization** (`plot_avg_ranking`): plots corrected average rankings with uncertainty bounds
- **Diagnostics**: tools for detecting bias and assessing anchor-ranking questions

## Citation

If you use `rankingQ`, please cite:

> Atsusaka, Yuki, and Seo-young Silvia Kim. 2025. "Addressing Measurement Errors in Ranking Questions for the Social Sciences." *Political Analysis* 33(4): 339-360. <https://doi.org/10.1017/pan.2024.33>

``` bibtex
@article{atsusaka_addressing_2025,
  author  = {Atsusaka, Yuki and Kim, Seo-young Silvia},
  title   = {Addressing Measurement Errors in Ranking Questions for the Social Sciences},
  journal = {Political Analysis},
  volume  = {33},
  number  = {4},
  pages   = {339--360},
  year    = {2025},
  doi     = {10.1017/pan.2024.33}
}
```
