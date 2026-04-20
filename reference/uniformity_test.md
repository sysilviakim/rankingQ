# Uniformity Test for Ranking Patterns

This function implements the uniformity test for ranking permutation
patterns documented in Atsusaka and Kim (2025).

## Usage

``` r
uniformity_test(data, var = NULL)
```

## Arguments

- data:

  The input dataset with ranking data.

- var:

  The variable within `data` to be used in the test. Defaults to NULL.

## Value

A chi-square test result.
