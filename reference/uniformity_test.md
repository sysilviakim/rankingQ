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

## Examples

``` r
tab <- table(c(
  rep("123", 10), rep("132", 10), rep("213", 10),
  rep("231", 10), rep("312", 10), rep("321", 10)
))
uniformity_test(tab)
#> 
#>  Chi-squared test for given probabilities
#> 
#> data:  tab
#> X-squared = 0, df = 5, p-value = 1
#> 
```
