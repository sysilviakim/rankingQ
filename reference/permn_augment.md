# Augmenting Permutation Patterns

In some distribution of ranking data, not all possible permutation
patterns may be realized due to the sample size or skewed distribution
of preferences.

## Usage

``` r
permn_augment(tab, J = NULL)
```

## Arguments

- tab:

  A table of observed permutation patterns.

- J:

  The length of the reference choice set. Defaults to NULL, in which it
  will count the number of characters in the first input.

## Value

A table of observed permutation patterns augmented with all possible
permutation patterns.

## Details

This function augments the given table with all possible observed
permutation patterns with a frequency of zero for unrealized patterns.
Currently, this only takes full rankings into account, as opposed to
partial rankings.

## Examples

``` r
tab <- table(c(rep("123", 100), rep("321", 50)))
permn_augment(tab, J = 3)
#> 123 132 213 231 312 321 
#> 100   0   0   0   0  50 

tab <- table(c("123", "321", "213", "312", "132", "231"))
permn_augment(tab, J = 3)
#> 
#> 123 132 213 231 312 321 
#>   1   1   1   1   1   1 
```
