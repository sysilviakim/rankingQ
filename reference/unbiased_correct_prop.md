# Unbiased Estimator of the Proportion of Random and Non-random Responses

This function computes the unbiased proportion of \*correct\* answers
after adjusting for the possibility that the respondent may have
randomly guessed the correct answer. The function is based on the
formula provided by Proposition 1 of Atsusaka and Kim (2024).

## Usage

``` r
unbiased_correct_prop(mean_c, J)
```

## Arguments

- mean_c:

  This is the raw proportion of the correct answers.

- J:

  The number of items to rank order.

## Value

A number between 0-1.

## Examples

``` r
unbiased_correct_prop(0.7, 3)
#> [1] 0.64
```
