# Turn the Frequency Table into a Tibble or Data Frame

This function converts a frequency table to a tibble or data frame. It
also creates a proportion variable as well as the frequency variable.
This function is useful when plotting distribution of ranking patterns;
see relevant vignette.

## Usage

``` r
table_to_tibble(tab, tibble = TRUE)
```

## Arguments

- tab:

  A frequency table.

- tibble:

  A logical value indicating whether the output should be a tibble or
  data frame. Default is `TRUE`.

## Value

A tibble or data frame, depending on the `tibble` argument.

## Examples

``` r
tab <- lapply(combinat::permn(seq(3)), paste0, collapse = "") |>
  sample(30, replace = TRUE) |>
  unlist() |>
  table()
table_to_tibble(tab)
#> # A tibble: 6 × 3
#>   ranking  freq  prop
#>   <fct>   <dbl> <dbl>
#> 1 123         7 0.233
#> 2 132         5 0.167
#> 3 213         6 0.2  
#> 4 231         4 0.133
#> 5 312         3 0.1  
#> 6 321         5 0.167
```
