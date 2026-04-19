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
#>   ranking  freq   prop
#>   <fct>   <dbl>  <dbl>
#> 1 123         5 0.167 
#> 2 132         8 0.267 
#> 3 213         5 0.167 
#> 4 231         2 0.0667
#> 5 312         5 0.167 
#> 6 321         5 0.167 
```
