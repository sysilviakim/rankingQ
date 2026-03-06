# Return Rankings with Items as Columns

This function takes a ranking dataset with rankings as columns and
returns a dataset with items as columns and rankings as cell values.
This function is useful for converting rankings to a format that allows
for average ranking calculations.

## Usage

``` r
item_to_rank(
  item_rank,
  format_input = "ordering",
  reference = NULL,
  long = FALSE
)
```

## Arguments

- item_rank:

  A data frame with rankings as columns, with items being ranked as cell
  values.

- format_input:

  Character string indicating the format of the data input, namely
  "ordering" or "ranking". Used for
  [`PLMIX::rank_ord_switch`](https://rdrr.io/pkg/PLMIX/man/rank_ord_switch.html).

- reference:

  A character vector of item names to be used for renaming the columns.
  If not specified, will use the first 26 letters of the alphabet.
  Default is \`NULL\`.

- long:

  Whether to return the output in a long data format. Default is
  \`FALSE\`.

## Value

A data frame with items that are being ranked as columns, with rankings
in cell values.

## Examples

``` r
true_pref <- data.frame(
  first = c("b", "a", "c"),
  second = c("c", "b", "b"),
  third = c("a", "c", "a")
)
item_to_rank(true_pref)
#>   a b c
#> 1 3 1 2
#> 2 1 2 3
#> 3 3 2 1
item_to_rank(true_pref, long = TRUE)
#> # A tibble: 9 × 2
#>   item   rank
#>   <chr> <int>
#> 1 a         3
#> 2 b         1
#> 3 c         2
#> 4 a         1
#> 5 b         2
#> 6 c         3
#> 7 a         3
#> 8 b         2
#> 9 c         1
```
