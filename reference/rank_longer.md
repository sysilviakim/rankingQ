# Turn Column(s) Recording Rankings in Wide Format into a Long Data Format

This function takes a data frame in wide format with columns recording
rankings into a long data format.

## Usage

``` r
rank_longer(x, cols = NULL, id = NULL, reference = NULL)
```

## Arguments

- x:

  A data frame in wide format with columns recording rankings.

- cols:

  A character vector of column names that record rankings. If there are
  multiple columns, the order of the columns should be the same as the
  order of the reference choice set. If there is a single column, the
  order in which the numbers appear in respondent-level character
  response should be the same as the order of the reference set.

- id:

  The column that uniquely identify the respondent.

- reference:

  If you wish to specify the reference choice set, you can provide a
  character vector.

## Value

A data frame in long format with columns recording rankings. The first
column is the id variable that has been pre-specified. The second and
third columns record what item is being ranked. The final column records
the ranking of the item.

## Details

If the data frame has more than one columns specified in the `cols`
argument, they will be translated as the first, second, third, etc.
items in the reference choice set. For example, if the first column
records 2, the second column records 1, and the third column records 3,
then the function will interpret that this respondent prefers the second
item the most, then the first item, then the third item.

If the data frame has only one column specified in the `cols` argument,
it will be parsed by character length. For example, if the column
records "213", then the function will interpret that this respondent
prefers the second item the most, then the first, and then the third
item.

Currently, this function depends on `tidyverse` functions. Eventually, a
`data.table` option will be added for large datasets.

## Examples

``` r
x <- data.frame(
  apple = c(2, 1, 3),
  orange = c(1, 3, 2),
  banana = c(3, 2, 1)
)
rank_longer(x)
#> No ID column specified. Using row number.
#> Multiple columns selected.
#> Using input column order as ranking order.
#> Joining with `by = join_by(item_name)`
#> # A tibble: 9 × 4
#>      id reference_no item_name ranking
#>   <int>        <int> <chr>       <dbl>
#> 1     1            1 apple           2
#> 2     1            2 orange          1
#> 3     1            3 banana          3
#> 4     2            1 apple           1
#> 5     2            2 orange          3
#> 6     2            3 banana          2
#> 7     3            1 apple           3
#> 8     3            2 orange          2
#> 9     3            3 banana          1

y <- data.frame(
  id = c("Bernie", "Yuki", "Silvia"),
  rank = c("123", "321", "213")
)
rank_longer(y, cols = "rank", id = "id")
#> One column selected. Parsing column by character length.
#> No reference choice set specified. Using general column names.
#> Joining with `by = join_by(item_name)`
#> # A tibble: 9 × 4
#>   id     reference_no item_name ranking
#>   <chr>         <int> <chr>       <dbl>
#> 1 Bernie            1 V1              1
#> 2 Bernie            2 V2              2
#> 3 Bernie            3 V3              3
#> 4 Silvia            1 V1              2
#> 5 Silvia            2 V2              1
#> 6 Silvia            3 V3              3
#> 7 Yuki              1 V1              3
#> 8 Yuki              2 V2              2
#> 9 Yuki              3 V3              1
rank_longer(
  y,
  cols = "rank", id = "id",
  reference = c("Money", "Power", "Respect")
)
#> One column selected. Parsing column by character length.
#> Joining with `by = join_by(item_name)`
#> # A tibble: 9 × 4
#>   id     reference_no item_name ranking
#>   <chr>         <int> <chr>       <dbl>
#> 1 Bernie            1 Money           1
#> 2 Bernie            2 Power           2
#> 3 Bernie            3 Respect         3
#> 4 Silvia            1 Money           2
#> 5 Silvia            2 Power           1
#> 6 Silvia            3 Respect         3
#> 7 Yuki              1 Money           3
#> 8 Yuki              2 Power           2
#> 9 Yuki              3 Respect         1
```
