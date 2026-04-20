# Turn Long Ranking Data into a Wide Format

This function takes ranking data in long format and returns a
wide-format data frame with one row per respondent. It can return either
one column per ranked item or a single pasted ranking string.

## Usage

``` r
rank_wider(
  x,
  id,
  item = "item_name",
  rank = "ranking",
  output = c("multiple", "single"),
  reference = NULL,
  ranking_name = "ranking"
)
```

## Arguments

- x:

  A data frame in long format with respondent identifiers, item names,
  and ranks.

- id:

  The column that uniquely identifies the respondent.

- item:

  The column that contains item names. Defaults to `"item_name"`.

- rank:

  The column that contains rank values. Defaults to `"ranking"`.

- output:

  The desired output format: `"multiple"` for one column per item or
  `"single"` for one pasted ranking string. Defaults to `"multiple"`.

- reference:

  Optional character vector giving the reference choice-set order. If
  omitted and `reference_no` is present in `x`, item order is inferred
  from that column.

- ranking_name:

  The name of the output column when `output = "single"`. Defaults to
  `"ranking"`.

## Value

A data frame in wide format with one row per respondent.

## Examples

``` r
x <- data.frame(
  id = c(1, 1, 1, 2, 2, 2),
  item_name = c("A", "B", "C", "A", "B", "C"),
  ranking = c(1, 2, 3, 3, 2, 1)
)

rank_wider(x, id = "id")
#>   id A B C
#> 1  1 1 2 3
#> 2  2 3 2 1
rank_wider(
  x,
  id = "id",
  output = "single",
  reference = c("A", "B", "C")
)
#>   id ranking
#> 1  1     123
#> 2  2     321
```
