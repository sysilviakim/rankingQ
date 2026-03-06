# Generate an Ordinal Sequence from a Number

This function generates an ordinal sequence of an arbitrary length. For
example, if the length is 3, the function will return the vector
`c("1st", "2nd", "3rd")`. This function is used within `avg_rank` and
such functions.

## Usage

``` r
ordinal_seq(length)
```

## Arguments

- length:

  The length of the ordinal sequence to generate. It should be a numeric
  value of length 1.

## Value

A vector of ordinal strings.

## Examples

``` r
ordinal_seq(11)
#>  [1] "1st"  "2nd"  "3rd"  "4th"  "5th"  "6th"  "7th"  "8th"  "9th"  "10th"
#> [11] "11th"
```
