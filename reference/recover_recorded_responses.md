# Recover the Recorded Responses Given that Ranking Items were Randomized

This function, using the order of the items that was presented to the
respondent as well as the true responses to the ranking question,
recovers the recorded responses, or the responses that the respondent
actually provided, ignoring the order in which the items were presented.

## Usage

``` r
recover_recorded_responses(true_order, presented_order, df = NULL)
```

## Arguments

- true_order:

  A string representing the true ranking of the respondent with respect
  to the reference choice set.

- presented_order:

  A string representing the order of the items that were presented to
  the respondent.

- df:

  The input data frame. Defaults to NULL. If NULL, the function expects
  inputs as simple strings such as "321" or "312".

## Value

A data frame with the true ranking of the items per respondent.

## Details

This is to see if behavior such as diagonalization occurred. Most survey
software will take the recorded response and translate it into the true
ranking of the respondent (observed ranking), but not providing the
recorded response.

For example, given a reference choice set of three items, A, B, and C,
the respondent may have been presented with the items in the order C, B,
A, and may have responded with 3-2-1 as a recorded response. The true
order of the items is A, B, C, so the observed/true ranking is 1-2-3.
This function takes C, B, A and 1-2-3 as inputs and returns 3-2-1.

## Examples

``` r
## This respondent's true ranking reported is A-B-C-D.
## However, the items were presented in the order B-A-D-C.
## Therefore, the respondent's recorded response is 2-1-4-3.
recover_recorded_responses(true_order = "1234", "2143") ## Output: "2143"
#> [1] "2143"

## This respondent's true ranking is reported as D-C-B-A.
## However, the items were presented in the order A-B-C-D.
## Therefore, the respondent's recorded response is 4-3-2-1.
recover_recorded_responses(true_order = "4321", "1234") ## Output: "4321"
#> [1] "4321"

## This respondent's true ranking is reported as C-A-D-B.
## However, the items were presented in the order D-C-B-A.
## Therefore, the respondent's recorded response is 3-1-4-2.
recover_recorded_responses(true_order = "2413", "4321") ## Output: "3142"
#> [1] "3142"
```
