# 6. Simulating Ranking Data

In this vignette, we describe our helper function `rpluce` to simulate
ranking data based on the Plackett-Luce model, also known as the
rank-order logit or random utility model. To draw from the PL, `rpluce`
requires a vector of probabilities that `t` items will be selected as
the first choice, respectively. For each unit, the output provides a
full ordering of `t` items with distinct alphabet letters.

``` r
library(rankingQ)
```

``` r
draw <- rpluce(n = 50,
               t = 4,
               prob = c(0.4, 0.2, 0.2, 0.2))
head(draw)
#>   1st 2nd 3rd 4th
#> 1   b   a   d   c
#> 2   d   b   c   a
#> 3   a   d   c   b
#> 4   d   c   b   a
#> 5   b   a   d   c
#> 6   a   b   d   c
```
