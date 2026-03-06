# Draw Samples from the Plackett-Luce Model

This function draws samples from the Plackett-Luce model, using
Algorithm 2.1, "Efficient Sampling from Plackett-Luce," in \[Xia
(2019)\](https://link.springer.com/book/10.1007/978-3-031-01582-3), page
20, Section 2.2.3 Sampling from Random Utility Models. The name `rpluce`
is a convention that follows random generations of numbers from
statistical distributions such as `rnorm` or `rmultinom`.

## Usage

``` r
rpluce(n, t, prob, choices = NULL, seed = NULL)
```

## Arguments

- n:

  The total number of samples to draw.

- t:

  The number of items or alternatives to choose from.

- prob:

  A vector of choice probabilities.

- choices:

  A vector of choices to be ranked.

- seed:

  An optional seed for the random number generator.

## Value

A data frame of rankings of t items for n assessors.

## Details

Input: A parameter \\\overrightarrow{\gamma} = (\gamma_1, \cdots,
\gamma_m)\\ of Plackett-Luce.  

Output: A ranking \\R \in \mathcal{L}(\mathcal{A})\\ from
\\pi\_{\overrightarrow{\gamma}} ( \cdot )\\ under Plackett–Luce.  
1: Let \\R = \emptyset\\ and \\A = \mathcal{A}\\.  
2: for \\t = 1\\ to \\m\\ do  
3: Choose an alternative \\a\_{i_t}\\ from \\A\\ with probability
proportional to \\\gamma\_{i_t}\\.  
4: \\R \leftarrow R \succ a\_{i_t}\\ and \\A \leftarrow A \\ \\ a\_{i_t}
\\\\.  
5: end for  
6: return \\R\\.

## Examples

``` r
rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.2), seed = 123)
#>    1st 2nd 3rd
#> 1    c   a   b
#> 2    a   c   b
#> 3    b   c   a
#> 4    a   b   c
#> 5    a   b   c
#> 6    a   b   c
#> 7    c   a   b
#> 8    b   c   a
#> 9    a   c   b
#> 10   a   c   b
```
