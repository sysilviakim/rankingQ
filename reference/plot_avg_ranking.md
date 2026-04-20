# Plot Average Rank Results

This function takes the output from the \`imprr_direct\` function and
plots the average rank results with confidence intervals. As long as the
mean and the confidence intervals are provided, this function will plot
other quantities of interest such as marginal, pairwise, top-k rankings.

## Usage

``` r
plot_avg_ranking(data, qoi_filter = "average rank", xlab = NULL, ylab = "")
```

## Arguments

- data:

  The results data from the \`imprr_direct\` function. If an external
  data frame, make sure that the column names are the same as the output
  from the \`imprr_direct\` function.

- qoi_filter:

  The quantity of interest (QOI) to filter for. Defaults to "average
  rank".

- xlab:

  The x-axis label. Defaults to NULL. If NULL and \`qoi_filter\` is
  provided, a simple capitalized label based on \`qoi_filter\` is used.
  If \`qoi_filter\` is NULL, the default ggplot x-axis label is left
  unchanged. If you'd like it to be empty, specify an empty string.

- ylab:

  The y-axis label. Defaults to an empty string.

## Value

A ggplot object.

## Examples

``` r
avg_rank_results <- data.frame(
  item = c("Party", "Religion", "Gender", "Race"),
  qoi = rep("average rank", 4),
  mean = c(1.7, 2.1, 2.8, 3.4),
  lower = c(1.5, 1.9, 2.6, 3.2),
  upper = c(1.9, 2.3, 3.0, 3.6)
)
plot_avg_ranking(avg_rank_results)

```
