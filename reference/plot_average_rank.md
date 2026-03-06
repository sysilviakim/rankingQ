# Plot Average Rank Results

This function takes the output from the \`imprr_direct\` function and
plots the average rank results with confidence intervals. As long as the
mean and the confidence intervals are provided, this function will plot
other quantities of interest such as marginal, pairwise, top-k rankings.

## Usage

``` r
plot_average_rank(data, qoi_filter = "average rank", xlab = NULL, ylab = "")
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

  The x-axis label. Defaults to NULL. If you'd like it to be empty,
  specify an empty string.

- ylab:

  The y-axis label. Defaults to an empty string.

## Value

A ggplot object.
