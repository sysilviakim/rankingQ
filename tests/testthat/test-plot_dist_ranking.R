test_that("plot_dist_ranking creates ggplot object", {
  # Create sample data
  tab <- data.frame(
    ranking = factor(c("123", "132", "213", "231", "312", "321")),
    freq = c(50, 30, 25, 20, 15, 10),
    prop = c(50, 30, 25, 20, 15, 10) / 150
  )
  
  plot <- plot_dist_ranking(tab)
  
  # Should return a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("plot_dist_ranking uses custom column names", {
  tab <- data.frame(
    pattern = factor(c("123", "321")),
    count = c(100, 50),
    proportion = c(100, 50) / 150
  )
  
  plot <- plot_dist_ranking(tab, x = "pattern", y = "proportion")
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_dist_ranking with custom parameters", {
  tab <- data.frame(
    ranking = factor(c("123", "321", "213")),
    freq = c(60, 30, 10),
    prop = c(60, 30, 10) / 100
  )
  
  plot <- plot_dist_ranking(
    tab,
    ylim = 0.8,
    fill = "blue",
    xlab = "Custom Label"
  )
  
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$x, "Custom Label")
})

test_that("plot_dist_ranking with 4-item rankings", {
  # Create 4-item ranking data
  tab <- data.frame(
    ranking = factor(c("1234", "4321", "2341", "1324")),
    freq = c(50, 30, 15, 5),
    prop = c(50, 30, 15, 5) / 100
  )
  
  plot <- plot_dist_ranking(tab)
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_dist_ranking handles small datasets", {
  tab <- data.frame(
    ranking = factor(c("12", "21")),
    freq = c(10, 5),
    prop = c(10, 5) / 15
  )
  
  plot <- plot_dist_ranking(tab, ylim = 1.0)
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_dist_ranking with custom styling", {
  tab <- data.frame(
    ranking = factor(c("123", "321", "213")),
    freq = c(60, 30, 10),
    prop = c(60, 30, 10) / 100
  )
  
  plot <- plot_dist_ranking(
    tab,
    fill = "darkgreen",
    linetype = "solid",
    h_color = "red",
    h_alpha = 0.8
  )
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_dist_ranking with text customization", {
  tab <- data.frame(
    ranking = factor(c("123", "321")),
    freq = c(70, 30),
    prop = c(70, 30) / 100
  )
  
  plot <- plot_dist_ranking(
    tab,
    vjust = -1.0,
    size = 4
  )
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_dist_ranking computes uniform reference line", {
  # For 3 items, uniform probability is 1/6
  tab <- data.frame(
    ranking = factor(c("123", "321", "213")),
    freq = c(60, 30, 10),
    prop = c(60, 30, 10) / 100
  )
  
  plot <- plot_dist_ranking(tab)
  
  # Verify plot is created (the horizontal line is at 1/factorial(3) = 1/6)
  expect_s3_class(plot, "ggplot")
})

test_that("plot_dist_ranking handles large permutation space", {
  # Create data for 4 items (24 possible permutations, but showing only a few)
  patterns <- c("1234", "4321", "2341", "3214", "1423")
  tab <- data.frame(
    ranking = factor(patterns),
    freq = c(30, 25, 20, 15, 10),
    prop = c(30, 25, 20, 15, 10) / 100
  )
  
  plot <- plot_dist_ranking(tab, ylim = 0.5)
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_dist_ranking with default xlab", {
  tab <- data.frame(
    ranking = factor(c("123", "321")),
    freq = c(70, 30),
    prop = c(70, 30) / 100
  )
  
  plot <- plot_dist_ranking(tab)
  
  expect_equal(plot$labels$x, "Recorded Responses")
})
