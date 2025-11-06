test_that("plot_average_rank creates ggplot object", {
  # Create sample data similar to imprr_direct output
  data <- data.frame(
    item = c("Item1", "Item2", "Item3"),
    qoi = rep("average rank", 3),
    outcome = rep("avg", 3),
    mean = c(2.5, 1.8, 3.2),
    lower = c(2.3, 1.6, 3.0),
    upper = c(2.7, 2.0, 3.4)
  )
  
  plot <- plot_average_rank(data)
  
  # Should return a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("plot_average_rank filters by qoi", {
  data <- data.frame(
    item = c("Item1", "Item2", "Item3", "Item1", "Item2"),
    qoi = c("average rank", "average rank", "average rank", "other", "other"),
    outcome = rep("avg", 5),
    mean = c(2.5, 1.8, 3.2, 1.5, 2.0),
    lower = c(2.3, 1.6, 3.0, 1.3, 1.8),
    upper = c(2.7, 2.0, 3.4, 1.7, 2.2)
  )
  
  plot <- plot_average_rank(data, qoi_filter = "average rank")
  
  # Should create plot (we can't easily test the filtered data inside the plot,
  # but we can verify it doesn't error)
  expect_s3_class(plot, "ggplot")
})

test_that("plot_average_rank accepts custom labels", {
  data <- data.frame(
    item = c("Item1", "Item2"),
    qoi = rep("average rank", 2),
    outcome = rep("avg", 2),
    mean = c(2.5, 1.8),
    lower = c(2.3, 1.6),
    upper = c(2.7, 2.0)
  )
  
  plot <- plot_average_rank(data, xlab = "Custom X Label", ylab = "Custom Y Label")
  
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$x, "Custom X Label")
  expect_equal(plot$labels$y, "Custom Y Label")
})

test_that("plot_average_rank with NULL qoi_filter", {
  data <- data.frame(
    item = c("Item1", "Item2"),
    qoi = rep("average rank", 2),
    outcome = rep("avg", 2),
    mean = c(2.5, 1.8),
    lower = c(2.3, 1.6),
    upper = c(2.7, 2.0)
  )
  
  plot <- plot_average_rank(data, qoi_filter = NULL)
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_average_rank with different qoi types", {
  data <- data.frame(
    item = c("Item1", "Item2"),
    qoi = rep("marginal ranking", 2),
    outcome = rep("Ranked 1st", 2),
    mean = c(0.25, 0.35),
    lower = c(0.20, 0.30),
    upper = c(0.30, 0.40)
  )
  
  plot <- plot_average_rank(data, qoi_filter = "marginal ranking")
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_average_rank handles empty string for labels", {
  data <- data.frame(
    item = c("Item1", "Item2"),
    qoi = rep("average rank", 2),
    outcome = rep("avg", 2),
    mean = c(2.5, 1.8),
    lower = c(2.3, 1.6),
    upper = c(2.7, 2.0)
  )
  
  plot <- plot_average_rank(data, xlab = "", ylab = "")
  
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$x, "")
  expect_equal(plot$labels$y, "")
})

test_that("plot_average_rank capitalizes qoi in xlab", {
  data <- data.frame(
    item = c("Item1", "Item2"),
    qoi = rep("top-k ranking", 2),
    outcome = rep("Top-1", 2),
    mean = c(0.25, 0.35),
    lower = c(0.20, 0.30),
    upper = c(0.30, 0.40)
  )
  
  plot <- plot_average_rank(data, qoi_filter = "top-k ranking", xlab = NULL)
  
  expect_s3_class(plot, "ggplot")
  # xlab should be capitalized version of qoi_filter
  expect_equal(plot$labels$x, "Top-k Ranking")
})
