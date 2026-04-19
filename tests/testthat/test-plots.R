# Test plot functions

test_that("plot_avg_ranking returns a ggplot object", {
  identity <- rankingQ::identity
  result <- imprr_direct(
    identity,
    J           = 4,
    main_q      = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 5
  )
  p <- plot_avg_ranking(result$results)
  expect_s3_class(p, "ggplot")
})

test_that("plot_avg_ranking with qoi_filter = NULL returns a ggplot object", {
  df <- data.frame(
    qoi   = rep("average rank", 4),
    item  = c("A", "B", "C", "D"),
    mean  = c(1.5, 2.0, 2.5, 3.0),
    lower = c(1.0, 1.5, 2.0, 2.5),
    upper = c(2.0, 2.5, 3.0, 3.5)
  )
  p <- plot_avg_ranking(df, qoi_filter = NULL)
  expect_s3_class(p, "ggplot")
})

test_that("plot_avg_ranking preserves a user-provided xlab", {
  df <- data.frame(
    qoi   = rep("average rank", 4),
    item  = c("A", "B", "C", "D"),
    mean  = c(1.5, 2.0, 2.5, 3.0),
    lower = c(1.0, 1.5, 2.0, 2.5),
    upper = c(2.0, 2.5, 3.0, 3.5)
  )
  p <- plot_avg_ranking(df, xlab = "Custom X Label")
  expect_equal(p$labels$x, "Custom X Label")
})

test_that("plot_dist_ranking returns a ggplot object", {
  tab <- table(
    c(
      rep("123", 10), rep("132", 5), rep("213", 8),
      rep("231", 7), rep("312", 6), rep("321", 4)
    )
  ) |>
    table_to_tibble()

  p <- plot_dist_ranking(tab)
  expect_s3_class(p, "ggplot")
})

test_that("plot_dist_ranking respects custom fill color", {
  tab <- table(c(rep("123", 10), rep("321", 10))) |>
    table_to_tibble()

  p <- plot_dist_ranking(tab, fill = "steelblue", ylim = 0.7)
  expect_s3_class(p, "ggplot")
})
