df_basic <- data.frame(
  id = c("Bernie", "Yuki", "Silvia"),
  rank = c("123", "321", "213")
)

df_long <- data.frame(
  item = c("A", "B", "C", "A", "B", "C"),
  rank = c(3L, 1L, 2L, 1L, 2L, 3L)
)

test_that("Basic one-column data without item names specified", {
  result <- avg_rank(df_basic, "rank")
  expect_equal(nrow(result), 3)
  expect_equal(result$item, c("1st", "2nd", "3rd"))
  expect_equal(result$mean, c(2, 5 / 3, 7 / 3))
})

test_that("Basic one-column data with item names specified", {
  result <- avg_rank(df_basic, "rank", items = c("Money", "Power", "Respect"))
  expect_equal(nrow(result), 3)
  expect_equal(
    result$item,
    factor(
      c("Money", "Power", "Respect"),
      levels = c("Money", "Power", "Respect")
    )
  )
})

test_that("Long-format data.", {
  result <- avg_rank(df_long, "rank", items = "item", long = TRUE)
  expect_equal(nrow(result), 3)
  expect_equal(result$item, c("A", "B", "C"))
  expect_equal(result$mean, c(2.0, 1.5, 2.5))
})

test_that("Error is thrown for incorrect arguments", {
  ## Writing tests for only a few cases
  expect_error(avg_rank(data.frame()), "The rankings variable must be specified.")
  expect_error(avg_rank(df_long, "invalid_col"), "The rankings variable is not contained in the given data frame.")
  expect_error(avg_rank(df_long, long = 3), "The 'long' argument must be either TRUE or FALSE.")
  expect_error(avg_rank(df_long, raw = "yes"), "The 'raw' argument must be either TRUE or FALSE.")
})
