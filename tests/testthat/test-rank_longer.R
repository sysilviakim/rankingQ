# Test rank_longer function

test_that("rank_longer converts single column correctly", {
  df <- data.frame(
    id = c(1, 2),
    ranking = c("123", "321")
  )

  result <- rank_longer(df, cols = "ranking", id = "id")

  expect_true("item_name" %in% names(result))
  expect_true("ranking" %in% names(result))
  expect_equal(nrow(result), 6)  # 2 rows * 3 items
})

test_that("rank_longer uses reference labels", {
  df <- data.frame(
    id = c(1, 2),
    ranking = c("123", "321")
  )

  result <- rank_longer(df, cols = "ranking", id = "id",
                        reference = c("A", "B", "C"))

  expect_true(all(c("A", "B", "C") %in% result$item_name))
})

test_that("rank_longer handles multiple columns", {
  df <- data.frame(
    id = c(1, 2),
    item1 = c(1, 3),
    item2 = c(2, 2),
    item3 = c(3, 1)
  )

  result <- rank_longer(df, cols = c("item1", "item2", "item3"), id = "id")

  expect_equal(nrow(result), 6)
})
