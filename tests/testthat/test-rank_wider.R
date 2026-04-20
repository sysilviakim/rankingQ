# Test rank_wider function

test_that("rank_wider converts long data to multiple columns", {
  df <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    item_name = c("A", "B", "C", "A", "B", "C"),
    ranking = c(1, 2, 3, 3, 2, 1)
  )

  result <- rank_wider(df, id = "id")

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("id", "A", "B", "C"))
  expect_equal(result$A, c(1, 3))
  expect_equal(result$B, c(2, 2))
  expect_equal(result$C, c(3, 1))
})

test_that("rank_wider converts long data to a single ranking string", {
  df <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    item_name = c("A", "B", "C", "A", "B", "C"),
    ranking = c(1, 2, 3, 3, 2, 1)
  )

  result <- rank_wider(
    df,
    id = "id",
    output = "single",
    reference = c("A", "B", "C")
  )

  expect_equal(names(result), c("id", "ranking"))
  expect_equal(result$ranking, c("123", "321"))
})

test_that("rank_wider round-trips rank_longer for a single ranking column", {
  wide_in <- data.frame(
    id = c(1, 2),
    ranking = c("123", "321")
  )

  long <- rank_longer(wide_in, cols = "ranking", id = "id")
  wide_out <- rank_wider(long, id = "id", output = "single")

  expect_equal(wide_out[order(wide_out$id), ], wide_in[order(wide_in$id), ])
})

test_that("rank_wider round-trips rank_longer for multiple ranking columns", {
  wide_in <- data.frame(
    id = c(1, 2),
    item1 = c(1, 3),
    item2 = c(2, 2),
    item3 = c(3, 1)
  )

  long <- rank_longer(wide_in, cols = c("item1", "item2", "item3"), id = "id")
  wide_out <- rank_wider(long, id = "id")

  expect_equal(wide_out[order(wide_out$id), ], wide_in[order(wide_in$id), ])
})

test_that("rank_wider errors on duplicated respondent-item pairs", {
  df <- data.frame(
    id = c(1, 1),
    item_name = c("A", "A"),
    ranking = c(1, 2)
  )

  expect_error(
    rank_wider(df, id = "id"),
    "Each respondent-item pair must appear at most once."
  )
})

test_that("rank_wider errors on duplicated ranks within respondent", {
  df <- data.frame(
    id = c(1, 1, 1),
    item_name = c("A", "B", "C"),
    ranking = c(1, 1, 3)
  )

  expect_error(
    rank_wider(df, id = "id"),
    "Each respondent must have unique rank values."
  )
})

test_that("rank_wider emits delimiter-separated single-column output for J greater than 9", {
  items <- paste0("Item_", 1:10)
  df <- data.frame(
    id = rep(c(1, 2), each = 10),
    item_name = rep(items, 2),
    ranking = c(1:10, 10:1)
  )

  result <- rank_wider(
    df,
    id = "id",
    output = "single",
    reference = items
  )

  expect_equal(
    result$ranking,
    c(
      paste(1:10, collapse = "|"),
      paste(10:1, collapse = "|")
    )
  )

  round_trip <- rank_longer(
    result,
    cols = "ranking",
    id = "id",
    reference = items
  )

  expect_equal(round_trip$ranking[round_trip$id == 1], 1:10)
  expect_equal(round_trip$ranking[round_trip$id == 2], 10:1)
})
