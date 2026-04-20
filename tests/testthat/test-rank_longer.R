# Test rank_longer function

test_that("rank_longer converts single column correctly", {
  df <- data.frame(
    id = c(1, 2),
    ranking = c("123", "321")
  )

  result <- rank_longer(df, cols = "ranking", id = "id")

  expect_true("item_name" %in% names(result))
  expect_true("ranking" %in% names(result))
  expect_equal(nrow(result), 6) # 2 rows * 3 items
})

test_that("rank_longer uses reference labels", {
  df <- data.frame(
    id = c(1, 2),
    ranking = c("123", "321")
  )

  result <- rank_longer(df,
    cols = "ranking", id = "id",
    reference = c("A", "B", "C")
  )

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

test_that("rank_longer creates row-number ids when id is omitted", {
  df <- data.frame(ranking = c("123", "321"))

  result <- suppressMessages(rank_longer(df, cols = "ranking"))

  expect_true("id" %in% names(result))
  expect_equal(unique(result$id), 1:2)
})

test_that("rank_longer arranges output by id and reference order", {
  df <- data.frame(
    id = c(2, 1),
    ranking = c("321", "123")
  )

  result <- suppressMessages(rank_longer(df, cols = "ranking", id = "id"))

  expect_equal(result$id, c(1, 1, 1, 2, 2, 2))
  expect_equal(result$reference_no, rep(1:3, 2))
})

test_that("rank_longer errors on duplicate ids", {
  df <- data.frame(
    id = c(1, 1),
    ranking = c("123", "321")
  )

  expect_error(
    suppressMessages(rank_longer(df, cols = "ranking", id = "id")),
    "The id argument does not uniquely identify the respondent."
  )
})

test_that("rank_longer validates cols and ranking length", {
  expect_error(
    rank_longer(data.frame(id = 1:2), cols = character(), id = "id"),
    "The cols argument must be a character vector of length 1 or greater."
  )
  expect_error(
    suppressMessages(
      rank_longer(
        data.frame(id = 1:2, ranking = c("1", "1")),
        cols = "ranking",
        id = "id"
      )
    ),
    "The max_ranking argument must be greater than 1."
  )
})

test_that("rank_longer errors when reference is shorter than ranking length", {
  df <- data.frame(
    id = 1:2,
    ranking = c("123", "321")
  )

  expect_error(
    suppressMessages(
      rank_longer(
        df,
        cols = "ranking",
        id = "id",
        reference = c("A", "B")
      )
    ),
    paste(
      "The max_ranking argument is greater than",
      "the length of the reference choice set."
    )
  )
})

test_that("rank_longer parses delimiter-separated rankings for J greater than 9", {
  df <- data.frame(
    id = c(1, 2),
    ranking = c(
      paste(1:10, collapse = "|"),
      paste(10:1, collapse = "|")
    )
  )

  result <- rank_longer(df, cols = "ranking", id = "id")

  expect_equal(nrow(result), 20)
  expect_equal(result$reference_no, rep(1:10, 2))
  expect_equal(result$ranking[result$id == 1], 1:10)
  expect_equal(result$ranking[result$id == 2], 10:1)
})
