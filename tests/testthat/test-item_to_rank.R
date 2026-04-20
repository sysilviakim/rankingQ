# Test item_to_rank function

test_that("item_to_rank converts ordering to ranking format", {
  # Ordering format: columns are rank positions, values are item indices
  df <- data.frame(
    first  = c(2L, 1L, 3L),
    second = c(3L, 2L, 2L),
    third  = c(1L, 3L, 1L)
  )
  result <- item_to_rank(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  # Default column names use first 3 letters of the alphabet
  expect_equal(names(result), c("a", "b", "c"))
})

test_that("item_to_rank with custom reference names", {
  df <- data.frame(
    first  = c(1L, 2L),
    second = c(2L, 1L)
  )
  result <- item_to_rank(df, reference = c("Alpha", "Beta"))
  expect_equal(names(result), c("Alpha", "Beta"))
})

test_that("item_to_rank converts ranking to ordering format", {
  df <- data.frame(
    a = c(3L, 1L),
    b = c(1L, 2L),
    c = c(2L, 3L)
  )

  result <- item_to_rank(df, format_input = "ranking")

  expect_equal(as.data.frame(result), data.frame(
    a = c(2L, 1L),
    b = c(3L, 2L),
    c = c(1L, 3L)
  ))
})

test_that("item_to_rank accepts character orderings without PLMIX", {
  df <- data.frame(
    first = c("b", "a"),
    second = c("c", "b"),
    third = c("a", "c")
  )

  result <- item_to_rank(df)

  expect_equal(as.data.frame(result), data.frame(
    a = c(3L, 1L),
    b = c(1L, 2L),
    c = c(2L, 3L)
  ))
})

test_that("item_to_rank long format returns item and rank columns", {
  df <- data.frame(
    first  = c(1L, 2L),
    second = c(2L, 1L)
  )
  result <- item_to_rank(df, long = TRUE)

  expect_true("item" %in% names(result))
  expect_true("rank" %in% names(result))
  # 2 respondents * 2 items = 4 rows
  expect_equal(nrow(result), 4)
})

test_that("item_to_rank errors on invalid long argument", {
  df <- data.frame(
    first  = c(1L, 2L),
    second = c(2L, 1L)
  )
  expect_error(
    item_to_rank(df, long = "yes"),
    "The 'long' argument must be either TRUE or FALSE."
  )
})

test_that("item_to_rank validates format_input and complete permutations", {
  expect_error(
    item_to_rank(data.frame(first = 1L, second = 2L), format_input = "oops"),
    "format_input must be either 'ordering' or 'ranking'."
  )

  expect_error(
    item_to_rank(data.frame(first = 1L, second = 1L)),
    "Each row of item_rank must be a complete permutation"
  )
})

test_that("item_to_rank handles more than 26 items without NA names", {
  df <- as.data.frame(t(replicate(3, sample(1:30))))
  out <- item_to_rank(df)

  expect_equal(ncol(out), 30)
  expect_false(any(is.na(names(out))))
})
