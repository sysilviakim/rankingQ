# Test rpluce function

test_that("rpluce returns a data frame with correct dimensions", {
  result <- rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.2), seed = 123)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 3)
  # Default column names are ordinal positions
  expect_equal(names(result), c("1st", "2nd", "3rd"))
})

test_that("rpluce output contains only valid items", {
  result <- rpluce(n = 20, t = 3, prob = c(0.5, 0.3, 0.2), seed = 1)

  # Each row should be a permutation of a, b, c
  items <- c("a", "b", "c")
  for (i in seq_len(nrow(result))) {
    expect_true(setequal(as.character(unlist(result[i, ])), items))
  }
})

test_that("rpluce is reproducible with a fixed seed", {
  r1 <- rpluce(n = 5, t = 3, prob = c(0.5, 0.3, 0.2), seed = 42)
  r2 <- rpluce(n = 5, t = 3, prob = c(0.5, 0.3, 0.2), seed = 42)
  expect_identical(r1, r2)
})

test_that("rpluce with custom choices renames columns", {
  result <- rpluce(
    n = 5, t = 3, prob = c(0.5, 0.3, 0.2),
    choices = c("X", "Y", "Z"), seed = 1
  )
  expect_equal(names(result), c("X", "Y", "Z"))
})

test_that("rpluce errors on invalid prob argument", {
  expect_error(
    rpluce(n = 5, t = 3, prob = c("a", "b", "c")),
    "The specified probability must be a number."
  )
  expect_error(
    rpluce(n = 5, t = 3, prob = c(NA, 0.5, 0.5)),
    "NA values are not allowed in the prob argument."
  )
  expect_error(
    rpluce(n = 5, t = 3, prob = c(-0.1, 0.6, 0.5)),
    "The specified probability must be between 0 and 1."
  )
  expect_error(
    rpluce(n = 5, t = 3, prob = c(0.5, 0.3, 0.1)),
    "The specified probability must sum to 1."
  )
  expect_error(
    rpluce(n = 5, t = 3, prob = c(0.5, 0.5)),
    "The specified probability must be of length t."
  )
})

test_that("rpluce errors on invalid choices argument", {
  expect_error(
    rpluce(n = 5, t = 3, prob = c(0.5, 0.3, 0.2), choices = c(1, 2, 3)),
    "The specified choices must be a character vector."
  )
  expect_error(
    rpluce(n = 5, t = 3, prob = c(0.5, 0.3, 0.2), choices = c("A", "B")),
    "The specified choices must be of length t."
  )
})
