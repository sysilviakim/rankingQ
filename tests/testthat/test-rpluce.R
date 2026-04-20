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

test_that("rpluce produces different samples for different seeds", {
  r1 <- rpluce(n = 20, t = 4, prob = c(0.4, 0.3, 0.2, 0.1), seed = 1)
  r2 <- rpluce(n = 20, t = 4, prob = c(0.4, 0.3, 0.2, 0.1), seed = 2)

  expect_false(identical(r1, r2))
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

test_that(
  "rpluce accepts floating-point probabilities that sum to 1 within tolerance",
  {
  expect_no_error(
    rpluce(n = 5, t = 3, prob = c(0.1, 0.2, 0.7), seed = 99)
  )
  }
)

test_that("rpluce handles four-item rankings", {
  result <- rpluce(n = 8, t = 4, prob = c(0.4, 0.3, 0.2, 0.1), seed = 11)

  expect_equal(names(result), ordinal_seq(4))

  for (i in seq_len(nrow(result))) {
    expect_setequal(as.character(unlist(result[i, ])), letters[1:4])
  }
})

test_that("rpluce handles degenerate Plackett-Luce probabilities", {
  result <- rpluce(n = 50, t = 3, prob = c(1, 0, 0), seed = 7)

  expect_equal(result$`1st`, rep("a", 50))
  expect_setequal(unique(result$`2nd`), c("b", "c"))
  expect_setequal(unique(result$`3rd`), c("b", "c"))
  expect_true(all(vapply(
    seq_len(nrow(result)),
    function(i) setequal(as.character(unlist(result[i, 2:3])), c("b", "c")),
    logical(1)
  )))
})

test_that("rpluce errors cleanly on invalid n and t boundary values", {
  expect_error(
    rpluce(n = 0, t = 3, prob = c(0.5, 0.3, 0.2)),
    "The specified n must be a positive whole number."
  )
  expect_error(
    rpluce(n = 1.5, t = 3, prob = c(0.5, 0.3, 0.2)),
    "The specified n must be a positive whole number."
  )
  expect_error(
    rpluce(n = 5, t = 1, prob = 1),
    "The specified t must be a whole number greater than or equal to 2."
  )
  expect_error(
    rpluce(n = 5, t = 2.5, prob = c(0.5, 0.5)),
    "The specified t must be a whole number greater than or equal to 2."
  )
})
