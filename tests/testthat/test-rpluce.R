test_that("rpluce generates correct number of samples", {
  set.seed(123)
  result <- rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.2))
  
  # Should have 10 rows (samples) and 3 columns (items)
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 3)
})

test_that("rpluce uses seed for reproducibility", {
  result1 <- rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.2), seed = 123)
  result2 <- rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.2), seed = 123)
  
  expect_equal(result1, result2)
})

test_that("rpluce produces different results with different seeds", {
  result1 <- rpluce(n = 100, t = 3, prob = c(0.5, 0.3, 0.2), seed = 123)
  result2 <- rpluce(n = 100, t = 3, prob = c(0.5, 0.3, 0.2), seed = 456)
  
  # Results should be different (with high probability)
  expect_false(identical(result1, result2))
})

test_that("rpluce uses custom choice names", {
  result <- rpluce(
    n = 5, t = 3, prob = c(0.5, 0.3, 0.2),
    choices = c("Apple", "Banana", "Cherry"),
    seed = 123
  )
  
  expect_equal(names(result), c("Apple", "Banana", "Cherry"))
})

test_that("rpluce uses ordinal names by default", {
  result <- rpluce(n = 5, t = 3, prob = c(0.5, 0.3, 0.2), seed = 123)
  
  expect_equal(names(result), c("1st", "2nd", "3rd"))
})

test_that("rpluce throws error for non-numeric probability", {
  expect_error(
    rpluce(n = 10, t = 3, prob = c("a", "b", "c")),
    "The specified probability must be a number."
  )
})

test_that("rpluce throws error for probabilities with NA", {
  expect_error(
    rpluce(n = 10, t = 3, prob = c(0.5, NA, 0.2)),
    "NA values are not allowed in the prob argument."
  )
})

test_that("rpluce throws error for probabilities outside [0,1]", {
  expect_error(
    rpluce(n = 10, t = 3, prob = c(0.5, 0.3, -0.2)),
    "The specified probability must be between 0 and 1."
  )
  
  expect_error(
    rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 1.2)),
    "The specified probability must be between 0 and 1."
  )
})

test_that("rpluce throws error when probabilities don't sum to 1", {
  expect_error(
    rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.1)),
    "The specified probability must sum to 1."
  )
  
  expect_error(
    rpluce(n = 10, t = 3, prob = c(0.4, 0.4, 0.4)),
    "The specified probability must sum to 1."
  )
})

test_that("rpluce throws error when prob length doesn't match t", {
  expect_error(
    rpluce(n = 10, t = 3, prob = c(0.6, 0.4)),
    "The specified probability must be of length t."
  )
  
  expect_error(
    rpluce(n = 10, t = 2, prob = c(0.3, 0.3, 0.4)),
    "The specified probability must be of length t."
  )
})

test_that("rpluce throws error for non-character choices", {
  expect_error(
    rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.2), choices = c(1, 2, 3)),
    "The specified choices must be a character vector."
  )
})

test_that("rpluce throws error when choices length doesn't match t", {
  expect_error(
    rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.2), choices = c("A", "B")),
    "The specified choices must be of length t."
  )
})

test_that("rpluce works with uniform probabilities", {
  result <- rpluce(
    n = 100, t = 3,
    prob = c(1/3, 1/3, 1/3),
    seed = 123
  )
  
  expect_equal(nrow(result), 100)
  expect_equal(ncol(result), 3)
})

test_that("rpluce works with extreme probabilities", {
  # One item has very high probability
  result <- rpluce(n = 50, t = 3, prob = c(0.98, 0.01, 0.01), seed = 123)
  
  expect_equal(nrow(result), 50)
  expect_equal(ncol(result), 3)
  
  # First column should mostly be "a" (the highest probability item)
  # This is a probabilistic test, so we check it occurs frequently
  first_choice_counts <- table(result[[1]])
  expect_true(first_choice_counts["a"] > 40)  # Should be > 40 out of 50
})

test_that("rpluce works with 4 items", {
  result <- rpluce(
    n = 20, t = 4,
    prob = c(0.4, 0.3, 0.2, 0.1),
    seed = 123
  )
  
  expect_equal(nrow(result), 20)
  expect_equal(ncol(result), 4)
})

test_that("rpluce works with 5 items", {
  result <- rpluce(
    n = 10, t = 5,
    prob = c(0.3, 0.25, 0.2, 0.15, 0.1),
    seed = 123
  )
  
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 5)
})

test_that("rpluce produces rankings with all items", {
  result <- rpluce(n = 10, t = 3, prob = c(0.5, 0.3, 0.2), seed = 123)
  
  # Each row should contain each item exactly once
  for (i in 1:nrow(result)) {
    row_items <- as.character(result[i, ])
    expect_equal(length(unique(row_items)), 3)
    expect_true(all(c("a", "b", "c") %in% row_items))
  }
})

test_that("rpluce returns a data frame", {
  result <- rpluce(n = 5, t = 3, prob = c(0.5, 0.3, 0.2), seed = 123)
  
  expect_s3_class(result, "data.frame")
})
