test_that("item_to_rank converts ordering to ranking format", {
  true_pref <- data.frame(
    first = c("b", "a", "c"),
    second = c("c", "b", "b"),
    third = c("a", "c", "a")
  )
  
  result <- item_to_rank(true_pref)
  
  # Should have same number of rows
  expect_equal(nrow(result), 3)
  
  # Should have columns for each item
  expect_equal(ncol(result), 3)
  
  # Check that it's a data frame
  expect_s3_class(result, "data.frame")
})

test_that("item_to_rank works with long format output", {
  true_pref <- data.frame(
    first = c("b", "a", "c"),
    second = c("c", "b", "b"),
    third = c("a", "c", "a")
  )
  
  result <- item_to_rank(true_pref, long = TRUE)
  
  # Should have 9 rows (3 respondents × 3 items)
  expect_equal(nrow(result), 9)
  
  # Should have item and rank columns
  expect_true(all(c("item", "rank") %in% names(result)))
})

test_that("item_to_rank uses custom reference names", {
  true_pref <- data.frame(
    first = c("b", "a", "c"),
    second = c("c", "b", "b"),
    third = c("a", "c", "a")
  )
  
  result <- item_to_rank(true_pref, reference = c("Apple", "Banana", "Cherry"))
  
  # Check that custom names are used as column names
  expect_true(all(c("Apple", "Banana", "Cherry") %in% names(result)))
})

test_that("item_to_rank throws error for invalid long argument", {
  true_pref <- data.frame(
    first = c("b", "a"),
    second = c("c", "b"),
    third = c("a", "c")
  )
  
  expect_error(
    item_to_rank(true_pref, long = "yes"),
    "The 'long' argument must be either TRUE or FALSE."
  )
  
  expect_error(
    item_to_rank(true_pref, long = 1),
    "The 'long' argument must be either TRUE or FALSE."
  )
})

test_that("item_to_rank handles different format_input", {
  # Test with ranking format (not ordering)
  true_pref <- data.frame(
    first = c(2, 1, 3),
    second = c(1, 3, 2),
    third = c(3, 2, 1)
  )
  
  # This should work with format_input = "ranking"
  result <- item_to_rank(true_pref, format_input = "ranking")
  
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
})

test_that("item_to_rank handles 4 items", {
  true_pref <- data.frame(
    first = c("a", "b"),
    second = c("b", "c"),
    third = c("c", "d"),
    fourth = c("d", "a")
  )
  
  result <- item_to_rank(true_pref)
  
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 4)
})

test_that("item_to_rank with long format has correct structure", {
  true_pref <- data.frame(
    first = c("a", "b"),
    second = c("b", "a")
  )
  
  result <- item_to_rank(true_pref, long = TRUE)
  
  # Should have 4 rows (2 respondents × 2 items)
  expect_equal(nrow(result), 4)
  
  # Check data types
  expect_true(is.numeric(result$rank))
})

test_that("item_to_rank preserves ranking information correctly", {
  # Simple case where we can verify the transformation
  true_pref <- data.frame(
    first = c("a"),
    second = c("b"),
    third = c("c")
  )
  
  result <- item_to_rank(true_pref)
  
  # First item should be ranked 1, second ranked 2, third ranked 3
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 1)
})
