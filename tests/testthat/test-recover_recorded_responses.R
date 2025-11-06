test_that("recover_recorded_responses works with basic example", {
  # Example from documentation: true ranking A-B-C-D, presented as B-A-D-C
  result <- recover_recorded_responses(true_order = "1234", presented_order = "2143")
  expect_equal(result, "2143")
})

test_that("recover_recorded_responses with reverse order", {
  # True ranking D-C-B-A, presented as A-B-C-D
  result <- recover_recorded_responses(true_order = "4321", presented_order = "1234")
  expect_equal(result, "4321")
})

test_that("recover_recorded_responses with mixed order", {
  # Example from documentation: true C-A-D-B, presented D-C-B-A
  result <- recover_recorded_responses(true_order = "2413", presented_order = "4321")
  expect_equal(result, "3142")
})

test_that("recover_recorded_responses with identity order", {
  # When presented order is the same as true order
  result <- recover_recorded_responses(true_order = "1234", presented_order = "1234")
  expect_equal(result, "1234")
})

test_that("recover_recorded_responses works with 3 items", {
  # Three-item ranking
  result <- recover_recorded_responses(true_order = "321", presented_order = "132")
  
  # Check that result has 3 characters
  expect_equal(nchar(result), 3)
  
  # Check that result contains only 1, 2, 3
  chars <- strsplit(result, "")[[1]]
  expect_true(all(chars %in% c("1", "2", "3")))
})

test_that("recover_recorded_responses works with data frame", {
  df <- data.frame(
    id = c(1, 2),
    ranking_row_rnd = c("2143", "1234"),
    ranking = c("1234", "4321")
  )
  
  result <- recover_recorded_responses(
    true_order = "ranking",
    presented_order = "ranking_row_rnd",
    df = df
  )
  
  # Should add a new column with _recorded suffix
  expect_true("ranking_recorded" %in% names(result))
  
  # Check the recovered responses
  expect_equal(result$ranking_recorded[1], "2143")
  expect_equal(result$ranking_recorded[2], "4321")
})

test_that("recover_recorded_responses throws error for missing column in df", {
  df <- data.frame(
    id = c(1, 2),
    ranking = c("1234", "4321")
  )
  
  expect_error(
    recover_recorded_responses(
      true_order = "ranking",
      presented_order = "nonexistent",
      df = df
    ),
    "Presented order variable is not in the dataframe."
  )
  
  expect_error(
    recover_recorded_responses(
      true_order = "nonexistent",
      presented_order = "ranking",
      df = df
    ),
    "Response order variable is not in the dataframe."
  )
})

test_that("recover_recorded_responses handles NA values in df", {
  df <- data.frame(
    id = c(1, 2, 3),
    ranking_row_rnd = c("2143", "1234", "1234"),
    ranking = c("1234", "4321", NA)
  )
  
  result <- recover_recorded_responses(
    true_order = "ranking",
    presented_order = "ranking_row_rnd",
    df = df
  )
  
  # NA should be preserved
  expect_true(is.na(result$ranking_recorded[3]))
  
  # Non-NA values should be computed correctly
  expect_equal(result$ranking_recorded[1], "2143")
  expect_equal(result$ranking_recorded[2], "4321")
})

test_that("recover_recorded_responses returns string", {
  result <- recover_recorded_responses(true_order = "123", presented_order = "321")
  expect_true(is.character(result))
  expect_equal(length(result), 1)
})

test_that("recover_recorded_responses with 5 items", {
  result <- recover_recorded_responses(
    true_order = "12345",
    presented_order = "54321"
  )
  
  # Should return a 5-character string
  expect_equal(nchar(result), 5)
})

test_that("recover_recorded_responses preserves all digits", {
  true_order <- "2413"
  presented_order <- "1234"
  
  result <- recover_recorded_responses(true_order = true_order, presented_order = presented_order)
  
  # Result should contain all digits 1-4
  chars <- strsplit(result, "")[[1]]
  expect_setequal(chars, c("1", "2", "3", "4"))
})

test_that("recover_recorded_responses with data frame multiple rows", {
  df <- data.frame(
    id = 1:5,
    app_identity_row_rnd = c("1234", "2143", "3412", "4321", "1324"),
    app_identity = c("1234", "1234", "1234", "1234", "1234")
  )
  
  result <- recover_recorded_responses(
    true_order = "app_identity",
    presented_order = "app_identity_row_rnd",
    df = df
  )
  
  expect_true("app_identity_recorded" %in% names(result))
  expect_equal(nrow(result), 5)
  
  # First row: presented=1234, true=1234 -> recorded=1234
  expect_equal(result$app_identity_recorded[1], "1234")
})
