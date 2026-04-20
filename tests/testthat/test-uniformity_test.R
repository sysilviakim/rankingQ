# Test uniformity_test function

test_that("uniformity_test works with table input", {
  # Create a uniform-ish table
  tab <- table(c(
    rep("123", 10), rep("132", 10), rep("213", 10),
    rep("231", 10), rep("312", 10), rep("321", 10)
  ))
  result <- uniformity_test(tab)
  expect_s3_class(result, "htest")
  expect_equal(result$statistic[[1]], 0) # Should be 0 for uniform
})

test_that("uniformity_test warns when var is given with table input", {
  tab <- table(c(
    rep("123", 10), rep("132", 10), rep("213", 10),
    rep("231", 10), rep("312", 10), rep("321", 10)
  ))

  expect_warning(
    result <- uniformity_test(tab, var = "ranking"),
    "ignored when 'data' is already a table"
  )
  expect_s3_class(result, "htest")
})

test_that("uniformity_test works with data frame input", {
  df <- data.frame(ranking = c(rep("123", 50), rep("321", 50)))
  result <- uniformity_test(df, var = "ranking")
  expect_s3_class(result, "htest")
  # Non-uniform distribution should have p-value < 0.05
  expect_true(result$p.value < 0.05)
})

test_that("uniformity_test throws error for invalid input", {
  df <- data.frame(other_col = c("a", "b", "c"))
  expect_error(
    uniformity_test(df, var = "ranking"),
    "The variable specified is not in the data."
  )
  expect_error(
    uniformity_test(df),
    "please specify the variable"
  )
  expect_error(
    uniformity_test(df, var = 123),
    "must be a character string"
  )
})

test_that("uniformity_test works with tibble input for 4-item rankings", {
  df <- tibble::tibble(
    ranking = c(rep("1234", 10), rep("4321", 10), rep("2143", 10), rep("3412", 10))
  )

  result <- suppressWarnings(uniformity_test(df, var = "ranking"))

  expect_s3_class(result, "htest")
  expect_match(result$method, "Chi-squared")
})
