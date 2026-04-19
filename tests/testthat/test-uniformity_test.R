# Test uniformity_test function

test_that("uniformity_test works with table input", {
  # Create a uniform-ish table
  tab <- table(c(rep("123", 10), rep("132", 10), rep("213", 10),
                 rep("231", 10), rep("312", 10), rep("321", 10)))
  result <- uniformity_test(tab)
  expect_s3_class(result, "htest")
  expect_equal(result$statistic[[1]], 0)  # Should be 0 for uniform
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
  expect_error(uniformity_test(df, var = "ranking"),
               "The variable specified is not in the data.")
  expect_error(uniformity_test(df),
               "please specify the variable")
  expect_error(uniformity_test(df, var = 123),
               "must be a character string")
})
