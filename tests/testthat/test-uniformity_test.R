test_that("uniformity_test works with table input", {
  # Create a table with some non-uniform distribution
  tab <- table(c(rep("123", 100), rep("321", 50), rep("213", 25)))
  
  result <- uniformity_test(tab)
  
  # Should return a htest object (chi-square test result)
  expect_s3_class(result, "htest")
  
  # Should have expected components
  expect_true("statistic" %in% names(result))
  expect_true("p.value" %in% names(result))
})

test_that("uniformity_test works with data frame and variable", {
  # Create a data frame
  df <- data.frame(
    id = 1:150,
    ranking = c(rep("123", 100), rep("321", 50))
  )
  
  result <- uniformity_test(df, var = "ranking")
  
  expect_s3_class(result, "htest")
  expect_true("p.value" %in% names(result))
})

test_that("uniformity_test augments permutations", {
  # Table with only 2 permutations out of 6 possible
  tab <- table(c(rep("123", 50), rep("321", 50)))
  
  result <- uniformity_test(tab)
  
  # Test should still run with augmented permutations
  expect_s3_class(result, "htest")
  
  # The test is against uniform distribution across all 6 permutations
  expect_equal(result$parameter, 5)  # df = 6 - 1
})

test_that("uniformity_test detects non-uniform distribution", {
  # Highly non-uniform: one pattern dominates
  tab <- table(c(rep("123", 1000), rep("321", 10), rep("213", 5)))
  
  result <- uniformity_test(tab)
  
  # Should have very small p-value
  expect_true(result$p.value < 0.05)
})

test_that("uniformity_test with uniform distribution", {
  # Create approximately uniform distribution across all permutations
  set.seed(123)
  all_perms <- c("123", "132", "213", "231", "312", "321")
  rankings <- sample(all_perms, 600, replace = TRUE)
  tab <- table(rankings)
  
  result <- uniformity_test(tab)
  
  # With uniform distribution, p-value should be relatively high
  # (though this is stochastic, so we use a lenient threshold)
  expect_true(result$p.value > 0.01)
})

test_that("uniformity_test throws error for non-character var", {
  df <- data.frame(ranking = c("123", "321"))
  
  expect_error(
    uniformity_test(df, var = 123),
    "The 'var' argument must be a character string."
  )
})

test_that("uniformity_test throws error when var not specified for non-table", {
  df <- data.frame(ranking = c("123", "321"))
  
  expect_error(
    uniformity_test(df),
    "If the data is not already in a table format, please specify the variable to be used in the test."
  )
})

test_that("uniformity_test throws error when var not in data", {
  df <- data.frame(ranking = c("123", "321"))
  
  expect_error(
    uniformity_test(df, var = "nonexistent"),
    "The variable specified is not in the data."
  )
})

test_that("uniformity_test works with 4-item rankings", {
  # Create table with 4-item rankings
  set.seed(456)
  # Sample from some 4-item permutations
  rankings <- sample(c("1234", "4321", "2341", "1324"), 200, replace = TRUE)
  tab <- table(rankings)
  
  result <- uniformity_test(tab)
  
  expect_s3_class(result, "htest")
  # Should test against all 24 permutations
  expect_equal(result$parameter, 23)  # df = 24 - 1
})

test_that("uniformity_test returns chi-square test method", {
  tab <- table(c("123", "321", "213"))
  
  result <- uniformity_test(tab)
  
  # Should be a chi-square test
  expect_match(result$method, "Chi-squared test")
})

test_that("uniformity_test works with single ranking pattern", {
  # Everyone gives the same ranking
  tab <- table(rep("123", 100))
  
  result <- uniformity_test(tab)
  
  # Should strongly reject uniformity
  expect_true(result$p.value < 0.001)
})

test_that("uniformity_test with tibble input", {
  df <- tibble::tibble(
    id = 1:100,
    ranking = sample(c("123", "321", "213"), 100, replace = TRUE)
  )
  
  result <- uniformity_test(df, var = "ranking")
  
  expect_s3_class(result, "htest")
})
