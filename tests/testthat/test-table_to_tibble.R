test_that("table_to_tibble converts table to tibble correctly", {
  # Create a simple table
  tab <- table(c("123", "321", "213", "123", "321"))
  
  result <- table_to_tibble(tab)
  
  # Check that it returns a tibble by default
  expect_s3_class(result, "tbl_df")
  
  # Check column names
  expect_true(all(c("ranking", "freq", "prop") %in% names(result)))
  
  # Check that ranking is a factor
  expect_true(is.factor(result$ranking))
  
  # Check that frequencies are correct
  expect_equal(sum(result$freq), 5)
  
  # Check that proportions sum to 1
  expect_equal(sum(result$prop), 1)
})

test_that("table_to_tibble returns data frame when tibble=FALSE", {
  tab <- table(c("123", "321", "213"))
  
  result <- table_to_tibble(tab, tibble = FALSE)
  
  # Check that it returns a data.frame but not a tibble
  expect_s3_class(result, "data.frame")
  expect_false("tbl_df" %in% class(result))
})

test_that("table_to_tibble calculates proportions correctly", {
  # Create a table with known frequencies
  tab <- table(c(rep("123", 60), rep("321", 30), rep("213", 10)))
  
  result <- table_to_tibble(tab)
  
  # Check proportions
  expect_equal(result$prop[result$ranking == "123"], 0.6)
  expect_equal(result$prop[result$ranking == "321"], 0.3)
  expect_equal(result$prop[result$ranking == "213"], 0.1)
})

test_that("table_to_tibble handles single element table", {
  tab <- table(c("123"))
  
  result <- table_to_tibble(tab)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$freq, 1)
  expect_equal(result$prop, 1)
})

test_that("table_to_tibble handles complex ranking patterns", {
  # Test with more complex patterns
  patterns <- c("1234", "4321", "2341", "1234", "4321")
  tab <- table(patterns)
  
  result <- table_to_tibble(tab)
  
  expect_equal(nrow(result), 3)  # Three unique patterns
  expect_equal(sum(result$freq), 5)
  expect_equal(sum(result$prop), 1)
})
