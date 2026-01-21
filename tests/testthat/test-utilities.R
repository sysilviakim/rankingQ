# Test utility functions

test_that("ordinal_seq generates correct ordinal sequences", {
  result <- ordinal_seq(4)
  expect_equal(result, c("1st", "2nd", "3rd", "4th"))

  result_5 <- ordinal_seq(5)
  expect_equal(result_5, c("1st", "2nd", "3rd", "4th", "5th"))
})

test_that("table_to_tibble converts table correctly", {
  tab <- table(c(rep("A", 10), rep("B", 20), rep("C", 30)))
  result <- table_to_tibble(tab)

  expect_s3_class(result, "data.frame")
  expect_true("prop" %in% names(result))
  expect_equal(sum(result$prop), 1, tolerance = 1e-10)
})
