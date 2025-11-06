test_that("ordinal_seq generates correct sequences", {
  # Test basic sequences
  expect_equal(ordinal_seq(3), c("1st", "2nd", "3rd"))
  expect_equal(ordinal_seq(5), c("1st", "2nd", "3rd", "4th", "5th"))
  
  # Test special cases (11th, 12th, 13th)
  result <- ordinal_seq(15)
  expect_equal(result[11], "11th")
  expect_equal(result[12], "12th")
  expect_equal(result[13], "13th")
  
  # Test longer sequences
  result_long <- ordinal_seq(25)
  expect_equal(length(result_long), 25)
  expect_equal(result_long[21], "21st")
  expect_equal(result_long[22], "22nd")
  expect_equal(result_long[23], "23rd")
  expect_equal(result_long[24], "24th")
  
  # Test edge case for 111th, 112th, 113th (if testing large sequences)
  result_large <- ordinal_seq(115)
  expect_equal(result_large[111], "111th")
  expect_equal(result_large[112], "112th")
  expect_equal(result_large[113], "113th")
})

test_that("ordinal_seq handles length argument correctly", {
  # Test length 1
  expect_equal(ordinal_seq(1), "1st")
  
  # Test length 10
  expect_equal(length(ordinal_seq(10)), 10)
})

test_that("ordinal_seq throws errors for invalid inputs", {
  # Non-numeric input
  expect_error(ordinal_seq("3"), "`length` must be a numeric value of length 1.")
  
  # Multiple values
  expect_error(ordinal_seq(c(1, 2, 3)), "`length` must be a numeric value of length 1.")
  
  # NULL input
  expect_error(ordinal_seq(NULL), "`length` must be a numeric value of length 1.")
})
