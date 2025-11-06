test_that("unbiased_correct_prop computes correct adjustment", {
  # Example from documentation
  result <- unbiased_correct_prop(0.7, 3)
  
  # Should be a number between 0 and 1
  expect_true(result >= 0 && result <= 1)
  
  # Manual calculation: (0.7 - 1/6) / (1 - 1/6) = 0.533333... / 0.833333... ≈ 0.64
  expected <- (0.7 - 1/factorial(3)) / (1 - 1/factorial(3))
  expect_equal(result, expected)
})

test_that("unbiased_correct_prop handles different values of J", {
  # Test with J = 2
  result_2 <- unbiased_correct_prop(0.8, 2)
  expected_2 <- (0.8 - 1/2) / (1 - 1/2)
  expect_equal(result_2, expected_2)
  
  # Test with J = 4
  result_4 <- unbiased_correct_prop(0.6, 4)
  expected_4 <- (0.6 - 1/factorial(4)) / (1 - 1/factorial(4))
  expect_equal(result_4, expected_4)
})

test_that("unbiased_correct_prop handles edge cases", {
  # When raw proportion equals random chance
  result <- unbiased_correct_prop(1/factorial(3), 3)
  expect_equal(result, 0)
  
  # When raw proportion is 1 (perfect)
  result_perfect <- unbiased_correct_prop(1, 3)
  expect_equal(result_perfect, 1)
})

test_that("unbiased_correct_prop with various mean_c values", {
  J <- 3
  
  # Test multiple values
  for (mean_c in c(0.3, 0.5, 0.7, 0.9)) {
    result <- unbiased_correct_prop(mean_c, J)
    expected <- (mean_c - 1/factorial(J)) / (1 - 1/factorial(J))
    expect_equal(result, expected)
  }
})

test_that("unbiased_correct_prop returns numeric", {
  result <- unbiased_correct_prop(0.7, 3)
  expect_true(is.numeric(result))
  expect_equal(length(result), 1)
})

test_that("unbiased_correct_prop with large J", {
  # With large J, 1/factorial(J) becomes very small
  result <- unbiased_correct_prop(0.8, 5)
  
  # Should be close to 0.8 when random chance is negligible
  expect_true(result > 0.79)
  expect_true(result < 0.81)
})

test_that("unbiased_correct_prop formula consistency", {
  # Verify the formula: (mean_c - 1/J!) / (1 - 1/J!)
  mean_c <- 0.65
  J <- 4
  
  result <- unbiased_correct_prop(mean_c, J)
  
  numerator <- mean_c - (1 / factorial(J))
  denominator <- 1 - (1 / factorial(J))
  expected <- numerator / denominator
  
  expect_equal(result, expected)
})
