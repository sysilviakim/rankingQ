# Test unbiased_correct_prop function

test_that("unbiased_correct_prop computes correctly", {
  # With 100% correct and J=4, should give 1
  result <- unbiased_correct_prop(1.0, J = 4)
  expect_equal(result, 1, tolerance = 1e-10)

  # With random guessing (1/J! correct), should give 0
  result_random <- unbiased_correct_prop(1/24, J = 4)
  expect_equal(result_random, 0, tolerance = 1e-10)
})

test_that("unbiased_correct_prop handles identity data", {
  identity <- rankingQ::identity
  prop_correct <- mean(identity$anc_correct_identity)

  result <- unbiased_correct_prop(prop_correct, J = 4)

  # Should be between 0 and 1
  expect_true(result >= 0 && result <= 1)

  # Should be close to the value from the paper (~0.684)
  expect_equal(result, 0.684, tolerance = 0.01)
})
