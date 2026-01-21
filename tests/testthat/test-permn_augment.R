# Test permn_augment function

test_that("permn_augment adds missing permutations", {
  # Only 2 of 6 possible permutations
  tab <- table(c(rep("123", 100), rep("321", 50)))
  result <- permn_augment(tab, J = 3)

  # Should have all 6 permutations
  expect_equal(length(result), 6)
  expect_true(all(c("123", "132", "213", "231", "312", "321") %in% names(result)))

  # Missing permutations should have 0 count
  expect_equal(result["132"], c("132" = 0))
  expect_equal(result["213"], c("213" = 0))
})

test_that("permn_augment preserves existing counts", {
  tab <- table(c(rep("123", 100), rep("321", 50)))
  result <- permn_augment(tab, J = 3)

  expect_equal(result["123"], c("123" = 100))
  expect_equal(result["321"], c("321" = 50))
})

test_that("permn_augment infers J from data", {
  tab <- table(c("123", "321"))
  result <- permn_augment(tab)  # J not specified

  expect_equal(length(result), 6)  # 3! = 6
})

test_that("permn_augment handles complete data", {
  # All permutations already present
  tab <- table(c("123", "132", "213", "231", "312", "321"))
  result <- permn_augment(tab, J = 3)

  expect_equal(length(result), 6)
  expect_true(all(result == 1))
})

test_that("permn_augment errors on inconsistent lengths", {
  tab <- table(c("123", "12"))
  expect_error(permn_augment(tab), "same number of characters")
})
