test_that("permn_augment augments missing permutations with zeros", {
  # Create a table with only some permutations
  tab <- table(c(rep("123", 100), rep("321", 50)))
  
  result <- permn_augment(tab, J = 3)
  
  # Should have all 3! = 6 permutations
  expect_equal(length(result), factorial(3))
  
  # Check that augmented permutations have frequency 0
  expect_equal(result["132"], 0)
  expect_equal(result["213"], 0)
  expect_equal(result["231"], 0)
  expect_equal(result["312"], 0)
  
  # Original frequencies should be preserved
  expect_equal(result["123"], 100)
  expect_equal(result["321"], 50)
})

test_that("permn_augment infers J from input when not specified", {
  tab <- table(c("123", "321", "213"))
  
  result <- permn_augment(tab)
  
  # Should have all 6 permutations for 3 items
  expect_equal(length(result), 6)
})

test_that("permn_augment sorts permutations alphabetically", {
  tab <- table(c("321", "123", "213"))
  
  result <- permn_augment(tab, J = 3)
  
  # Check that names are sorted
  expect_equal(names(result), sort(names(result)))
  expect_equal(names(result)[1], "123")
  expect_equal(names(result)[6], "321")
})

test_that("permn_augment handles complete permutation sets", {
  # All permutations already present
  tab <- table(c("123", "132", "213", "231", "312", "321"))
  
  result <- permn_augment(tab, J = 3)
  
  # Should still have exactly 6 permutations
  expect_equal(length(result), 6)
  
  # All should have frequency 1
  expect_true(all(result == 1))
})

test_that("permn_augment works with 4 items", {
  tab <- table(c("1234", "4321"))
  
  result <- permn_augment(tab, J = 4)
  
  # Should have all 4! = 24 permutations
  expect_equal(length(result), factorial(4))
  
  # Most should be zero
  expect_equal(sum(result == 0), 22)
})

test_that("permn_augment throws error for inconsistent character lengths", {
  # Mix of 3-character and 4-character patterns
  tab <- table(c("123", "1234"))
  
  expect_error(
    permn_augment(tab, J = 3),
    "All names must have the same number of characters."
  )
})

test_that("permn_augment handles large J values", {
  # Test with J=5 (120 permutations)
  tab <- table(c("12345", "54321"))
  
  result <- permn_augment(tab, J = 5)
  
  expect_equal(length(result), factorial(5))
  expect_equal(sum(result), 2)
})
