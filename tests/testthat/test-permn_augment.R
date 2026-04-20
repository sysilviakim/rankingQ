# Test permn_augment function

test_that("permn_augment adds missing permutations", {
  # Only 2 of 6 possible permutations
  tab <- table(c(rep("123", 100), rep("321", 50)))
  result <- permn_augment(tab, J = 3)

  # Should have all 6 permutations
  expect_equal(length(result), 6)
  expect_true(
    all(c("123", "132", "213", "231", "312", "321") %in% names(result))
  )

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
  result <- permn_augment(tab) # J not specified

  expect_equal(length(result), 6) # 3! = 6
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
  expect_error(permn_augment(tab), "must imply the same J")
})

test_that("permn_augment handles delimiter-separated labels", {
  tab <- table(c(rep("1,2,3", 2), "3,2,1"))
  result <- permn_augment(tab, J = 3)

  expect_equal(length(result), 6)
  expect_true(
    all(c("1,2,3", "1,3,2", "2,1,3", "2,3,1", "3,1,2", "3,2,1") %in% names(result))
  )
  expect_equal(result["1,2,3"], c("1,2,3" = 2))
  expect_equal(result["1,3,2"], c("1,3,2" = 0))
})

test_that("compact multi-digit permutation labels can be parsed unambiguously", {
  parsed <- rankingQ:::.parse_permutation_label("12345678910")

  expect_equal(parsed$J, 10L)
  expect_identical(parsed$style, "compact_multi")
  expect_equal(parsed$values, 1:10)
  expect_equal(
    rankingQ:::.format_permutation_values(parsed$values),
    paste(1:10, collapse = "|")
  )
})
