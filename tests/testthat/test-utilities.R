# Test utility functions

test_that("ordinal_seq generates correct ordinal sequences", {
  result <- ordinal_seq(4)
  expect_equal(result, c("1st", "2nd", "3rd", "4th"))

  result_5 <- ordinal_seq(5)
  expect_equal(result_5, c("1st", "2nd", "3rd", "4th", "5th"))
  expect_equal(ordinal_seq(1), "1st")
})

test_that("table_to_tibble converts table correctly", {
  tab <- table(c(rep("A", 10), rep("B", 20), rep("C", 30)))
  result <- table_to_tibble(tab)

  expect_s3_class(result, "data.frame")
  expect_true("prop" %in% names(result))
  expect_equal(sum(result$prop), 1, tolerance = 1e-10)
})

test_that("table_to_tibble with tibble=FALSE returns plain data.frame", {
  tab <- table(c(rep("A", 10), rep("B", 20)))
  result <- table_to_tibble(tab, tibble = FALSE)

  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "tbl_df"))
  expect_true(all(c("ranking", "freq", "prop") %in% names(result)))
})

test_that("ordinal_seq handles teen numbers with 'th' suffix", {
  result <- ordinal_seq(13)
  expect_equal(result[11], "11th")
  expect_equal(result[12], "12th")
  expect_equal(result[13], "13th")
})

test_that("ordinal_seq handles suffix changes after 20 and 110", {
  result <- ordinal_seq(25)
  expect_equal(result[21:24], c("21st", "22nd", "23rd", "24th"))

  result_large <- ordinal_seq(115)
  expect_equal(result_large[111:113], c("111th", "112th", "113th"))
})

test_that("ordinal_seq errors on invalid input", {
  expect_error(
    ordinal_seq(c(1, 2)),
    "`length` must be a numeric value of length 1."
  )
  expect_error(
    ordinal_seq("a"),
    "`length` must be a numeric value of length 1."
  )
  expect_error(
    ordinal_seq(NULL),
    "`length` must be a numeric value of length 1."
  )
})

test_that("table_to_tibble keeps factor rankings and exact proportions", {
  tab <- table(c(rep("123", 60), rep("321", 30), rep("213", 10)))
  result <- table_to_tibble(tab)
  props <- setNames(result$prop, as.character(result$ranking))

  expect_true(is.factor(result$ranking))
  expect_equal(unname(props[c("123", "321", "213")]), c(0.6, 0.3, 0.1))
})

test_that("table_to_tibble handles single-element tables", {
  result <- table_to_tibble(table("123"))

  expect_equal(nrow(result), 1)
  expect_equal(result$freq, 1)
  expect_equal(result$prop, 1)
})
