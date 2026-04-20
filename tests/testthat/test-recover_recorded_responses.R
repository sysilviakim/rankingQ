# Test recover_recorded_responses function

test_that(
  "recover_recorded_responses works with string inputs (examples from docs)",
  {
  # A-B-C-D true ranking, items presented as B-A-D-C -> recorded "2143"
  expect_equal(recover_recorded_responses("1234", "2143"), "2143")

  # D-C-B-A true ranking, items presented as A-B-C-D -> recorded "4321"
  expect_equal(recover_recorded_responses("4321", "1234"), "4321")

  # C-A-D-B true ranking, items presented as D-C-B-A -> recorded "3142"
  expect_equal(recover_recorded_responses("2413", "4321"), "3142")
  }
)

test_that(
  "recover_recorded_responses: identity presentation yields same as true",
  {
  # When presented in natural order, recorded response equals true ranking
  expect_equal(recover_recorded_responses("1234", "1234"), "1234")
  expect_equal(recover_recorded_responses("3214", "1234"), "3214")
  }
)

test_that("recover_recorded_responses handles delimiter-separated rankings for J greater than 9", {
  true_order <- paste(10:1, collapse = "|")
  presented_order <- paste(c(2, 1, 4:10, 3), collapse = "|")

  expect_equal(
    recover_recorded_responses(true_order, presented_order),
    paste(c(9, 10, 7:1, 8), collapse = "|")
  )
})

test_that("recover_recorded_responses handles item-label inputs directly", {
  expect_equal(
    recover_recorded_responses("CADB", "DCBA"),
    "3142"
  )

  expect_equal(
    recover_recorded_responses(
      "cherry|apple|date|banana",
      "date|cherry|banana|apple"
    ),
    "3142"
  )
})

test_that("recover_recorded_responses supports explicit reference order for mixed inputs", {
  reference <- c("A", "B", "C", "D")

  expect_equal(
    recover_recorded_responses("2413", "D|C|B|A", reference = reference),
    "3142"
  )

  expect_equal(
    recover_recorded_responses("CADB", "4321", reference = reference),
    "3142"
  )
})

test_that("recover_recorded_responses requires reference for mixed numeric and label inputs", {
  expect_error(
    recover_recorded_responses("2413", "DCBA"),
    "reference must be supplied when mixing numeric position codes with item-label inputs."
  )

  expect_error(
    recover_recorded_responses("CADB", "4321"),
    "reference must be supplied when mixing numeric position codes with item-label inputs."
  )
})

test_that("recover_recorded_responses errors when df columns are missing", {
  df <- data.frame(a = "1234", b = "2143", stringsAsFactors = FALSE)

  expect_error(
    recover_recorded_responses("a", "missing_col", df = df),
    "Presented order variable is not in the dataframe."
  )
  expect_error(
    recover_recorded_responses("missing_col", "b", df = df),
    "Response order variable is not in the dataframe."
  )
})

test_that("recover_recorded_responses works with df input", {
  df <- data.frame(
    app_identity_row_rnd = c("2143", "1234"),
    app_identity         = c("1234", "4321"),
    stringsAsFactors     = FALSE
  )
  result <- recover_recorded_responses(
    "app_identity", "app_identity_row_rnd",
    df = df
  )

  expect_true("app_identity_recorded" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("recover_recorded_responses works with df input and explicit reference labels", {
  df <- data.frame(
    truth = c("CADB", "DBCA"),
    presented = c("DCBA", "ABCD"),
    stringsAsFactors = FALSE
  )

  result <- recover_recorded_responses(
    "truth",
    "presented",
    df = df,
    reference = c("A", "B", "C", "D")
  )

  expect_true("presented_recorded" %in% names(result))
  expect_equal(result$presented_recorded, c("3142", "4231"))
})

test_that("recover_recorded_responses preserves source columns for generic names", {
  df <- data.frame(
    truth = "2413",
    presented = "4321",
    stringsAsFactors = FALSE
  )

  result <- recover_recorded_responses("truth", "presented", df = df)

  expect_true("presented_recorded" %in% names(result))
  expect_equal(result$presented, "4321")
  expect_equal(result$presented_recorded, "3142")
})

test_that("recover_recorded_responses errors when output column already exists", {
  df <- data.frame(
    truth = "2413",
    presented = "4321",
    presented_recorded = "stale",
    stringsAsFactors = FALSE
  )

  expect_error(
    recover_recorded_responses("truth", "presented", df = df),
    "Output column 'presented_recorded' already exists in the dataframe."
  )
})

test_that("recover_recorded_responses handles NA in df input", {
  df <- data.frame(
    app_row_rnd      = c("1234", NA),
    app_true         = c("1234", NA),
    stringsAsFactors = FALSE
  )
  result <- recover_recorded_responses("app_true", "app_row_rnd", df = df)
  expect_true(is.na(result$app_recorded[2]))
})

test_that("recover_recorded_responses handles empty df input", {
  df <- data.frame(
    app_row_rnd      = character(0),
    app_true         = character(0),
    stringsAsFactors = FALSE
  )

  result <- recover_recorded_responses("app_true", "app_row_rnd", df = df)

  expect_true("app_recorded" %in% names(result))
  expect_equal(nrow(result), 0)
  expect_identical(result$app_recorded, character(0))
})

test_that("recover_recorded_responses validates malformed string indices", {
  expect_error(
    recover_recorded_responses("1234", "21a4"),
    "presented_order must contain only numeric position codes."
  )
  expect_error(
    recover_recorded_responses("1234", "2145"),
    "presented_order contains indices outside the range of true_order."
  )
})

test_that("recover_recorded_responses validates malformed df indices", {
  df <- data.frame(
    app_row_rnd      = "2145",
    app_true         = "1234",
    stringsAsFactors = FALSE
  )

  expect_error(
    recover_recorded_responses("app_true", "app_row_rnd", df = df),
    "Row 1: presented_order contains indices outside the range of true_order."
  )
})
