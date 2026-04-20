df_basic <- data.frame(
  id = c("Bernie", "Yuki", "Silvia"),
  rank = c("123", "321", "213")
)

df_long <- data.frame(
  item = c("A", "B", "C", "A", "B", "C"),
  rank = c(3L, 1L, 2L, 1L, 2L, 3L)
)

test_that("Basic one-column data without item names specified", {
  result <- avg_rank(df_basic, "rank")
  expect_equal(nrow(result), 3)
  expect_equal(result$item, c("1st", "2nd", "3rd"))
  expect_equal(result$mean, c(2, 5 / 3, 7 / 3))
})

test_that("Basic one-column data with item names specified", {
  result <- avg_rank(df_basic, "rank", items = c("Money", "Power", "Respect"))
  expect_equal(nrow(result), 3)
  expect_equal(
    result$item,
    factor(
      c("Money", "Power", "Respect"),
      levels = c("Money", "Power", "Respect")
    )
  )
})

test_that("Long-format data.", {
  result <- avg_rank(df_long, "rank", items = "item", long = TRUE)
  expect_equal(nrow(result), 3)
  expect_equal(result$item, c("A", "B", "C"))
  expect_equal(result$mean, c(2.0, 1.5, 2.5))
  expect_equal(result$se, c(1.0, 0.5, 0.5))
})

test_that("Wide-format item labels must match the number of ranked items", {
  df_four <- data.frame(rank = c("1234", "2143"))

  expect_error(
    avg_rank(df_four, "rank", items = c("A", "B", "C")),
    "The number of reference choice set's elements in the items variable does not match the number of items ranked."
  )
})

test_that("IPW avg_rank accepts item mappings as a data frame", {
  identity_w <- rankingQ::identity_w
  items_df <- data.frame(
    variable = paste0("app_identity_", 1:4),
    item = c("Party", "Religion", "Gender", "Race")
  )

  result <- avg_rank(
    identity_w,
    items = items_df,
    weight = "weights",
    raw = FALSE
  )

  expect_equal(nrow(result), 4)
  expect_equal(as.character(result$item), items_df$item)
})

test_that("avg_rank parses delimiter-separated rankings for J greater than 9", {
  df_ten <- data.frame(
    rank = c(
      paste(1:10, collapse = "|"),
      paste(10:1, collapse = "|")
    )
  )

  result <- avg_rank(df_ten, "rank")

  expect_equal(nrow(result), 10)
  expect_equal(as.character(result$item), ordinal_seq(10))
  expect_equal(result$mean, rep(5.5, 10))
})

test_that("avg_rank ignores unrelated duplicate output columns", {
  identity_w <- rankingQ::identity_w
  identity_w[["ranking_copy"]] <- identity_w[["ranking"]]
  names(identity_w)[names(identity_w) == "ranking_copy"] <- "ranking"

  raw_result <- avg_rank(
    identity_w,
    rankings = "app_identity",
    items = c("Party", "Religion", "Gender", "Race")
  )

  items_df <- data.frame(
    variable = paste0("app_identity_", 1:4),
    item = c("Party", "Religion", "Gender", "Race")
  )
  ipw_result <- avg_rank(
    identity_w,
    items = items_df,
    weight = "weights",
    raw = FALSE
  )

  expect_equal(nrow(raw_result), 4)
  expect_equal(as.character(raw_result$item), items_df$item)
  expect_equal(nrow(ipw_result), 4)
  expect_equal(as.character(ipw_result$item), items_df$item)
})

test_that("Error is thrown for incorrect arguments", {
  ## Writing tests for only a few cases
  expect_error(
    avg_rank(data.frame()),
    "The rankings variable must be specified."
  )
  expect_error(
    avg_rank(df_long, "invalid_col"),
    "The rankings variable is not contained in the given data frame."
  )
  expect_error(
    avg_rank(df_long, long = 3),
    "The 'long' argument must be either TRUE or FALSE."
  )
  expect_error(
    avg_rank(df_long, raw = "yes"),
    "The 'raw' argument must be either TRUE or FALSE."
  )
})
