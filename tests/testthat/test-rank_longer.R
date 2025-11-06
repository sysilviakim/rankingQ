test_that("rank_longer works with wide format multiple columns", {
  # Example from documentation
  x <- data.frame(
    apple = c(2, 1, 3),
    orange = c(1, 3, 2),
    banana = c(3, 2, 1)
  )
  
  result <- rank_longer(x)
  
  # Should have 9 rows (3 respondents × 3 items)
  expect_equal(nrow(result), 9)
  
  # Check column names
  expect_true(all(c("id", "reference_no", "item_name", "ranking") %in% names(result)))
  
  # Check that item_names match column names
  expect_true(all(c("apple", "orange", "banana") %in% result$item_name))
})

test_that("rank_longer works with single column character format", {
  y <- data.frame(
    id = c("Bernie", "Yuki", "Silvia"),
    rank = c("123", "321", "213")
  )
  
  result <- rank_longer(y, cols = "rank", id = "id")
  
  # Should have 9 rows (3 respondents × 3 items)
  expect_equal(nrow(result), 9)
  
  # Check that id column is preserved
  expect_true(all(c("Bernie", "Yuki", "Silvia") %in% result$id))
  
  # Check rankings
  expect_equal(nrow(result), 9)
})

test_that("rank_longer uses reference names when specified", {
  y <- data.frame(
    id = c("Bernie", "Yuki", "Silvia"),
    rank = c("123", "321", "213")
  )
  
  result <- rank_longer(
    y,
    cols = "rank",
    id = "id",
    reference = c("Money", "Power", "Respect")
  )
  
  # Check that reference names are used
  expect_true(all(c("Money", "Power", "Respect") %in% result$item_name))
})

test_that("rank_longer creates ID when not specified", {
  x <- data.frame(
    apple = c(2, 1, 3),
    orange = c(1, 3, 2),
    banana = c(3, 2, 1)
  )
  
  # Suppress message about creating ID
  result <- suppressMessages(rank_longer(x))
  
  # Should have an id column with row numbers
  expect_true("id" %in% names(result))
  expect_equal(unique(result$id), 1:3)
})

test_that("rank_longer throws error for non-data frame input", {
  expect_error(
    rank_longer(c(1, 2, 3)),
    "The x argument must be a data frame."
  )
})

test_that("rank_longer throws error for duplicate IDs", {
  x <- data.frame(
    id = c("A", "A", "B"),
    apple = c(2, 1, 3),
    orange = c(1, 3, 2),
    banana = c(3, 2, 1)
  )
  
  expect_error(
    rank_longer(x, id = "id"),
    "The id argument does not uniquely identify the respondent."
  )
})

test_that("rank_longer throws error when max_ranking is 1", {
  x <- data.frame(
    id = c("A", "B", "C"),
    only_one = c(1, 1, 1)
  )
  
  expect_error(
    suppressMessages(rank_longer(x, cols = "only_one", id = "id")),
    "The max_ranking argument must be greater than 1."
  )
})

test_that("rank_longer throws error for empty cols argument", {
  x <- data.frame(
    id = c("A", "B", "C")
  )
  
  expect_error(
    rank_longer(x, cols = character(0), id = "id"),
    "The cols argument must be a character vector of length 1 or greater."
  )
})

test_that("rank_longer handles 4-item rankings", {
  x <- data.frame(
    id = c("R1", "R2"),
    item1 = c(1, 4),
    item2 = c(2, 3),
    item3 = c(3, 2),
    item4 = c(4, 1)
  )
  
  result <- rank_longer(x, id = "id")
  
  # Should have 8 rows (2 respondents × 4 items)
  expect_equal(nrow(result), 8)
  
  # Check reference numbers
  expect_equal(unique(result$reference_no), 1:4)
})

test_that("rank_longer throws error when reference is shorter than rankings", {
  x <- data.frame(
    id = c("A"),
    rank = c("1234")
  )
  
  expect_error(
    suppressMessages(
      rank_longer(x, cols = "rank", id = "id", reference = c("A", "B"))
    ),
    "The max_ranking argument is greater than the length of the reference choice set."
  )
})

test_that("rank_longer produces sorted output", {
  x <- data.frame(
    id = c("Z", "A", "M"),
    apple = c(2, 1, 3),
    orange = c(1, 3, 2),
    banana = c(3, 2, 1)
  )
  
  result <- rank_longer(x, id = "id")
  
  # Check that output is sorted by id and reference_no
  # IDs should be in order Z, A, M (preserving input order within each id group)
  ids_in_order <- result$id[seq(1, nrow(result), by = 3)]
  expect_equal(ids_in_order, c("Z", "A", "M"))
  
  # Within each ID, reference_no should be 1, 2, 3
  for (id_val in c("Z", "A", "M")) {
    refs <- result$reference_no[result$id == id_val]
    expect_equal(refs, 1:3)
  }
})
