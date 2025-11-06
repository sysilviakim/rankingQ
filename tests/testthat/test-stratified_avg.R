test_that("stratified_avg throws error for non-character var_stratum", {
  identity <- rankingQ::identity
  
  expect_error(
    stratified_avg(
      identity,
      var_stratum = 123,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 10
    ),
    "var_stratum must be a character."
  )
})

test_that("stratified_avg throws error for non-character main_q", {
  # Create a simple stratification variable
  identity <- rankingQ::identity
  identity$stratum <- sample(c("A", "B"), nrow(identity), replace = TRUE)
  
  expect_error(
    stratified_avg(
      identity,
      var_stratum = "stratum",
      J = 4,
      main_q = 123,
      anc_correct = "anc_correct_identity",
      n_bootstrap = 10
    ),
    "main_q must be a character."
  )
})

test_that("stratified_avg throws error for non-character anc_correct", {
  identity <- rankingQ::identity
  identity$stratum <- sample(c("A", "B"), nrow(identity), replace = TRUE)
  
  expect_error(
    stratified_avg(
      identity,
      var_stratum = "stratum",
      J = 4,
      main_q = "app_identity",
      anc_correct = 123,
      n_bootstrap = 10
    ),
    "main_q must be a character."
  )
})

test_that("stratified_avg infers J from data", {
  # Create a stratification variable
  identity <- rankingQ::identity
  identity$stratum <- sample(c("Group1", "Group2"), nrow(identity), replace = TRUE)
  
  # This should work without specifying J
  result <- stratified_avg(
    identity,
    var_stratum = "stratum",
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 5,
    seed = 123,
    verbose = FALSE
  )
  
  # Should return a data frame
  expect_s3_class(result, "data.frame")
})

test_that("stratified_avg with specified J", {
  identity <- rankingQ::identity
  identity$stratum <- sample(c("A", "B"), nrow(identity), replace = TRUE)
  
  result <- stratified_avg(
    identity,
    var_stratum = "stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 5,
    seed = 123
  )
  
  expect_s3_class(result, "data.frame")
  
  # Should have mean and item columns
  expect_true("mean" %in% names(result))
  expect_true("item" %in% names(result))
})

test_that("stratified_avg with custom labels", {
  identity <- rankingQ::identity
  identity$stratum <- sample(c("A", "B"), nrow(identity), replace = TRUE)
  
  result <- stratified_avg(
    identity,
    var_stratum = "stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    labels = c("Party", "Religion", "Gender", "Race"),
    n_bootstrap = 5,
    seed = 123
  )
  
  expect_s3_class(result, "data.frame")
  
  # Items should use custom labels
  expect_true(all(c("Party", "Religion", "Gender", "Race") %in% result$item))
})

test_that("stratified_avg with survey weights", {
  identity <- rankingQ::identity
  identity$stratum <- sample(c("A", "B"), nrow(identity), replace = TRUE)
  identity$weight <- identity$s_weight
  
  result <- stratified_avg(
    identity,
    var_stratum = "stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    weight = "weight",
    n_bootstrap = 5,
    seed = 123
  )
  
  expect_s3_class(result, "data.frame")
})

test_that("stratified_avg with IPW method", {
  identity <- rankingQ::identity
  identity$stratum <- sample(c("A", "B"), nrow(identity), replace = TRUE)
  
  result <- stratified_avg(
    identity,
    var_stratum = "stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 5,
    ipw = TRUE,
    seed = 123
  )
  
  expect_s3_class(result, "data.frame")
})

test_that("stratified_avg produces multiple bootstrap samples", {
  identity <- rankingQ::identity
  identity$stratum <- sample(c("A", "B"), nrow(identity), replace = TRUE)
  
  n_boot <- 10
  result <- stratified_avg(
    identity,
    var_stratum = "stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = n_boot,
    seed = 123
  )
  
  # With 4 items and n_boot bootstraps, should have 4 * n_boot rows
  expect_equal(nrow(result), 4 * n_boot)
})

test_that("stratified_avg with verbose output", {
  identity <- rankingQ::identity
  identity$stratum <- sample(c("A", "B"), nrow(identity), replace = TRUE)
  
  # This should run without error (verbose just affects console output)
  result <- stratified_avg(
    identity,
    var_stratum = "stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 3,
    verbose = TRUE,
    seed = 123
  )
  
  expect_s3_class(result, "data.frame")
})

test_that("stratified_avg uses seed for reproducibility", {
  identity <- rankingQ::identity
  identity$stratum <- sample(c("A", "B"), nrow(identity), replace = TRUE)
  
  result1 <- stratified_avg(
    identity,
    var_stratum = "stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 5,
    seed = 456
  )
  
  result2 <- stratified_avg(
    identity,
    var_stratum = "stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 5,
    seed = 456
  )
  
  # Results should be identical with same seed
  expect_equal(result1, result2)
})
