# Tests for imprr_direct_rcpp function

test_that("imprr_direct_rcpp returns expected structure", {
  data(identity_w)

  result <- imprr_direct_rcpp(
    data = identity_w,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 50,
    seed = 123
  )

  # Check structure

  expect_type(result, "list")
  expect_named(result, c("est_p_random", "results"))

  # Check est_p_random
  expect_s3_class(result$est_p_random, "tbl_df")
  expect_named(result$est_p_random, c("mean", "lower", "upper"))
  expect_equal(nrow(result$est_p_random), 1)

  # Check results
  expect_s3_class(result$results, "tbl_df")
  expect_true("item" %in% names(result$results))
  expect_true("qoi" %in% names(result$results))
  expect_true("mean" %in% names(result$results))
})

test_that("imprr_direct_rcpp produces reasonable estimates", {
  data(identity_w)

  result <- imprr_direct_rcpp(
    data = identity_w,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 100,
    seed = 456
  )

  # p_random should be between 0 and 1
  expect_true(result$est_p_random$mean >= 0 && result$est_p_random$mean <= 1)

  # Average ranks should be between 1 and J
  avg_ranks <- result$results[result$results$qoi == "average rank", ]
  expect_true(all(avg_ranks$mean >= 1 & avg_ranks$mean <= 4))
})

test_that("imprr_direct_rcpp is faster than imprr_direct", {
  skip_on_cran()
  data(identity_w)

  # Run both versions
  time_rcpp <- system.time({
    result_rcpp <- imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 50,
      seed = 123
    )
  })["elapsed"]

  time_tidy <- system.time({
    result_tidy <- imprr_direct(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 50,
      seed = 123
    )
  })["elapsed"]

  # Rcpp should be faster
  expect_true(time_rcpp < time_tidy)

  # Should be at least 10x faster
  expect_true(time_tidy / time_rcpp > 10)
})

test_that("imprr_direct_rcpp handles custom weights", {
  data(identity_w)

  # Create some weights
  weights <- identity_w$w

  result <- imprr_direct_rcpp(
    data = identity_w,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 50,
    seed = 789,
    weight = weights
  )

  expect_type(result, "list")
  expect_true(result$est_p_random$mean >= 0)
})

test_that("imprr_direct_rcpp errors on malformed weight length", {
  data(identity_w)

  expect_error(
    imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 10,
      seed = 789,
      weight = 1
    ),
    "weight must have the same length as the number of rows in data."
  )
})

test_that("imprr_direct_rcpp validates bootstrap count", {
  data(identity_w)

  expect_error(
    imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 0,
      seed = 789
    ),
    "n_bootstrap must be a single integer >= 1."
  )
})

test_that("imprr_direct_rcpp validates main_q when inferring J", {
  data(identity_w)

  expect_error(
    imprr_direct_rcpp(
      data = identity_w[, setdiff(names(identity_w), "app_identity")],
      J = NULL,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 1,
      seed = 789
    ),
    "When J is NULL, main_q must exist as a column in data so J can be inferred."
  )
})

test_that("imprr_direct_rcpp validates anc_correct column presence", {
  data(identity_w)

  expect_error(
    imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "missing_anchor",
      n_bootstrap = 1,
      seed = 789
    ),
    "anc_correct column not found in data."
  )
})

test_that("imprr_direct_rcpp with verbose output", {
  data(identity_w)

  expect_message(
    imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 10,
      seed = 111,
      verbose = TRUE
    ),
    "Rcpp bootstrap"
  )
})
