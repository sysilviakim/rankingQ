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

test_that("imprr_direct_rcpp uses exact ranking column names", {
  df <- data.frame(
    q10_1 = c(1, 2, 1, 2),
    q10_2 = c(2, 1, 2, 1),
    q1_1 = c(2, 1, 2, 1),
    q1_2 = c(1, 2, 1, 2),
    anc = c(1, 1, 1, 1)
  )

  out <- imprr_direct_rcpp(
    data = df,
    J = 2,
    main_q = "q1",
    anc_correct = "anc",
    n_bootstrap = 1,
    seed = 1
  )

  expect_equal(sort(unique(out$results$item)), c("q1_1", "q1_2"))
})

test_that("imprr_direct_rcpp validates population and assumption inputs", {
  data(identity_w)

  expect_error(
    imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "oops",
      n_bootstrap = 1,
      seed = 123
    ),
    "population must be one of:"
  )

  expect_error(
    imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "all",
      assumption = "oops",
      n_bootstrap = 1,
      seed = 123
    ),
    "assumption must be one of:"
  )

  expect_error(
    imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "non-random",
      assumption = "uniform",
      n_bootstrap = 1,
      seed = 123
    ),
    "assumption is only used when population = 'all'"
  )
})

test_that("imprr_direct_rcpp all-population contaminated matches default target", {
  data(identity_w)

  out_default <- imprr_direct_rcpp(
    data = identity_w,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 10,
    seed = 123
  )

  out_contaminated <- imprr_direct_rcpp(
    data = identity_w,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    population = "all",
    assumption = "contaminated",
    n_bootstrap = 10,
    seed = 123
  )

  expect_equal(out_contaminated$est_p_random, out_default$est_p_random)
  expect_equal(out_contaminated$results, out_default$results)
})

test_that("imprr_direct_rcpp accepts common input variants", {
  data(identity_w)

  out_default <- imprr_direct_rcpp(
    data = identity_w,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 10,
    seed = 123
  )

  out_variant <- imprr_direct_rcpp(
    data = identity_w,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    population = "Non random",
    assumption = "Contaminate",
    n_bootstrap = 10,
    seed = 123
  )

  expect_equal(out_variant$est_p_random, out_default$est_p_random)
  expect_equal(out_variant$results, out_default$results)
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
