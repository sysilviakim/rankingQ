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

test_that("imprr_direct_rcpp messages when using equal weights by default", {
  data(identity_w)

  expect_message(
    imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 1,
      seed = 123
    ),
    "No weight column supplied; using equal weights for all observations."
  )
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

test_that("imprr_direct_rcpp accepts a weight column name", {
  data(identity_w)

  result <- imprr_direct_rcpp(
    data = identity_w,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 50,
    seed = 789,
    weight = "weights"
  )

  expect_type(result, "list")
  expect_true(result$est_p_random$mean >= 0)
})

test_that("imprr_direct_rcpp accepts a fixed p_random without anc_correct", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1)
  )

  result <- imprr_direct_rcpp(
    data = toy,
    J = 2,
    main_q = "q",
    p_random = 0.25,
    n_bootstrap = 3,
    seed = 1
  )

  expect_equal(as.numeric(result$est_p_random[1, ]), c(0.25, 0.25, 0.25))
  expect_equal(nrow(result$results), 10L)
})

test_that("imprr_direct_rcpp messages when p_random overrides anc_correct", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1),
    anc = c(1, 0, 1, 0)
  )

  expect_message(
    imprr_direct_rcpp(
      data = toy,
      J = 2,
      main_q = "q",
      anc_correct = "anc",
      p_random = 0.25,
      n_bootstrap = 3,
      seed = 1
    ),
    "p_random supplied; ignoring anc_correct"
  )
})

test_that("imprr_direct_rcpp defaults to no correction without anc_correct or p_random", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1)
  )

  expect_message(
    result <- imprr_direct_rcpp(
      data = toy,
      J = 2,
      main_q = "q",
      n_bootstrap = 3,
      seed = 1
    ),
    "No anc_correct or p_random supplied"
  )

  expect_equal(as.numeric(result$est_p_random[1, ]), c(0, 0, 0))
  expect_false(anyNA(result$results$mean))
})

test_that("imprr_direct_rcpp errors on missing weight column", {
  data(identity_w)

  expect_error(
    imprr_direct_rcpp(
      data = identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 10,
      seed = 789,
      weight = "missing_weight"
    ),
    "weight column not found in data."
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
    paste(
      "When J is NULL, main_q must exist as a column in data",
      "so J can be inferred."
    )
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

test_that("imprr_direct_rcpp rejects empty data", {
  data(identity_w)

  expect_error(
    imprr_direct_rcpp(
      data = identity_w[0, ],
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 1,
      seed = 789
    ),
    "There is no data to analyze. Please check the input data."
  )
})

test_that("imprr_direct_rcpp errors when bootstrap draws imply invalid non-random rate", {
  bad <- data.frame(
    q = "123",
    q_1 = 1,
    q_2 = 2,
    q_3 = 3,
    anc = c(1, NA, 1)
  )

  expect_error(
    imprr_direct_rcpp(
      data = bad,
      main_q = "q",
      anc_correct = "anc",
      n_bootstrap = 2,
      seed = 1
    ),
    "Estimated non-random response rate is too small/non-finite."
  )
})

test_that("imprr_direct_rcpp infers J from delimiter-separated main_q values", {
  ranking_mat <- rbind(1:10, 10:1)
  toy <- as.data.frame(ranking_mat)
  names(toy) <- paste0("q_", 1:10)
  toy$q <- c(
    paste(1:10, collapse = "|"),
    paste(10:1, collapse = "|")
  )
  toy$anc <- c(1, 1)

  out <- imprr_direct_rcpp(
    data = toy,
    J = NULL,
    main_q = "q",
    anc_correct = "anc",
    n_bootstrap = 1,
    seed = 1
  )

  expect_equal(nrow(out$est_p_random), 1)
  expect_equal(nrow(out$results), 290)
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

test_that(
  "imprr_direct_rcpp all-population contaminated matches default target",
  {
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
  }
)

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
