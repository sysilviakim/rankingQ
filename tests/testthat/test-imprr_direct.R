test_that("plug-in estimation works", {
  ## Otherwise identity data not correctly called
  ## data("identity") does not work (not sure why?)
  identity <- rankingQ::identity
  example_direct <- imprr_direct(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 10
  )
  expect_equal(nrow(example_direct$est_p_random), 1L)
  expect_named(example_direct$est_p_random, c("mean", "lower", "upper"))
  expect_true(example_direct$est_p_random$mean > 0 &
    example_direct$est_p_random$mean < 1)
  expect_true(example_direct$est_p_random$lower <=
                example_direct$est_p_random$mean)
  expect_true(example_direct$est_p_random$upper >=
                example_direct$est_p_random$mean)
})

test_that("imprr_direct validates bootstrap count", {
  identity <- rankingQ::identity

  expect_error(
    imprr_direct(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 0
    ),
    "n_bootstrap must be a single integer >= 1."
  )
})

test_that("imprr_direct validates main_q when inferring J", {
  identity <- rankingQ::identity

  expect_error(
    imprr_direct(
      identity[, setdiff(names(identity), "app_identity")],
      J = NULL,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 1
    ),
    "When J is NULL, main_q must exist as a column in data so J can be inferred."
  )
})

test_that("imprr_direct validates anc_correct column presence", {
  identity <- rankingQ::identity

  expect_error(
    imprr_direct(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "missing_anchor",
      n_bootstrap = 1
    ),
    "anc_correct column not found in data."
  )
})

test_that("imprr_direct accepts a weight column name", {
  identity <- rankingQ::identity

  out <- imprr_direct(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    weight = "s_weight",
    n_bootstrap = 1,
    seed = 1
  )

  expect_equal(nrow(out$results), 44L)
})

test_that("imprr_direct errors on missing weight column", {
  identity <- rankingQ::identity

  expect_error(
    imprr_direct(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      weight = "missing_weight",
      n_bootstrap = 1
    ),
    "weight column not found in data."
  )
})

test_that("imprr_direct rejects empty data", {
  identity <- rankingQ::identity[0, ]

  expect_error(
    imprr_direct(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 1
    ),
    "There is no data to analyze. Please check the input data."
  )
})

test_that("imprr_direct uses exact ranking column names", {
  df <- data.frame(
    q10_1 = c(1, 2, 1, 2),
    q10_2 = c(2, 1, 2, 1),
    q1_1 = c(2, 1, 2, 1),
    q1_2 = c(1, 2, 1, 2),
    anc = c(1, 1, 1, 1)
  )

  out <- imprr_direct(
    df,
    J = 2,
    main_q = "q1",
    anc_correct = "anc",
    n_bootstrap = 1,
    seed = 1
  )

  expect_equal(sort(unique(out$results$item)), c("q1_1", "q1_2"))
})

test_that("imprr_direct validates population and assumption inputs", {
  identity <- rankingQ::identity

  expect_error(
    imprr_direct(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "oops",
      n_bootstrap = 1
    ),
    "population must be one of:"
  )

  expect_error(
    imprr_direct(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "all",
      assumption = "oops",
      n_bootstrap = 1
    ),
    "assumption must be one of:"
  )

  expect_error(
    imprr_direct(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "non-random",
      assumption = "uniform",
      n_bootstrap = 1
    ),
    "assumption is only used when population = 'all'"
  )
})

test_that("imprr_direct all-population contaminated matches default target", {
  identity <- rankingQ::identity

  out_default <- imprr_direct(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 10,
    seed = 123
  )

  out_contaminated <- imprr_direct(
    identity,
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

test_that("imprr_direct accepts common input variants", {
  identity <- rankingQ::identity

  out_default <- imprr_direct(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 10,
    seed = 123
  )

  out_variant <- imprr_direct(
    identity,
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
