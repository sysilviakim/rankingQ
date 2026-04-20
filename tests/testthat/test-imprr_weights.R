# Test imprr_weights function

test_that("IPW estimation works", {
  identity <- rankingQ::identity
  example_ipw <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity"
  )

  # Check structure of output
  expect_type(example_ipw, "list")
  expect_named(example_ipw, c("est_p_random", "results", "rankings"))

  # Check est_p_random is between 0 and 1
  expect_true(example_ipw$est_p_random >= 0 && example_ipw$est_p_random <= 1)

  # Check results has weights column
  expect_true("weights" %in% names(example_ipw$results))

  # Check rankings has all 24 permutations for J=4
  expect_equal(nrow(example_ipw$rankings), 24)
})

test_that("imprr_weights messages when using equal weights by default", {
  identity <- rankingQ::identity

  expect_message(
    imprr_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity"
    ),
    "No weight column supplied; using equal weights for all observations."
  )
})

test_that("IPW weights sum to approximately 1", {
  identity <- rankingQ::identity
  example_ipw <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity"
  )

  # Bias-corrected probabilities should sum to 1
  expect_equal(sum(example_ipw$rankings$prop_bc), 1, tolerance = 1e-10)
})

test_that("imprr_weights accepts a weight column name", {
  identity <- rankingQ::identity

  out <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    weight = "s_weight"
  )

  expect_true("weights" %in% names(out$results))
})

test_that("imprr_weights errors on missing weight column", {
  identity <- rankingQ::identity

  expect_error(
    imprr_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      weight = "missing_weight"
    ),
    "weight column not found in data."
  )
})

test_that("imprr_weights errors when estimated non-random rate is too small", {
  bad <- data.frame(
    q_1 = c(1, 1, 1, 1),
    q_2 = c(2, 2, 2, 2),
    anc_correct = c(0, 0, 0, 0)
  )

  expect_error(
    imprr_weights(
      bad,
      J = 2,
      main_q = "q",
      anc_correct = "anc_correct"
    ),
    "non-random response rate"
  )
})

test_that("imprr_weights uses exact ranking column names", {
  df <- data.frame(
    q10_1 = c(1, 2, 1, 2),
    q10_2 = c(2, 1, 2, 1),
    q1_1 = c(2, 1, 2, 1),
    q1_2 = c(1, 2, 1, 2),
    anc = c(1, 1, 1, 1)
  )

  out <- imprr_weights(
    df,
    J = 2,
    main_q = "q1",
    anc_correct = "anc"
  )

  expect_equal(out$results$ranking, c("21", "12", "21", "12"))
})

test_that("imprr_weights validates population and assumption inputs", {
  identity <- rankingQ::identity

  expect_error(
    imprr_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "oops"
    ),
    "population must be one of:"
  )

  expect_error(
    imprr_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "all",
      assumption = "oops"
    ),
    "assumption must be one of:"
  )

  expect_error(
    imprr_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "non-random",
      assumption = "uniform"
    ),
    "assumption is only used when population = 'all'"
  )
})

test_that("imprr_weights all-population contaminated matches default target", {
  identity <- rankingQ::identity

  out_default <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity"
  )

  out_contaminated <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    population = "all",
    assumption = "contaminated"
  )

  expect_equal(out_contaminated$est_p_random, out_default$est_p_random)
  expect_equal(out_contaminated$rankings, out_default$rankings)
  expect_equal(out_contaminated$results, out_default$results)
})

test_that("imprr_weights accepts common input variants", {
  identity <- rankingQ::identity

  out_default <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity"
  )

  out_variant <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    population = "Non random",
    assumption = "Contaminate"
  )

  expect_equal(out_variant$est_p_random, out_default$est_p_random)
  expect_equal(out_variant$rankings, out_default$rankings)
  expect_equal(out_variant$results, out_default$results)
})
