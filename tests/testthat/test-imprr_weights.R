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

test_that("imprr_weights accepts a fixed p_random without anc_correct", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1)
  )

  out <- imprr_weights(
    toy,
    J = 2,
    main_q = "q",
    p_random = 0.25
  )

  expect_equal(out$est_p_random, 0.25)
  expect_true("weights" %in% names(out$results))
})

test_that("imprr_weights messages when p_random overrides anc_correct", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1),
    anc = c(1, 0, 1, 0)
  )

  expect_message(
    imprr_weights(
      toy,
      J = 2,
      main_q = "q",
      anc_correct = "anc",
      p_random = 0.25
    ),
    "p_random supplied; ignoring anc_correct"
  )
})

test_that("imprr_weights defaults to no correction without anc_correct or p_random", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1)
  )

  expect_message(
    out <- imprr_weights(
      toy,
      J = 2,
      main_q = "q"
    ),
    "No anc_correct or p_random supplied"
  )

  expect_equal(out$est_p_random, 0)
  expect_equal(out$rankings$prop_bc, out$rankings$prop_obs)
  expect_equal(out$results$weights, rep(1, nrow(toy)))
})

test_that("imprr_weights uses supplied weights in est_p_random", {
  toy <- data.frame(
    q_1 = c(1, 1, 2, 2),
    q_2 = c(2, 2, 1, 1),
    anc = c(1, 1, 0, 0),
    w_high = c(10, 10, 1, 1),
    w_low = c(6, 6, 5, 5)
  )

  out_high <- imprr_weights(
    toy,
    J = 2,
    main_q = "q",
    anc_correct = "anc",
    weight = "w_high"
  )

  out_low <- imprr_weights(
    toy,
    J = 2,
    main_q = "q",
    anc_correct = "anc",
    weight = "w_low"
  )

  expect_false(isTRUE(all.equal(out_high$est_p_random, out_low$est_p_random)))
  expect_equal(out_high$est_p_random, 1 - (((20 / 22) - 0.5) / 0.5))
  expect_equal(out_low$est_p_random, 1 - (((12 / 22) - 0.5) / 0.5))
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

test_that("imprr_weights rejects invalid weight values", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1),
    anc = c(1, 1, 1, 1),
    bad_w = c(1, -1, 1, 1),
    inf_w = c(Inf, 1, 1, 1)
  )

  expect_error(
    imprr_weights(
      toy,
      J = 2,
      main_q = "q",
      anc_correct = "anc",
      weight = "bad_w"
    ),
    "weight values cannot be negative."
  )
  expect_error(
    imprr_weights(
      toy,
      J = 2,
      main_q = "q",
      anc_correct = "anc",
      weight = "inf_w"
    ),
    "weight values must be finite and non-missing."
  )
  expect_error(
    imprr_weights(
      toy,
      J = 2,
      main_q = "q",
      anc_correct = "anc",
      weight = c(0, 0, 0, 0)
    ),
    "weight values must sum to a positive number."
  )
})

test_that("imprr_weights validates missing anc_correct and empty data clearly", {
  identity <- rankingQ::identity

  expect_error(
    imprr_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "missing_anc"
    ),
    "anc_correct column not found in data."
  )

  expect_error(
    imprr_weights(
      identity[0, ],
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity"
    ),
    "There is no data to analyze"
  )
})

test_that("imprr_weights validates J inference requirements clearly", {
  bad <- data.frame(
    anc_correct = c(1, 1, 1)
  )

  expect_error(
    imprr_weights(
      bad,
      J = NULL,
      main_q = "q",
      anc_correct = "anc_correct"
    ),
    "When J is NULL, main_q must exist as a column in data so J can be inferred."
  )
})

test_that("imprr_weights infers J when the first main_q value is NA", {
  identity <- rankingQ::identity
  identity$app_identity[1] <- NA_character_

  out_inferred <- imprr_weights(
    identity,
    J = NULL,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity"
  )

  out_explicit <- imprr_weights(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity"
  )

  expect_equal(out_inferred$est_p_random, out_explicit$est_p_random)
  expect_equal(out_inferred$rankings, out_explicit$rankings)
  expect_equal(out_inferred$results, out_explicit$results)
})

test_that("imprr_weights no longer accepts seed", {
  identity <- rankingQ::identity

  expect_error(
    imprr_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      seed = 1
    ),
    "unused argument"
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

test_that("imprr_weights accepts direct ranking-column input and bare names", {
  df <- data.frame(
    party = c(1, 2, 1, 2),
    gender = c(2, 1, 2, 1),
    anc = c(1, 1, 1, 1),
    w = c(1, 2, 1, 2)
  )

  out <- imprr_weights(
    df,
    main_q = c(party, gender),
    anc_correct = anc,
    weight = w
  )

  expect_equal(out$results$ranking, c("12", "21", "12", "21"))
  expect_equal(sort(out$rankings$ranking), c("12", "21"))
})

test_that("imprr_weights respects a custom ranking column name everywhere", {
  df <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1),
    anc = c(1, 1, 1, 1)
  )

  out <- imprr_weights(
    df,
    J = 2,
    main_q = "q",
    anc_correct = "anc",
    ranking = "myrank"
  )

  expect_true("myrank" %in% names(out$results))
  expect_false("ranking" %in% names(out$results))
  expect_equal(out$results$myrank, c("12", "21", "12", "21"))

  expect_true("myrank" %in% names(out$rankings))
  expect_false("ranking" %in% names(out$rankings))
  expect_equal(out$rankings$myrank, c("12", "21"))
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

test_that("imprr_weights errors on conflicting output column names", {
  identity_w <- rankingQ::identity_w

  expect_error(
    imprr_weights(
      identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity"
    ),
    paste(
      "data already contains output column\\(s\\): ranking, weights\\.",
      "imprr_weights\\(\\) does not overwrite existing output columns\\."
    )
  )

  df <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1),
    anc = c(1, 1, 1, 1),
    myrank = c("12", "21", "12", "21")
  )

  expect_error(
    imprr_weights(
      df,
      J = 2,
      main_q = "q",
      anc_correct = "anc",
      ranking = "myrank"
    ),
    "data already contains output column\\(s\\): myrank\\."
  )
})

test_that("add_ipw_weights returns augmented data by default", {
  identity <- rankingQ::identity

  out_full <- suppressMessages(
    imprr_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity"
    )
  )
  out_aug <- suppressMessages(
    add_ipw_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity"
    )
  )

  expect_s3_class(out_aug, "data.frame")
  expect_equal(names(out_aug), c(names(identity), "ipw_weights"))
  expect_false("ranking" %in% names(out_aug))
  expect_equal(out_aug$ipw_weights, out_full$results$weights)
})

test_that("add_ipw_weights optionally keeps ranking outputs", {
  identity <- rankingQ::identity

  out <- suppressMessages(
    add_ipw_weights(
      identity,
      J = 4,
      main_q = app_identity,
      anc_correct = anc_correct_identity,
      keep_ranking = TRUE,
      ranking_col = "myrank",
      keep_rankings = TRUE
    )
  )

  expect_type(out, "list")
  expect_named(out, c("data", "rankings", "est_p_random"))
  expect_true(all(c("ipw_weights", "myrank") %in% names(out$data)))
  expect_true("myrank" %in% names(out$rankings))
  expect_false("ranking" %in% names(out$data))
})

test_that("add_ipw_weights preserves an existing weights column", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1),
    anc = c(1, 1, 1, 1),
    weights = c(2, 3, 4, 5)
  )

  out <- add_ipw_weights(
    toy,
    J = 2,
    main_q = "q",
    anc_correct = "anc",
    weight = "weights"
  )

  expect_equal(out$weights, toy$weights)
  expect_true("ipw_weights" %in% names(out))
})

test_that("add_ipw_weights validates output column collisions", {
  identity <- rankingQ::identity
  identity$ipw_weights <- 1

  expect_error(
    add_ipw_weights(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity"
    ),
    "weight_col already exists in data."
  )

  expect_error(
    add_ipw_weights(
      rankingQ::identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      keep_ranking = TRUE,
      ranking_col = "app_identity"
    ),
    "ranking_col already exists in data."
  )
})
