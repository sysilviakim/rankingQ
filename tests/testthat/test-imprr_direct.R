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

test_that("imprr_direct messages when using equal weights by default", {
  identity <- rankingQ::identity

  expect_message(
    imprr_direct(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 1,
      seed = 1
    ),
    "No weight column supplied; using equal weights for all observations."
  )
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
    paste(
      "When J is NULL, main_q must exist as a column in data",
      "so J can be inferred."
    )
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

test_that("imprr_direct accepts a fixed p_random without anc_correct", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1)
  )

  out <- imprr_direct(
    toy,
    J = 2,
    main_q = "q",
    p_random = 0.25,
    n_bootstrap = 3,
    seed = 1
  )

  expect_equal(as.numeric(out$est_p_random[1, ]), c(0.25, 0.25, 0.25))
  expect_equal(nrow(out$results), 10L)
})

test_that("imprr_direct messages when p_random overrides anc_correct", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1),
    anc = c(1, 0, 1, 0)
  )

  expect_message(
    imprr_direct(
      toy,
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

test_that("imprr_direct defaults to no correction without anc_correct or p_random", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1)
  )

  expect_message(
    out <- imprr_direct(
      toy,
      J = 2,
      main_q = "q",
      n_bootstrap = 3,
      seed = 1
    ),
    "No anc_correct or p_random supplied"
  )

  expect_equal(as.numeric(out$est_p_random[1, ]), c(0, 0, 0))
  expect_false(anyNA(out$results$mean))
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

test_that("imprr_direct infers J from delimiter-separated main_q values", {
  ranking_mat <- rbind(1:10, 10:1)
  toy <- as.data.frame(ranking_mat)
  names(toy) <- paste0("q_", 1:10)
  toy$q <- c(
    paste(1:10, collapse = "|"),
    paste(10:1, collapse = "|")
  )
  toy$anc <- c(1, 1)

  out <- imprr_direct(
    toy,
    J = NULL,
    main_q = "q",
    anc_correct = "anc",
    n_bootstrap = 1,
    seed = 1
  )

  expect_equal(nrow(out$est_p_random), 1)
  expect_equal(nrow(out$results), 290)
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
