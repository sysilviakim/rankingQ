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
