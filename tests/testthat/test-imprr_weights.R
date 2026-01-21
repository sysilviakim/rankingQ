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
