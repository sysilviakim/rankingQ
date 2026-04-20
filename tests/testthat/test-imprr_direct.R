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
