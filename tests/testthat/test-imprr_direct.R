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
  expect_equal(
    example_direct$est_p_random,
    structure(
      list(
        mean = 0.315068713332798, lower = 0.289994374347022,
        upper = 0.344265852286426
      ),
      row.names = c(NA, -1L), class = "data.frame"
    )
  )
})
