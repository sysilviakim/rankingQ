# Test imprr_weights_boot function

test_that("imprr_weights_boot returns direct-style summaries", {
  identity <- rankingQ::identity

  out <- suppressMessages(imprr_weights_boot(
    identity,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 2,
    seed = 123
  ))

  expect_type(out, "list")
  expect_named(out, c("est_p_random", "results"))
  expect_equal(names(out$est_p_random), c("mean", "lower", "upper"))
  expect_equal(
    names(out$results),
    c("item", "qoi", "outcome", "mean", "lower", "upper")
  )
  expect_equal(nrow(out$results), 44L)
  expect_setequal(
    unique(out$results$qoi),
    c("average rank", "pairwise ranking", "top-k ranking", "marginal ranking")
  )
})

test_that("imprr_weights_boot matches manual one-draw IPW summaries", {
  toy <- data.frame(
    q_1 = c(1, 1, 2, 2),
    q_2 = c(2, 2, 1, 1)
  )

  set.seed(123)
  idx <- sample.int(nrow(toy), size = nrow(toy), replace = TRUE)
  boot_toy <- toy[idx, , drop = FALSE]

  expected <- data.frame(
    item = c(rep("q_1", 5), rep("q_2", 5)),
    qoi = c(
      "average rank", "pairwise ranking", "top-k ranking",
      "marginal ranking", "marginal ranking",
      "average rank", "pairwise ranking", "top-k ranking",
      "marginal ranking", "marginal ranking"
    ),
    outcome = c(
      "Avg: q_1", "v. q_2", "Top-1", "Ranked 1", "Ranked 2",
      "Avg: q_2", "v. q_1", "Top-1", "Ranked 1", "Ranked 2"
    ),
    mean = c(
      mean(boot_toy$q_1),
      mean(boot_toy$q_1 < boot_toy$q_2),
      mean(boot_toy$q_1 <= 1),
      mean(boot_toy$q_1 == 1),
      mean(boot_toy$q_1 == 2),
      mean(boot_toy$q_2),
      mean(boot_toy$q_2 < boot_toy$q_1),
      mean(boot_toy$q_2 <= 1),
      mean(boot_toy$q_2 == 1),
      mean(boot_toy$q_2 == 2)
    ),
    stringsAsFactors = FALSE
  )
  expected$lower <- expected$mean
  expected$upper <- expected$mean

  out <- suppressMessages(imprr_weights_boot(
    toy,
    J = 2,
    main_q = "q",
    p_random = 0,
    n_bootstrap = 1,
    seed = 123
  ))

  expect_equal(out$est_p_random$mean, 0)
  expect_equal(out$est_p_random$lower, 0)
  expect_equal(out$est_p_random$upper, 0)
  actual <- as.data.frame(
    out$results[order(out$results$item, out$results$qoi, out$results$outcome), ]
  )
  expected <- expected[order(expected$item, expected$qoi, expected$outcome), ]
  rownames(actual) <- NULL
  rownames(expected) <- NULL
  expect_equal(
    actual,
    expected
  )
})

test_that("imprr_weights_boot keeps fixed p_random constant across draws", {
  toy <- data.frame(
    q_1 = c(1, 2, 1, 2),
    q_2 = c(2, 1, 2, 1)
  )

  out <- suppressMessages(imprr_weights_boot(
    toy,
    J = 2,
    main_q = "q",
    p_random = 0.25,
    n_bootstrap = 3,
    seed = 123
  ))

  expect_equal(out$est_p_random$mean, 0.25)
  expect_equal(out$est_p_random$lower, 0.25)
  expect_equal(out$est_p_random$upper, 0.25)
  expect_false(anyNA(out$results$mean))
})

test_that("imprr_weights_boot handles existing weights output columns safely", {
  identity_w <- rankingQ::identity_w

  out <- suppressMessages(imprr_weights_boot(
    identity_w,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    weight = "s_weight",
    n_bootstrap = 1,
    seed = 123
  ))

  expect_equal(nrow(out$results), 44L)
  expect_true(out$est_p_random$mean >= 0 && out$est_p_random$mean <= 1)
})

test_that("imprr_weights_boot validates n_bootstrap and uniform-all semantics", {
  identity <- rankingQ::identity

  expect_error(
    imprr_weights_boot(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 0
    ),
    "n_bootstrap must be a single integer >= 1."
  )

  expect_message(
    out <- imprr_weights_boot(
      identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      population = "all",
      assumption = "uniform",
      n_bootstrap = 1,
      seed = 123
    ),
    "population = 'all' with assumption = 'uniform' implies no correction"
  )
  expect_equal(out$est_p_random$mean, 0)
})
