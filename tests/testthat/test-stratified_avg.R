# Test stratified_avg function

test_that("stratified_avg returns correct structure", {
  skip_on_cran()

  identity <- rankingQ::identity
  set.seed(1)
  identity$test_stratum <- sample(
    c("group1", "group2"), nrow(identity),
    replace = TRUE
  )

  result <- stratified_avg(
    data         = identity,
    var_stratum  = "test_stratum",
    J            = 4,
    main_q       = "app_identity",
    anc_correct  = "anc_correct_identity",
    n_bootstrap  = 5,
    seed         = 123
  )

  expect_s3_class(result, "data.frame")
  expect_true("mean" %in% names(result))
  expect_true("item" %in% names(result))
  # n_bootstrap bootstraps * J items per bootstrap
  expect_equal(nrow(result), 5 * 4)
})

test_that("stratified_avg returns IPW averages when ipw = TRUE", {
  skip_on_cran()

  identity <- rankingQ::identity
  set.seed(3)
  identity$test_stratum <- sample(
    c("group1", "group2"), nrow(identity),
    replace = TRUE
  )

  result <- stratified_avg(
    data = identity,
    var_stratum = "test_stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 3,
    ipw = TRUE,
    seed = 123
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3 * 4)
  expect_false(anyNA(result$mean))
  expect_setequal(as.character(unique(result$item)), paste0("app_identity_", 1:4))
})

test_that("stratified_avg errors on non-character var_stratum", {
  identity <- rankingQ::identity
  expect_error(
    stratified_avg(
      data        = identity,
      var_stratum = 123,
      J           = 4,
      main_q      = "app_identity",
      anc_correct = "anc_correct_identity"
    ),
    "var_stratum must be a character."
  )
})

test_that("stratified_avg errors on non-character main_q", {
  identity <- rankingQ::identity
  expect_error(
    stratified_avg(
      data        = identity,
      var_stratum = "anc_correct_identity",
      J           = 4,
      main_q      = 123,
      anc_correct = "anc_correct_identity"
    ),
    "main_q must be a character."
  )
})

test_that("stratified_avg errors on non-character anc_correct", {
  identity <- rankingQ::identity
  expect_error(
    stratified_avg(
      data        = identity,
      var_stratum = "anc_correct_identity",
      J           = 4,
      main_q      = "app_identity",
      anc_correct = 123
    ),
    "anc_correct must be a character."
  )
})

test_that("stratified_avg uses provided weight vector", {
  skip_on_cran()

  identity <- rankingQ::identity
  set.seed(2)
  identity$test_stratum <- sample(c("group1", "group2"), nrow(identity), TRUE)

  w1 <- rep(1, nrow(identity))
  w2 <- c(
    rep(10, nrow(identity) / 2),
    rep(1, nrow(identity) - nrow(identity) / 2)
  )

  out1 <- stratified_avg(
    data = identity,
    var_stratum = "test_stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    weight = w1,
    n_bootstrap = 3,
    seed = 123
  )
  out2 <- stratified_avg(
    data = identity,
    var_stratum = "test_stratum",
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    weight = w2,
    n_bootstrap = 3,
    seed = 123
  )

  expect_false(isTRUE(all.equal(out1$mean, out2$mean)))
})

test_that("stratified_avg infers J and is reproducible with the same seed", {
  skip_on_cran()

  identity <- rankingQ::identity
  set.seed(4)
  identity$test_stratum <- sample(c("group1", "group2"), nrow(identity), TRUE)

  out1 <- stratified_avg(
    data = identity,
    var_stratum = "test_stratum",
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 2,
    seed = 321
  )
  out2 <- stratified_avg(
    data = identity,
    var_stratum = "test_stratum",
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    n_bootstrap = 2,
    seed = 321
  )

  expect_identical(out1, out2)
  expect_equal(nrow(out1), 2 * 4)
  expect_setequal(as.character(unique(out1$item)), paste0("app_identity_", 1:4))
})
