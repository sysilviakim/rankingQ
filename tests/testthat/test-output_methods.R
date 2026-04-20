test_that("tidy() standardizes direct estimator output", {
  out <- suppressMessages(
    imprr_direct(
      rankingQ::identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 5,
      seed = 1
    )
  )

  expect_s3_class(out, "rankingQ_output")

  avg_direct <- generics::tidy(
    out,
    method = "direct",
    type = "average_rank"
  )
  pairwise_raw <- generics::tidy(
    out,
    method = "raw",
    type = "pairwise",
    item = "app_identity_1"
  )

  expect_named(
    avg_direct,
    c(
      "method", "type", "item", "term", "outcome",
      "comparison_item", "k", "estimate", "conf.low", "conf.high"
    )
  )
  expect_equal(nrow(avg_direct), 4L)
  expect_true(all(avg_direct$method == "direct"))
  expect_true(all(avg_direct$type == "average_rank"))
  expect_true(all(pairwise_raw$method == "raw"))
  expect_true(all(pairwise_raw$type == "pairwise"))
  expect_true(all(pairwise_raw$item == "app_identity_1"))
})

test_that("summary(), plot(), and autoplot() work for direct outputs", {
  out <- suppressMessages(
    imprr_direct(
      rankingQ::identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 5,
      seed = 1
    )
  )

  s <- summary(out, method = "direct", type = "average_rank")
  txt <- capture.output(print(s))

  expect_s3_class(s, "summary.rankingQ_output")
  expect_true(any(grepl("Direct estimates", txt, fixed = TRUE)))
  expect_s3_class(plot(out), "ggplot")
  expect_s3_class(
    ggplot2::autoplot(out, method = "raw", type = "pairwise"),
    "ggplot"
  )
})

test_that("tidy() standardizes IPW output regardless of method requested", {
  out <- suppressMessages(
    imprr_weights(
      rankingQ::identity,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity"
    )
  )

  ipw_topk <- generics::tidy(out, method = "ipw", type = "top_k")
  raw_avg <- generics::tidy(out, method = "raw", type = "average_rank")
  p_random <- generics::tidy(out, component = "p_random")

  expect_true(all(ipw_topk$method == "ipw"))
  expect_true(all(ipw_topk$type == "top_k"))
  expect_true(all(is.na(ipw_topk$conf.low)))
  expect_true(all(raw_avg$method == "raw"))
  expect_named(p_random, c("term", "estimate", "conf.low", "conf.high"))
  expect_error(
    generics::tidy(out, method = "direct"),
    "method must be one of: raw, ipw"
  )
  expect_s3_class(plot(out, method = "ipw"), "ggplot")
})

test_that("imprr_direct_rcpp exposes raw and direct methods through tidy()", {
  out <- suppressMessages(
    imprr_direct_rcpp(
      rankingQ::identity_w,
      J = 4,
      main_q = "app_identity",
      anc_correct = "anc_correct_identity",
      n_bootstrap = 5,
      seed = 1
    )
  )

  raw_avg <- generics::tidy(out, method = "raw", type = "average_rank")
  direct_avg <- generics::tidy(out, method = "direct", type = "average_rank")

  expect_equal(nrow(raw_avg), 4L)
  expect_equal(nrow(direct_avg), 4L)
  expect_s3_class(ggplot2::autoplot(out), "ggplot")
})
