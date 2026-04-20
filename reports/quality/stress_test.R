## Stress test for rankingQ package
## Generates synthetic ranking data under varying configurations
## and exercises all exported functions.

suppressPackageStartupMessages({
  library(rankingQ)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
})

set.seed(20260421)

out_dir <- file.path("reports", "quality")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(out_dir, "stress_test_log.txt")
cat("rankingQ stress test — started\n", file = log_file)
log <- function(msg) {
  cat(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n"),
      file = log_file, append = TRUE)
}

## ----------------------------------------------------------------------------
## Data generators
## ----------------------------------------------------------------------------

make_gamma <- function(J, shape = c("uniform", "moderate", "skewed", "extreme")) {
  shape <- match.arg(shape)
  g <- switch(shape,
    uniform  = rep(1 / J, J),
    moderate = (J:1) / sum(J:1),
    skewed   = (seq(J, 1)^2) / sum(seq(J, 1)^2),
    extreme  = {
      v <- c(10, rep(1, J - 1))
      v / sum(v)
    }
  )
  g
}

## Generate mixed data: a fraction of "random" responders and the rest drawn
## from Plackett-Luce. Return a wide data frame plus the anchor indicator.
gen_mixed_data <- function(N, J, gamma, p_random,
                           anchor_vec = NULL) {
  n_random <- round(p_random * N)
  n_nonrandom <- N - n_random

  if (n_nonrandom > 0) {
    pl_data <- rpluce(n = n_nonrandom, t = J, prob = gamma)
  } else {
    pl_data <- data.frame(matrix(character(), nrow = 0, ncol = J))
    colnames(pl_data) <- ordinal_seq(J)
  }

  if (n_random > 0) {
    rand_rows <- t(replicate(n_random, sample(ordinal_seq(J))))
    rand_df <- as.data.frame(rand_rows, stringsAsFactors = FALSE)
    colnames(rand_df) <- ordinal_seq(J)
  } else {
    rand_df <- pl_data[0, , drop = FALSE]
  }

  dat <- rbind(pl_data, rand_df)
  dat <- dat[sample(nrow(dat)), , drop = FALSE]
  rownames(dat) <- NULL

  ## Convert string items back to rank integers per item
  ## (Simulate the typical Qualtrics wide format: one col per item,
  ## cell value = rank assigned)
  items <- ordinal_seq(J)
  wide <- matrix(NA_integer_, nrow = nrow(dat), ncol = J,
                 dimnames = list(NULL, paste0("main_q_", seq_len(J))))
  for (i in seq_len(nrow(dat))) {
    ordering <- as.character(unlist(dat[i, ]))
    wide[i, ] <- match(items, ordering)
  }
  wide_df <- as.data.frame(wide)

  ## Anchor indicator: 1 if this respondent passes an anchor item,
  ## 0 otherwise. We simulate the anchor as "the PL responders
  ## mostly pass; the random responders pass with prob 1/J!"
  if (!is.null(anchor_vec)) {
    ## Random responders pass the anchor with prob 1/J (uniform)
    pass_if_random <- rbinom(nrow(wide_df), 1, 1 / factorial(J))
    is_random <- rep(c(rep(0, n_nonrandom), rep(1, n_random)),
                     length.out = nrow(wide_df))
    ## But we shuffled, so we need the pre-shuffle identity.
    ## Simpler: set anchor_pass with probability that matches
    ## 'non-random' vs 'random' composition for a roughly correct rate.
    anchor <- rbinom(nrow(wide_df), 1, 1 - p_random * (1 - 1 / factorial(J)))
    wide_df$anchor_pass <- anchor
  }

  wide_df
}

## ----------------------------------------------------------------------------
## Test runner
## ----------------------------------------------------------------------------

run_one <- function(cfg) {
  res <- list(cfg = cfg, status = "ok", errors = character(),
              timings = list(), checks = list())

  safely_time <- function(label, expr) {
    tryCatch({
      t0 <- Sys.time()
      val <- force(expr)
      dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      res$timings[[label]] <<- dt
      val
    }, error = function(e) {
      res$status <<- "fail"
      res$errors <<- c(res$errors, paste0(label, ": ", conditionMessage(e)))
      NULL
    })
  }

  gamma <- make_gamma(cfg$J, cfg$shape)
  dat <- gen_mixed_data(cfg$N, cfg$J, gamma, cfg$p_random,
                        anchor_vec = TRUE)

  ## Weight variants
  weight_arg <- switch(cfg$weight_mode,
    none   = NULL,
    vec    = runif(cfg$N, 0.5, 2),
    colname = {
      dat$w <- runif(cfg$N, 0.5, 2)
      "w"
    }
  )

  ## 1. imprr_direct
  est_direct <- safely_time("imprr_direct", imprr_direct(
    data = dat,
    J = cfg$J,
    main_q = "main_q_",
    anc_correct = if (cfg$use_anchor) "anchor_pass" else NULL,
    p_random = if (cfg$use_anchor) NULL else 0.1,
    n_bootstrap = cfg$n_boot,
    weight = weight_arg,
    verbose = FALSE
  ))

  ## 2. imprr_direct_rcpp (should match imprr_direct closely)
  est_rcpp <- safely_time("imprr_direct_rcpp", imprr_direct_rcpp(
    data = dat,
    J = cfg$J,
    main_q = "main_q_",
    anc_correct = if (cfg$use_anchor) "anchor_pass" else NULL,
    p_random = if (cfg$use_anchor) NULL else 0.1,
    n_bootstrap = cfg$n_boot,
    weight = weight_arg,
    verbose = FALSE
  ))

  ## 3. imprr_weights
  est_w <- safely_time("imprr_weights", imprr_weights(
    data = dat,
    J = cfg$J,
    main_q = "main_q_",
    anc_correct = if (cfg$use_anchor) "anchor_pass" else NULL,
    p_random = if (cfg$use_anchor) NULL else 0.1,
    weight = weight_arg
  ))

  ## 4. rank_longer / rank_wider round trip
  safely_time("rank_longer_wider", {
    main_cols <- paste0("main_q_", seq_len(cfg$J))
    dat$id <- seq_len(nrow(dat))
    long <- rank_longer(dat[, c("id", main_cols)], cols = main_cols, id = "id")
    wide_back <- rank_wider(long, id = "id")
    stopifnot(nrow(wide_back) == nrow(dat))
  })

  ## 5. uniformity_test
  safely_time("uniformity_test", {
    main_cols <- paste0("main_q_", seq_len(cfg$J))
    u_df <- dat[, main_cols]
    ## collapse to single string ranking for uniformity_test
    u_df$ranking <- apply(u_df, 1, paste0, collapse = "")
    uniformity_test(u_df, var = "ranking")
  })

  ## 6. avg_rank on raw rankings
  safely_time("avg_rank", {
    main_cols <- paste0("main_q_", seq_len(cfg$J))
    avg_rank(dat, rankings = main_cols)
  })

  ## 7. permn_augment — only small J (factorial blows up)
  if (cfg$J <= 5) {
    safely_time("permn_augment", {
      main_cols <- paste0("main_q_", seq_len(cfg$J))
      u_df <- dat[, main_cols]
      u_df$ranking <- apply(u_df, 1, paste0, collapse = "")
      tab <- table(u_df$ranking)
      permn_augment(tab, J = cfg$J)
    })
  }

  ## Check: imprr_direct vs imprr_direct_rcpp should agree
  if (!is.null(est_direct) && !is.null(est_rcpp)) {
    d1 <- est_direct$bc_ave %||% est_direct$qoi_ave
    d2 <- est_rcpp$bc_ave %||% est_rcpp$qoi_ave
    if (!is.null(d1) && !is.null(d2) && nrow(d1) == nrow(d2)) {
      max_diff <- max(abs(d1$bc_estimate - d2$bc_estimate), na.rm = TRUE)
      res$checks$direct_vs_rcpp_max_diff <- max_diff
    }
  }

  res
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

## ----------------------------------------------------------------------------
## Configuration grid
## ----------------------------------------------------------------------------

grid <- expand.grid(
  N        = c(200, 1000, 5000),
  J        = c(3, 4, 5, 7),
  p_random = c(0, 0.1, 0.3, 0.5),
  shape    = c("uniform", "moderate", "skewed", "extreme"),
  weight_mode = c("none", "vec", "colname"),
  use_anchor = c(TRUE, FALSE),
  n_boot   = 50,
  stringsAsFactors = FALSE
)

## Trim: running the full grid is expensive. Sample configurations
## to get broad coverage without burning hours.
set.seed(42)
grid <- grid[sample.int(nrow(grid), size = 120), ]
rownames(grid) <- NULL

log(sprintf("Running %d configurations", nrow(grid)))

## ----------------------------------------------------------------------------
## Execute
## ----------------------------------------------------------------------------

all_results <- vector("list", nrow(grid))
t_start <- Sys.time()

for (i in seq_len(nrow(grid))) {
  cfg <- as.list(grid[i, ])
  log(sprintf(
    "[%d/%d] N=%d J=%d p=%.1f shape=%s w=%s anchor=%s",
    i, nrow(grid), cfg$N, cfg$J, cfg$p_random, cfg$shape,
    cfg$weight_mode, cfg$use_anchor
  ))
  all_results[[i]] <- tryCatch(
    run_one(cfg),
    error = function(e) {
      list(cfg = cfg, status = "crash",
           errors = conditionMessage(e), timings = list(), checks = list())
    }
  )
}

t_end <- Sys.time()
log(sprintf("Total runtime: %.1f min",
            as.numeric(difftime(t_end, t_start, units = "mins"))))

## ----------------------------------------------------------------------------
## Summarize
## ----------------------------------------------------------------------------

summary_df <- map_dfr(all_results, function(r) {
  tibble(
    N        = r$cfg$N,
    J        = r$cfg$J,
    p_random = r$cfg$p_random,
    shape    = r$cfg$shape,
    weight   = r$cfg$weight_mode,
    anchor   = r$cfg$use_anchor,
    status   = r$status,
    n_errors = length(r$errors),
    errors   = paste(r$errors, collapse = " | "),
    t_direct = r$timings$imprr_direct %||% NA_real_,
    t_rcpp   = r$timings$imprr_direct_rcpp %||% NA_real_,
    t_weights = r$timings$imprr_weights %||% NA_real_,
    direct_vs_rcpp_diff = r$checks$direct_vs_rcpp_max_diff %||% NA_real_
  )
})

saveRDS(all_results, file.path(out_dir, "stress_test_results.rds"))
write.csv(summary_df, file.path(out_dir, "stress_test_summary.csv"),
          row.names = FALSE)

log(sprintf("Done. Pass: %d / Fail: %d / Crash: %d",
            sum(summary_df$status == "ok"),
            sum(summary_df$status == "fail"),
            sum(summary_df$status == "crash")))

message("Stress test complete. See ", out_dir)
