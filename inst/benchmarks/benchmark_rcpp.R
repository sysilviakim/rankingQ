# Benchmark comparing tidyverse vs Rcpp implementations
# Run this after installing the package with Rcpp support
#
# The Rcpp version (imprr_direct_rcpp) is typically 100-300x faster than
# the tidyverse version (imprr_direct).

library(rankingQ)
library(dplyr)

# Load example data
data(identity_w)

# Parameters
main_q <- "app_identity"
anc_correct <- "anc_correct_identity"
J <- 4

cat("=== Benchmark: imprr_direct vs imprr_direct_rcpp ===\n\n")
cat("Data dimensions:", nrow(identity_w), "rows,", J, "items\n")
cat("Number of bootstrap samples: 200\n\n")

# Benchmark tidyverse version
cat("Running tidyverse version (imprr_direct)...\n")
time_tidy <- system.time({
  result_tidy <- imprr_direct(
    data = identity_w,
    J = J,
    main_q = main_q,
    anc_correct = anc_correct,
    n_bootstrap = 200,
    seed = 123456
  )
})["elapsed"]
cat("Tidyverse time:", round(time_tidy, 2), "seconds\n\n")

# Benchmark Rcpp version
cat("Running Rcpp version (imprr_direct_rcpp)...\n")
time_rcpp <- system.time({
  result_rcpp <- imprr_direct_rcpp(
    data = identity_w,
    J = J,
    main_q = main_q,
    anc_correct = anc_correct,
    n_bootstrap = 200,
    seed = 123456
  )
})["elapsed"]
cat("Rcpp time:", round(time_rcpp, 2), "seconds\n\n")

# Calculate speedup
speedup <- time_tidy / time_rcpp
cat("=== Results ===\n")
cat("Tidyverse:", round(time_tidy, 2), "seconds\n")
cat("Rcpp:", round(time_rcpp, 2), "seconds\n")
cat("Speedup:", round(speedup, 0), "x faster\n\n")

# Verify results are similar
cat("=== Verification ===\n")
cat("Comparing estimated proportion of random responses:\n")
cat("  Tidyverse:", round(result_tidy$est_p_random$mean, 4), "\n")
cat("  Rcpp:", round(result_rcpp$est_p_random$mean, 4), "\n")

# Compare average ranks for first item
cat("\nComparing average ranks for item 1 (Party):\n")
tidy_avg1 <- result_tidy$results %>%
  filter(qoi == "average rank", item == paste0(main_q, "_1")) %>%
  pull(mean)
rcpp_avg1 <- result_rcpp$results %>%
  filter(qoi == "average rank", item == paste0(main_q, "_1")) %>%
  pull(mean)
cat("  Tidyverse:", round(tidy_avg1, 4), "\n")
cat("  Rcpp:", round(rcpp_avg1, 4), "\n")

# Extended benchmark with different bootstrap counts
cat("\n=== Extended Benchmark: Varying Bootstrap Samples ===\n")
n_boots <- c(100, 200, 500, 1000)

for (nb in n_boots) {
  cat("\nn_bootstrap =", nb, "\n")

  # Tidyverse (only run small ones to save time)
  if (nb <= 200) {
    t_tidy <- system.time({
      imprr_direct(identity_w, J = J, main_q = main_q,
                   anc_correct = anc_correct, n_bootstrap = nb, seed = 123)
    })["elapsed"]
  } else {
    t_tidy <- time_tidy * (nb / 200)  # Estimate based on linear scaling
    cat("  (Tidyverse estimated)\n")
  }

  # Rcpp
  t_rcpp <- system.time({
    imprr_direct_rcpp(identity_w, J = J, main_q = main_q,
                      anc_correct = anc_correct, n_bootstrap = nb, seed = 123)
  })["elapsed"]

  cat("  Tidyverse:", round(t_tidy, 2), "s | Rcpp:", round(t_rcpp, 2),
      "s | Speedup:", round(t_tidy / t_rcpp, 0), "x\n")
}

cat("\n=== Benchmark Complete ===\n")
cat("\nRecommendation: Use imprr_direct_rcpp() for production use.\n")
