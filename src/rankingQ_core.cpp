#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double weighted_mean_cpp(NumericVector x, NumericVector weights) {
  int n = x.size();
  double sum_wx = 0.0;
  double sum_w = 0.0;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(weights[i])) {
      sum_wx += x[i] * weights[i];
      sum_w += weights[i];
    }
  }

  return sum_wx / sum_w;
}

// [[Rcpp::export]]
double weighted_var_cpp(NumericVector x, NumericVector weights) {
  int n = x.size();
  double sum_w = 0.0;
  double sum_wx = 0.0;
  double sum_wx2 = 0.0;

  for (int i = 0; i < n; i++) {
    if (!NumericVector::is_na(x[i]) && !NumericVector::is_na(weights[i])) {
      sum_w += weights[i];
      sum_wx += weights[i] * x[i];
      sum_wx2 += weights[i] * x[i] * x[i];
    }
  }

  double mean = sum_wx / sum_w;
  return (sum_wx2 / sum_w) - (mean * mean);
}

// [[Rcpp::export]]
NumericVector pairwise_indicator_cpp(
    NumericVector target,
    NumericVector comparison) {
  int n = target.size();
  NumericVector result(n);

  for (int i = 0; i < n; i++) {
    result[i] = (target[i] < comparison[i]) ? 1.0 : 0.0;
  }

  return result;
}

// [[Rcpp::export]]
NumericVector topk_indicator_cpp(NumericVector ranks, int k) {
  int n = ranks.size();
  NumericVector result(n);

  for (int i = 0; i < n; i++) {
    result[i] = (ranks[i] <= k) ? 1.0 : 0.0;
  }

  return result;
}

// [[Rcpp::export]]
NumericVector marginal_indicator_cpp(NumericVector ranks, int k) {
  int n = ranks.size();
  NumericVector result(n);

  for (int i = 0; i < n; i++) {
    result[i] = (ranks[i] == k) ? 1.0 : 0.0;
  }

  return result;
}

// [[Rcpp::export]]
double bias_correct_cpp(double estimate, double g_U, double p_non_random) {
  return (estimate - (g_U * (1.0 - p_non_random))) / p_non_random;
}

// [[Rcpp::export]]
NumericVector bias_correct_vec_cpp(
    NumericVector estimates,
    NumericVector g_U,
    double p_non_random) {
  int n = estimates.size();
  NumericVector result(n);

  for (int i = 0; i < n; i++) {
    result[i] =
      (estimates[i] - (g_U[i] * (1.0 - p_non_random))) / p_non_random;
  }

  return result;
}

// [[Rcpp::export]]
double prop_non_random_cpp(double prop_correct, int J) {
  double J_factorial = 1.0;
  for (int i = 2; i <= J; i++) {
    J_factorial *= i;
  }
  double inv_factorial = 1.0 / J_factorial;
  return (prop_correct - inv_factorial) / (1.0 - inv_factorial);
}

// [[Rcpp::export]]
List compute_qoi_stats_cpp(NumericMatrix data, NumericVector weights, int J) {
  int N = data.nrow();
  int J_1 = J - 1;
  double J_factorial = 1.0;
  for (int i = 2; i <= J; i++) {
    J_factorial *= i;
  }

  double uniform_avg_rank = (1.0 + J) / 2.0;
  double uniform_pairwise = 0.5;
  double uniform_prob = 1.0 / J;

  // Results storage
  NumericVector avg_ranks(J);
  NumericMatrix pairwise_probs(J, J_1);
  NumericMatrix topk_probs(J, J_1);
  NumericMatrix marginal_probs(J, J);

  double sum_w = 0.0;
  for (int i = 0; i < N; i++) {
    sum_w += weights[i];
  }

  // For each item j
  for (int j = 0; j < J; j++) {
    // Average rank
    double sum_wx = 0.0;
    for (int i = 0; i < N; i++) {
      sum_wx += data(i, j) * weights[i];
    }
    avg_ranks[j] = sum_wx / sum_w;

    // Pairwise probabilities
    int pair_idx = 0;
    for (int k = 0; k < J; k++) {
      if (k != j) {
        double sum_pair = 0.0;
        for (int i = 0; i < N; i++) {
          if (data(i, j) < data(i, k)) {
            sum_pair += weights[i];
          }
        }
        pairwise_probs(j, pair_idx) = sum_pair / sum_w;
        pair_idx++;
      }
    }

    // Top-k probabilities
    for (int k = 0; k < J_1; k++) {
      double sum_topk = 0.0;
      for (int i = 0; i < N; i++) {
        if (data(i, j) <= (k + 1)) {
          sum_topk += weights[i];
        }
      }
      topk_probs(j, k) = sum_topk / sum_w;
    }

    // Marginal probabilities
    for (int k = 0; k < J; k++) {
      double sum_marg = 0.0;
      for (int i = 0; i < N; i++) {
        if (data(i, j) == (k + 1)) {
          sum_marg += weights[i];
        }
      }
      marginal_probs(j, k) = sum_marg / sum_w;
    }
  }

  return List::create(
    Named("avg_ranks") = avg_ranks,
    Named("pairwise_probs") = pairwise_probs,
    Named("topk_probs") = topk_probs,
    Named("marginal_probs") = marginal_probs,
    Named("uniform_avg_rank") = uniform_avg_rank,
    Named("uniform_pairwise") = uniform_pairwise,
    Named("uniform_prob") = uniform_prob
  );
}

// [[Rcpp::export]]
List bootstrap_qoi_cpp(
    NumericMatrix data,
    NumericVector anc_correct,
    NumericVector weights,
    int J,
    int n_bootstrap,
    int seed) {
  int N = data.nrow();
  int J_1 = J - 1;

  double J_factorial = 1.0;
  for (int i = 2; i <= J; i++) {
    J_factorial *= i;
  }

  double uniform_avg_rank = (1.0 + J) / 2.0;
  double uniform_pairwise = 0.5;
  double uniform_prob = 1.0 / J;

  // Storage for bootstrap results
  NumericVector p_random_boot(n_bootstrap);
  NumericMatrix avg_ranks_boot(n_bootstrap, J);
  NumericMatrix avg_ranks_raw_boot(n_bootstrap, J);
  // For simplicity, store flattened pairwise, topk, marginal
  int n_pairwise = J * J_1;
  int n_topk = J * J_1;
  int n_marginal = J * J;
  NumericMatrix pairwise_boot(n_bootstrap, n_pairwise);
  NumericMatrix pairwise_raw_boot(n_bootstrap, n_pairwise);
  NumericMatrix topk_boot(n_bootstrap, n_topk);
  NumericMatrix topk_raw_boot(n_bootstrap, n_topk);
  NumericMatrix marginal_boot(n_bootstrap, n_marginal);
  NumericMatrix marginal_raw_boot(n_bootstrap, n_marginal);

  // Set seed for reproducibility
  Environment base_env("package:base");
  Function set_seed = base_env["set.seed"];
  set_seed(seed);

  // Pre-allocate indices vector
  IntegerVector indices(N);

  for (int b = 0; b < n_bootstrap; b++) {
    // Generate bootstrap indices using R's RNG (already seeded)
    for (int i = 0; i < N; i++) {
      indices[i] = (int)(R::runif(0, 1) * N);
      if (indices[i] >= N) indices[i] = N - 1; // Safety bound
    }

    // Compute p_non_random for this bootstrap
    double sum_correct = 0.0;
    double sum_w = 0.0;
    for (int i = 0; i < N; i++) {
      int idx = indices[i];
      sum_correct += anc_correct[idx] * weights[idx];
      sum_w += weights[idx];
    }
    double prop_correct = sum_correct / sum_w;
    double p_non_random =
      (prop_correct - 1.0 / J_factorial) / (1.0 - 1.0 / J_factorial);
    if (!R_finite(p_non_random) || p_non_random <= 1e-12) {
      p_random_boot[b] = NA_REAL;
      for (int j = 0; j < J; j++) {
        avg_ranks_boot(b, j) = NA_REAL;
        avg_ranks_raw_boot(b, j) = NA_REAL;
      }
      for (int c = 0; c < n_pairwise; c++) {
        pairwise_boot(b, c) = NA_REAL;
        pairwise_raw_boot(b, c) = NA_REAL;
      }
      for (int c = 0; c < n_topk; c++) {
        topk_boot(b, c) = NA_REAL;
        topk_raw_boot(b, c) = NA_REAL;
      }
      for (int c = 0; c < n_marginal; c++) {
        marginal_boot(b, c) = NA_REAL;
        marginal_raw_boot(b, c) = NA_REAL;
      }
      continue;
    }
    p_random_boot[b] = 1.0 - p_non_random;

    // Compute QOI for each item
    for (int j = 0; j < J; j++) {
      // Average rank
      double sum_wx = 0.0;
      double sum_w_item = 0.0;
      for (int i = 0; i < N; i++) {
        int idx = indices[i];
        sum_wx += data(idx, j) * weights[idx];
        sum_w_item += weights[idx];
      }
      double raw_avg = sum_wx / sum_w_item;
      avg_ranks_raw_boot(b, j) = raw_avg;
      avg_ranks_boot(b, j) =
        (raw_avg - uniform_avg_rank * (1.0 - p_non_random)) / p_non_random;

      // Pairwise
      int pair_idx = 0;
      for (int k = 0; k < J; k++) {
        if (k != j) {
          double sum_pair = 0.0;
          for (int i = 0; i < N; i++) {
            int idx = indices[i];
            if (data(idx, j) < data(idx, k)) {
              sum_pair += weights[idx];
            }
          }
          double raw_pair = sum_pair / sum_w_item;
          pairwise_raw_boot(b, j * J_1 + pair_idx) = raw_pair;
          pairwise_boot(b, j * J_1 + pair_idx) =
            (raw_pair - uniform_pairwise * (1.0 - p_non_random)) /
            p_non_random;
          pair_idx++;
        }
      }

      // Top-k
      for (int k = 0; k < J_1; k++) {
        double sum_topk = 0.0;
        for (int i = 0; i < N; i++) {
          int idx = indices[i];
          if (data(idx, j) <= (k + 1)) {
            sum_topk += weights[idx];
          }
        }
        double raw_topk = sum_topk / sum_w_item;
        topk_raw_boot(b, j * J_1 + k) = raw_topk;
        // For top-k, g_U = (k+1)/J
        double g_U_topk = (double)(k + 1) / J;
        topk_boot(b, j * J_1 + k) =
          (raw_topk - g_U_topk * (1.0 - p_non_random)) / p_non_random;
      }

      // Marginal
      for (int k = 0; k < J; k++) {
        double sum_marg = 0.0;
        for (int i = 0; i < N; i++) {
          int idx = indices[i];
          if (data(idx, j) == (k + 1)) {
            sum_marg += weights[idx];
          }
        }
        double raw_marg = sum_marg / sum_w_item;
        marginal_raw_boot(b, j * J + k) = raw_marg;
        marginal_boot(b, j * J + k) =
          (raw_marg - uniform_prob * (1.0 - p_non_random)) / p_non_random;
      }
    }
  }

  return List::create(
    Named("p_random") = p_random_boot,
    Named("avg_ranks") = avg_ranks_boot,
    Named("avg_ranks_raw") = avg_ranks_raw_boot,
    Named("pairwise") = pairwise_boot,
    Named("pairwise_raw") = pairwise_raw_boot,
    Named("topk") = topk_boot,
    Named("topk_raw") = topk_raw_boot,
    Named("marginal") = marginal_boot,
    Named("marginal_raw") = marginal_raw_boot
  );
}

// [[Rcpp::export]]
NumericVector weighted_table_cpp(CharacterVector x, NumericVector weights) {
  int n = x.size();

  // Get unique values
  CharacterVector unique_vals = Rcpp::unique(x);
  int n_unique = unique_vals.size();

  NumericVector result(n_unique);
  CharacterVector result_names(n_unique);

  for (int i = 0; i < n_unique; i++) {
    result_names[i] = unique_vals[i];
    double sum_w = 0.0;
    for (int j = 0; j < n; j++) {
      if (x[j] == unique_vals[i]) {
        sum_w += weights[j];
      }
    }
    result[i] = sum_w;
  }

  result.attr("names") = result_names;
  return result;
}
