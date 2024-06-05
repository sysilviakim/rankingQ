#' Bias-correct the Distribution of Ranking Permutations
#' Using a Paired Anchor Question and Produce De-biased Average Rankings
#'
#' @description This function implements the bias correction of the ranking
#' distribution using a paired anchor question.
#'
#' @importFrom dplyr `%>%` mutate select group_by arrange summarise
#' @importFrom tidyselect matches
#' @importFrom estimatr lm_robust
#'
#' @param data The input dataset with ranking data.
#' @param J The number of items in the ranking question. Defaults to NULL,
#' in which case it will be inferred from the data.
#' @param main_q Column name for the main ranking question to be analyzed.
#' @param anchor_q Column name for the paired anchor question.
#' @param anc_correct Indicator for passing the anchor question.
#' @param anc_correct_pattern The correct pattern to pass the anchor question
#' filter, given the reference set. Defaults to NULL.
#' If NULL, it will taken on a J-length string with a natural progression of
#' numbers, such as "123", "1234", "1234567", and so on.
#' @param n_bootstrap Number of bootstraps. Defaults to 200.
#' @param seed Seed for \code{set.seed} for reproducibility.
#'
#' @return A list.
#'
#' @export

imprr_direct <- function(data,
                         J = NULL,
                         main_q,
                         # anchor_q,
                         anc_correct,
                         # anc_correct_pattern = NULL,
                         n_bootstrap = 200,
                         seed = 123456,
                         weight = NULL) {
  ## Suppress global variable warning
  estimate <- g_U <- est.p.random <- item <- qoi <-
    outcome <- bc_estimate <- NULL

  # Setup ======================================================================
  N <- nrow(data)
  if (is.null(J)) {
    J <- nchar(data[[main_q]][[1]])
  }

  if (is.null(weight)) {
    weight <- rep(1, N)
  }


  # Check the validity of the input arguments ==================================

  ## List for bootstrapped results
  list_weights <- list_qoi <- list_prop <- vector("list", length = n_bootstrap)

  # Boostrapping ===============================================================
  ## We bootstrap to account for uncertainty from the estimation of Pr(random)
  ## Sample with replacement
  ## There are three loops here with
  ### i: over n_bootstrap
  ### j: over J items
  ### k: over K estimates

  set.seed(seed)
  for (i in 1:n_bootstrap) {
    ## Sample indices
    index <- sample(1:nrow(data), size = nrow(data), replace = TRUE)

    ## This is the bootstrapped data
    boostrap_dat <- data[index, ]

    # This will be cut out since we won't use it
    # ## Anchor ranking only
    # loc_anc <- boostrap_dat %>%
    #   select(matches(anchor_q)) %>%
    #   select(matches("_[[:digit:]]$"))

    ## Main ranking only (Silvia, I edited here slightly)
    loc_app <- boostrap_dat %>%
      select(matches(main_q)) %>%
      select(matches("_[[:digit:]]$"))

    # Step 1: Get the proportion of random answers
    ## This requires anchor questions and item order randomization
    p_non_random <- (mean(boostrap_dat[[anc_correct]]) - 1 / factorial(J)) /
      (1 - 1 / factorial(J))

    # Step 2: Get the naive estimates of simple quantities

    item_names <- colnames(loc_app)
    J_1 <- J - 1
    all_qoi_list <- list()

    for (j in 1:J) {
      # Specify each item as the target item in return
      target_item <- item_names[j]
      other_items <- item_names[-j]

      # Step 2.1: Locally code a few quantities
      ## Marginal rank
      Y_rank_target <- boostrap_dat[target_item] %>% pull()

      ## Pairwise ranking indicator
      Y_pairwise <- list()
      for (k in 1:J_1) {
        compar <- boostrap_dat[other_items[k]] %>% pull() # Comparison item
        Y_pairwise[[k]] <- ifelse(Y_rank_target < compar, 1, 0)
      }

      # Top-k ranking indicator
      Y_top <- list()
      for (k in 1:J_1) {
        Y_top[[k]] <- ifelse(Y_rank_target <= k, 1, 0)
      }

      # Marginal ranking indicator
      Y_marginal <- list()
      tgt <- boostrap_dat[target_item] %>% pull()
      for (k in 1:J) {
        Y_marginal[[k]] <- ifelse(tgt == k, 1, 0)
      }

      # Step 2.2: Get raw estimates of
      ## Average ranks
      m_rank_target <- lm_robust(Y_rank_target ~ 1, weights = weight) %>% tidy()

      ## Pairwise ranking probabilities
      m_pairwise <- list()
      for (k in 1:J_1) {
        m_pairwise[[k]] <- lm_robust(Y_pairwise[[k]] ~ 1, weights = weight) %>% tidy()
      }

      ## Top-k ranking probabilities
      m_top <- list()
      for (k in 1:J_1) {
        m_top[[k]] <- lm_robust(Y_top[[k]] ~ 1, weights = weight) %>% tidy()
      }

      ## Marginal ranking probabilities
      m_marginal <- list()
      for (k in 1:J) {
        m_marginal[[k]] <- lm_robust(Y_marginal[[k]] ~ 1, weights = weight) %>% tidy()
      }

      # Step 3: Get the QOI based on random responses
      ## g(random)---QOI based on uniform distribution
      gg_averagerank <- m_rank_target %>%
        select(estimate) %>%
        mutate(
          outcome = paste0("Avg:", " ", target_item),
          qoi = "average rank",
          g_U = (1 + J) / 2
        )

      gg_pairwise <- do.call(rbind.data.frame, m_pairwise) %>%
        select(estimate) %>%
        mutate(
          outcome = paste0("v.", " ", other_items),
          qoi = "pairwise ranking",
          g_U = 0.5
        )

      gg_topk <- do.call(rbind.data.frame, m_top) %>%
        select(estimate) %>%
        mutate(
          outcome = paste0("Top-", "", 1:J_1),
          qoi = "top-k ranking",
          g_U = 1 / J
        )

      gg_marginal <- do.call(rbind.data.frame, m_marginal) %>%
        select(estimate) %>%
        mutate(
          outcome = paste0("Ranked", " ", 1:J),
          qoi = "marginal ranking",
          g_U = 1 / J
        )

      # Step 4: Directly apply bias-correction (Equation 6)
      ## bc_estimate: bias-corrected estimate
      all_qoi <- rbind(
        gg_averagerank,
        gg_pairwise,
        gg_topk,
        gg_marginal
      ) %>%
        mutate(
          bc_estimate = (estimate - (g_U * (1 - p_non_random))) / p_non_random,
          item = target_item
        )

      ## Save the estimates for all quantities
      all_qoi_list[[j]] <- all_qoi
    }

    all_qoi_df <- do.call(rbind.data.frame, all_qoi_list)

    list_prop[[i]] <- 1 - p_non_random # Estimated prop of random responses
    list_qoi[[i]] <- all_qoi_df # Bias-corrected estimates of several QOIs
  }
  message("Bootstrapping finished.")

  # Summarize results
  ## Compute the mean and 95% CI based on bootstrapping
  ## Return all weights for IPW

  df_random <- do.call(rbind.data.frame, list_prop)
  colnames(df_random) <- "est.p.random"
  df_random_summary <- df_random %>%
    summarise(
      mean = mean(est.p.random),
      lower = quantile(est.p.random, 0.025),
      upper = quantile(est.p.random, 0.975)
    )

  df_qoi_summary <- do.call(rbind.data.frame, list_qoi) %>%
    group_by(item, qoi, outcome) %>%
    arrange(bc_estimate) %>%
    summarise(
      mean = mean(bc_estimate),
      lower = quantile(bc_estimate, 0.025),
      upper = quantile(bc_estimate, 0.975)
    )

  return(
    list(
      est_p_random = df_random_summary,
      qoi = df_qoi_summary
    )
  )
}
