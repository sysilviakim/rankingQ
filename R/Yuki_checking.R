
library(tidyverse)
library(clarify)

m2 <- readRDS(file="m2.RData")

# Check Silvia's target ranking
tgt_ranking <- c("gender", "religion", "race", "party")

out <- sim_rank_randeff(m2, permn = tgt_ranking,
                        random_var = "ideo7",
                        range_cont = 1,
                        seed = 123)

out # probs seem to be too low



# Let's try all possible rankings
all_rankings <- combinat::permn(tgt_ranking)

# # Just caring the first choice for checking
# all_rankings <- list(c("party", "race", "religion", "gender"),
#                      c("race", "party", "religion", "gender"),
#                      c("religion", "race", "party", "gender"),
#                      c("gender", "race", "religion", "party"))

out_vec <- list()

for(i in 1:24){
out_vec[[i]] <- sim_rank_randeff(m = m2,
                                 permn = all_rankings[[i]],
                                 random_var = "ideo7",
                                 range_cont = 1:7,
                                 seed = 123)
}

out_vec

as.list(out_vec) %>%
  bind_rows() %>%
  group_by(ideo7) %>%
  summarize(sum_mean = sum(mean)) %>%
  print()

# Probs do not sum up to one



