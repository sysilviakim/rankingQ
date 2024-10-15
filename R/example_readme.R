
remotes::install_github(
  "sysilviakim/rankingQ",
  INSTALL_opts = c("--no-multiarch"),
  dependencies = TRUE
)

library(rankingQ)
library(tidyverse)

load("data/identity_ranking.Rda")

head(identity_ranking)

identity_ranking <- identity_ranking %>%
  rename(app_identity_1 = app_party,
         app_identity_2 = app_religion,
         app_identity_3 = app_gender,
         app_identity_4 = app_race)


out_direct <- imprr_direct(
  data = identity_ranking,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)


out_direct$est_p_random


out_direct$qoi <- out_direct$qoi %>%
  mutate(item = case_when(item == "app_identity_1" ~ "party",
                          item == "app_identity_2" ~ "religion",
                          item == "app_identity_3" ~ "gender",
                          item == "app_identity_4" ~ "race"))

out_direct$qoi %>%
  filter(item == "party")



out_direct$qoi %>%
  filter(qoi == "average rank")




