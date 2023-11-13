
library(tidyverse)
library(clarify)

m1 <- readRDS(file="m1.RData")
m2 <- readRDS(file="m2.RData")

# Let's try all possible rankings
tgt_ranking <- c("gender", "race", "party", "religion")
all_rankings <- combinat::permn(tgt_ranking)
out_vec_raw <- list()
out_vec_correct <- list()

for(i in 1:24){

out_vec_raw[[i]] <- sim_rank_randeff(m = m1,
                                     permn = all_rankings[[i]],
                                     random_var = "ideo7",
                                     range_cont = 1:7,
                                     seed = 123)


out_vec_correct[[i]] <- sim_rank_randeff(m = m2,
                                         permn = all_rankings[[i]],
                                         random_var = "ideo7",
                                         range_cont = 1:7,
                                         seed = 123)
}

out_vec_raw
out_vec_correct


as.list(out_vec_raw) %>%
  bind_rows() %>%
  group_by(ideo7) %>%
  summarize(sum_mean = sum(mean)) %>%
  print()


as.list(out_vec_correct) %>%
  bind_rows() %>%
  group_by(ideo7) %>%
  summarize(sum_mean = sum(mean)) %>%
  print()

# Probs sum up to one!

dt_raw <- as.list(out_vec_raw) %>%
  bind_rows() %>%
  mutate(Result = "Raw Data")

dt_correct <- as.list(out_vec_correct) %>%
  bind_rows() %>%
  mutate(Result = "Bias Corrected")

ggdt <- rbind(dt_raw, dt_correct)


# Visualize the results
ggdt %>%
  ggplot(aes(x = ideo7, y = mean, color = Result)) +
  geom_point(size = 0.3) +
  geom_pointrange(aes(ymin = low, ymax = high), size = 0.3) +
  scale_color_manual(values = c("darkcyan", "darkred")) +
  facet_wrap(~ ranking) +
  theme_bw() +
  xlim(1, 7) +
  xlab("Ideology (liberal - conservative)") +
  ylab("Predicted Probability") +
  ggtitle("Effect of Ideology on Relative Partisanship") -> p

p

ggsave(here::here("placketluce_weight.pdf"),
       width = 12, height = 7)


# Save the most interesting results seperately
ggdt_sub <- ggdt %>%
  filter(Result == "Bias Corrected",
         ranking %in% c("gender_party_race_religion",
                        "gender_race_party_religion",
                        "gender_race_religion_party",
                        "gender_religion_race_party",
                        "race_gender_party_religion",
                        "religion_gender_race_party"))

ggdt_sub %>%
  ggplot(aes(x = ideo7, y = mean)) +
  geom_point(size = 0.3, color = "darkcyan") +
  geom_pointrange(aes(ymin = low, ymax = high), size = 0.3, color = "darkcyan") +
  facet_wrap(~ ranking) +
  theme_bw() +
  xlim(1, 7) +
  ylim(0, 0.25) +
  xlab("Ideology (liberal - conservative)") +
  ylab("Predicted Probability") +
  ggtitle("Effect of Ideology on Relative Partisanship") -> p_sub

p_sub
ggsave(here::here("placketluce_ideology_sub.pdf"),
       width = 8, height = 4)


# Study the effect of partisanship

m_pid <- readRDS(file="m_pid.RData")
out_vec_correct <- list()

for(i in 1:24){
out_vec_correct[[i]] <- sim_rank_randeff(m = m_pid,
                                           permn = all_rankings[[i]],
                                           random_var = "pid7",
                                           range_cont = 1:7,
                                           seed = 123)
}

ggdt_pid <- as.list(out_vec_correct) %>%
  bind_rows() %>%
  filter(ranking %in% c("gender_party_race_religion",
                 "gender_race_party_religion",
                 "gender_race_religion_party",
                 "gender_religion_race_party",
                 "race_gender_party_religion",
                 "religion_gender_race_party"))


ggdt_pid %>%
  ggplot(aes(x = pid7, y = mean)) +
  geom_point(size = 0.3, color = "darkcyan") +
  geom_pointrange(aes(ymin = low, ymax = high), size = 0.3, color = "darkcyan") +
  facet_wrap(~ ranking) +
  theme_bw() +
  xlim(1, 7) +
  ylim(0, 0.25) +
  xlab("Party ID (Strong D to Strong R)") +
  ylab("Predicted Probability") +
  ggtitle("Effect of Party ID on Relative Partisanship") -> p_pid

p_pid
ggsave(here::here("placketluce_pid_sub.pdf"),
       width = 8, height = 4)
