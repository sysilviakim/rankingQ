
library(tidyverse)
library(clarify)

m1 <- readRDS(file="m1.RData")
m2 <- readRDS(file="m2.RData")

# Let's try all possible rankings
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
  geom_point(alpha = 0.4) +
  geom_pointrange(aes(ymin = low, ymax = high), alpha = 0.4) +
  scale_color_manual(values = c("darkcyan", "darkred")) +
  facet_wrap(~ ranking) +
  theme_bw() +
  xlim(1, 7) +
  xlab("Ideology (liberal - conservative)") +
  ylab("Predicted Probability") +
  ggtitle("Effect of Ideology on Relative Partisanship") -> p

p

ggsave(here::here("placketluce_weight.pdf"),
       width = 12, height = 7
)
