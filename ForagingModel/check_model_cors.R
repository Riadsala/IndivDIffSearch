# do foraging model fits correlate with anything in the search tasks?

library(tidyverse)
library(rstan)
library(tidybayes)


# read in model
m <- readRDS("all_qjep_2020.rds")

# extract indiv params

m %>% spread_draws(b[param], u[param, person]) %>% 
  select(-.iteration, -.chain) %>%
  mutate(
    condition = if_else(param <= 4, "feature", "conjunction"),
    b = b  + u,
    param = as_factor(param),
    param = fct_recode(param, 
                       bA = "1", bS = "2", bP = "3", bD = "4",
                       bA = "5", bS = "6", bP = "7", bD = "8")) %>%
  select(-u) %>%
  pivot_wider(names_from = param, values_from = b) %>%
  group_by(person, condition) %>% 
  median_hdci(bA, bS, bP, bD) -> post

# plot and check I have coded things properly
post %>% ggplot(aes(bP, bD, colour = condition)) + geom_point()

# now read in visual search data (summary stats)!
search <- read_csv("../Scripts/summaryData.csv") %>%
  # fix subject number
  rename(person = "observer") %>%
  mutate(person = 1:n())

# if I have done things correctly, bS should correlated with the switch rates here!

post %>% filter(condition == "conjunction") %>%
  full_join(search, by = "person") -> d

# not great... why? not great summary stat (mean or median??)
ggplot(d, aes(bS, fg_conj_run_num)) + geom_point()
ggplot(d, aes(bS, fg_conj_run_length)) + geom_point()

# what about bP and search strat?
post %>% filter(condition == "feature") %>%
  full_join(search, by = "person") -> d

cor.test(d$ls_prop_hetero, d$bA)
cor.test(d$ls_prop_hetero, d$bS)
cor.test(d$ls_prop_hetero, d$bP)
cor.test(d$ls_prop_hetero, d$bD)

cor.test(d$ls_prop_hetero, d$fg_feat_log2)

cor.test(d$ac_propOpt, d$bP)
cor.test(d$ls_prop_hetero, d$fg_conj_run_length)



ggplot(d, aes(bP, ls_prop_hetero)) + geom_point() +
  geom_smooth(method = "lm")


post %>% filter(condition == "conjunction") %>%
  full_join(search, by = "person") -> d

cor.test(d$ac_propOpt, d$bD)
cor.test(d$fg_conj_run_num, d$bS)

summary(lm(data = d, fg_conj_log2 ~ bS+bP))


cor.test(d$bP, d$fg_conj_log2)
