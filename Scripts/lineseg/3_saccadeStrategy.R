library(tidyverse)
library(ggthemes)
library(forcats)
library(binom)
library(lme4)
library(scales)

figXn <- 3.5
figYn <- 2.5
 
fix_dat <- readRDS(file="scratch/processedFixationData.Rda")
trl_dat <- readRDS(file="scratch/processedRTandAccData.Rda")

# only take correct trials
fix_dat <- filter(left_join(fix_dat, trl_dat), accuracy == 1)
trl_dat <- filter(trl_dat, accuracy == 1)


# remove people with poor target easy/absent accuracy
fix_dat <- filter(fix_dat, !(observer %in% c(4, 21, 33, 56, 58)))
trl_dat <- filter(trl_dat, !(observer %in% c(4, 21, 33, 56, 58)))

# remove fixations falling outside of simulus
fix_dat <- filter(fix_dat, is.finite(x), is.finite(y))

# classify every fixation as homo (left), central, or hetro (right)
centralWidth <- 0.1 # used to be 64 pixels! #change to 1 visual degree
fix_dat$side <- 'central'
fix_dat$side[which(fix_dat$x <(0-centralWidth/2))] <- "homo"
fix_dat$side[which(fix_dat$x >(0+centralWidth/2))] <- "hetero"

fix_dat$side <- as_factor(fix_dat$side)

agg_dat = (filter(fix_dat, side!="central", n<6, n>1, targSide=="absent") 
  %>% group_by(observer, session, n) 
    %>% summarise(
    	nTrials=length(trial),
      propHetro=mean(side=="hetero"), 
      lowerS = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
      upperS = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

agg_dat$observer <- fct_reorder(agg_dat$observer, agg_dat$propHetro, fun=mean)

plt <- ggplot(filter(agg_dat, n<=15), 
  aes(x=n, y=propHetro, ymin = lowerS, ymax = upperS, colour = session))
plt <- plt + geom_point() + geom_path() + geom_errorbar()
plt <- plt + facet_wrap(~observer, nrow = 4)
plt <- plt + scale_x_continuous(
  name = "fixation number", breaks = c(2,4,6,8,10))
plt <- plt + scale_y_continuous(
  name = "proportion of fixations to heterogeneous side", limits = c(0,1), breaks = c(0,1))
plt <- plt + theme_bw() + scale_colour_ptol()
plt <- plt + theme(
  legend.justification = c(1,0), 
  legend.position = c(1,0),
  legend.background = element_rect(fill="white"))
ggsave("scratch/strategyBySessionAndPerson.pdf", width = 10, height=6)
ggsave("scratch/strategyBySessionAndPerson.png", width = 10, height=6)

# --------------------------------------------------------------------------------
# how well does search strategy predict RT?
# --------------------------------------------------------------------------------

(left_join(fix_dat, trl_dat) 
  %>% filter(n > 1, n <= 5, side != "central")
  %>% group_by(observer, session, trial, targSide) 
  %>% summarise(
    prop_homo = mean(side == "homo"),
    prop_hetero = mean(side == "hetero"),
    rt    = unique(rt))
  %>% group_by(observer, session, targSide) 
  %>% summarise(
    n_trials = length(trial),
    prop_homo  = mean(prop_homo),
    lowerS = binom.confint(prop_homo*n_trials, n_trials, method='wilson')$lower,
    upperS = binom.confint(prop_homo*n_trials, n_trials, method='wilson')$upper,
    prop_hetero  = mean(prop_hetero),
    median_rt = median(rt),
    meanlogrt = mean(log(rt,2)),
    sderr = sd(log(rt,2)/sqrt(n_trials)),
    upperRT = meanlogrt + 1.96*sderr,
    lowerRT = meanlogrt - 1.96*sderr)
  %>% filter(n_trials > 10) -> dat)

(dat %>% 
  filter(targSide == "absent") %>% 
  select(observer, session, prop_homo, prop_hetero, lowerS, upperS)) -> dat_absent
(dat %>%
  filter(targSide == "hard") %>%
  select(observer, session, meanlogrt, lowerRT, upperRT)) -> dat_hard

dat <- full_join(dat_absent, dat_hard)

r_sess_a = with(filter(dat, session == "a"), cor.test(prop_homo, meanlogrt))
# r_sess_a = format(round(r_sess_a$estimate, 2), nsmall = 2)
r_sess_b = with(filter(dat, session == "b"), cor.test(prop_homo, meanlogrt))
# r_sess_b = format(round(r_sess_b$estimate, 2), nsmall = 2)

r_df = tibble(
  text = c(
    paste("r = ", r_sess_a), 
    paste("r =", r_sess_b)),
  x = c(0.90, 0.90), y = c(12, 11), session = c("a", "b"))

a_labels = c(1, 2, 4, 8, 16)
a_breaks = log(1000*a_labels, 2)

plt <- ggplot(dat, aes(x = prop_homo, xmin = lowerS, xmax = upperS, y = meanlogrt, ymin = lowerRT, ymax = upperRT, colour = session))
plt <- plt + geom_errorbar(size=0.25, colour = "grey")
plt <- plt + geom_errorbarh(size=0.25, colour = "grey")
plt <- plt + geom_point() + geom_smooth(method = lm, fullrange = TRUE, se = F)
plt <- plt + theme_bw() + scale_colour_ptol()
plt <- plt + scale_x_continuous("prop. homo fixations ", 
  limits = c(0, 1), breaks = c(0, 1))
plt <- plt + scale_y_continuous("reaction time (s)",
  limits = c(10.7, 14.5), breaks = a_breaks, labels = a_labels, expand = c(0, 0))
# plt <- plt + coord_cartesian(xlim=c(0,1), ylim=c(10.5, 14.5))
plt <- plt + theme(
  legend.justification = c(-0.05,1), 
  legend.position = c(0,0.99),
  legend.key = element_rect(fill = "white"),
  legend.background = element_rect(fill="white", colour = "white"),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
plt
ggsave("./scratch/strat_compare_meanlog_rt.pdf", width = figYn, height = figYn)
ggsave("./scratch/strat_compare_meanlog_rt.png", width = figYn, height = figYn)

# --------------------------------------------------------------------------------
# how consistent is stratety between session a and b?
# --------------------------------------------------------------------------------

agg_dat = (filter(fix_dat, side!="central", n<=5, n>1, targSide=="absent") 
  %>% group_by(observer, session) 
    %>% summarise(
      nTrials=length(trial),
      propHetro=mean(side=="hetero"), 
      lowerS = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
      upperS = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))


(agg_dat 
  %>% select(-nTrials)
  %>% unite(propHetro, propHetro, lowerS, upperS) 
  %>% spread(key = session, value = propHetro )
  %>%  separate(a, into = c("a_strat", "a_lower", "a_upper"), sep = "_", convert = TRUE) 
  %>%  separate(b, into = c("b_strat", "b_lower", "b_upper"), sep = "_", convert = TRUE)) -> t_rt_dat


cor.test(t_rt_dat$a_strat, t_rt_dat$b_strat)



plt <- ggplot(t_rt_dat, aes(x = a_strat, y = b_strat, xmin = a_lower, xmax = a_upper, ymin = b_lower, ymax = b_upper))
plt <- plt + geom_errorbar(size=0.25, colour = "grey") 
plt <- plt + geom_errorbarh(size=0.25, colour = "grey")
plt <- plt + geom_point()
plt <- plt + geom_abline( linetype=2)
plt <- plt + geom_smooth(method = lm, se = T, colour = "black", fullrange = TRUE)
plt <- plt + scale_x_continuous("a: prop. hetero. fixations", 
  limits = c(-1, 2), breaks = c(0,1))
plt <- plt + scale_y_continuous("b: prop. hetero. fixations", 
    limits = c(-1, 2), breaks = c(0,1))
plt <- plt + coord_cartesian(xlim=c(0,1), ylim=c(0,1)) 
plt <- plt + theme_bw() + scale_color_ptol(name = "target condition")
plt <- plt + theme(legend.title=element_blank(),
 panel.grid.major = element_blank(),
 panel.grid.minor = element_blank())
# plt <- plt + geom_text(label = "r = 0.72", x = 0.25, y = 0.85)
ggsave("scratch/strat_corr.pdf", width = figYn, height = figYn)
ggsave("scratch/strat_corr.png", width = figYn, height = figYn)

# --------------------------------------------------------------------------------
# output data for cross experiment correlations!
# base statistic for correlation on only first 3 fixations
# use session B only
# --------------------------------------------------------------------------------

write_csv(filter(dat, session == "b"), "scratch/lineseg_output.csv")