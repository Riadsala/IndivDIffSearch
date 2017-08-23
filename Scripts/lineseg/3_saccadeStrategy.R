library(tidyverse)
library(ggthemes)
library(forcats)
library(binom)
library(lme4)
library(scales)

figXn <- 6
figYn <- 4
 
fix_dat <- readRDS(file="scratch/processedFixationData.Rda")
trl_dat <- readRDS(file="scratch/processedRTandAccData.Rda")

# fix suspected coding bug

# person10_a_fix <- filter(fix_dat, observer == 10, session == 'a')
# person11_a_fix <- filter(fix_dat, observer == 11, session == 'a')
# person10_a_fix$observer <- 11
# person11_a_fix$observer <- 10
# fix_dat <- filter(fix_dat, !(observer == 10 & session == 'a'))
# fix_dat <- filter(fix_dat, !(observer == 11 & session == 'a'))
# fix_dat <- rbind(fix_dat, person10_a_fix, person11_a_fix)

# person10_a_trl <- filter(trl_dat, observer == 10, session == 'a')
# person11_a_trl <- filter(trl_dat, observer == 11, session == 'a')
# person10_a_trl$observer <- 11
# person11_a_trl$observer <- 10
# trl_dat <- filter(trl_dat, !(observer == 10 & session == 'a'))
# trl_dat <- filter(trl_dat, !(observer == 11 & session == 'a'))
# trl_dat <- rbind(trl_dat, person10_a_trl, person11_a_trl)

# only take correct trials
trl_dat <- filter(trl_dat, accuracy == 1)
fix_dat <- filter(left_join(fix_dat, trl_dat), accuracy == 1)

# remove person 15 as they never found the hard targets
trl_dat <- filter(trl_dat, observer != 15)
fix_dat <- filter(fix_dat, observer != 15)

# remove people with poor target easy/absent accuracy
fix_dat <- filter(fix_dat, !(observer %in% c(4, 21, 33)))
trl_dat <- filter(trl_dat, !(observer %in% c(4, 21, 33)))

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
  %>% filter(n > 1, n <= 3, side != "central")
  %>% group_by(observer, session, trial, targSide) 
  %>% summarise(
    prop_homo = mean(side == "homo"),
    rt    = unique(rt))
  %>% group_by(observer, session, targSide) 
  %>% summarise(
    prop_homo  = mean(prop_homo),
    median_rt = median(rt),
    meanlogrt = mean(log(rt,2)))
  ) -> dat

dat2 <- filter(dat, targSide == "absent")
dat2$median_rt <- filter(dat, targSide == "hard")$median_rt

r_sess_a = with(filter(dat2, session == "a"), cor.test(prop_homo, median_rt))
r_sess_a = format(round(r_sess_a$estimate, 2), nsmall = 2)
r_sess_b = with(filter(dat2, session == "b"), cor.test(prop_homo, median_rt))
r_sess_b = format(round(r_sess_b$estimate, 2), nsmall = 2)

r_df = tibble(
  text = c(
    paste("italic(r) == ", r_sess_a), 
    paste("italic(r) ==", r_sess_b)),
  x = c(0.90, 0.90), y = c(5, 2), session = c("a", "b"))

plt <- ggplot(dat2, aes(x = prop_homo, y = median_rt/1000, colour = session))
plt <- plt + geom_point() + geom_smooth(method = lm)
plt <- plt + theme_minimal() + scale_colour_ptol()
plt <- plt + scale_x_continuous("prop. homogeneous fixations (absent)", limits = c(0, 1))
plt <- plt + scale_y_continuous("median reaction time for hard trials (secs)", limits = c(0,8))
plt <- plt + theme(
  legend.justification = c(0,1), 
  legend.position = c(0,1),
  legend.background = element_rect(fill="white"))
plt <- plt + geom_text(data=r_df, 
  aes(x = x, y = y, label = text, colour=session), show.legend = FALSE, parse = TRUE)
ggsave("./scratch/start_compare_median_rt.pdf", width = figYn, height = figYn)

# --------------------------------------------------------------------------------
# how consistent is stratety between session a and b?
# --------------------------------------------------------------------------------

agg_dat = (filter(fix_dat, side!="central", n<=6, n>1, targSide=="absent") 
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


# t_rt_dat <- filter(t_rt_dat, !(observer %in% c(10,11)))

cor.test(t_rt_dat$a_strat, t_rt_dat$b_strat)


plt <- ggplot(t_rt_dat, aes(x = a_strat, y = b_strat))
plt <- plt + geom_point()
plt <- plt + geom_abline()
plt <- plt + geom_smooth(method = lm, se = T)
plt <- plt + scale_x_continuous("session a: prop. hetero. fixations", 
  limits = c(0, 1), expand = c(0,0))
plt <- plt + scale_y_continuous("session b: prop. hetero. fixations", 
    limits = c(0, 1), expand = c(0,0))
plt <- plt + theme_minimal() + scale_colour_ptol()
plt <- plt + geom_text(label = "r = 0.72", x = 0.25, y = 0.75)
ggsave("../scratch/strat_corr.pdf", width = figXn, height = figYn)
ggsave("../scratch/strat_corr.png", width = figXn, height = figYn)
# --------------------------------------------------------------------------------
# output data for cross experiment correlations!
# base statistic for correlation on only first 3 fixations
# use session B only
# --------------------------------------------------------------------------------




write_csv(filter(dat2, session == "b"), "scratch/lineseg_output.csv")



