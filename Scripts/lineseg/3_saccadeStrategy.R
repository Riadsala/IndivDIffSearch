library(tidyverse)
library(ggthemes)
library(forcats)
library(binom)
library(lme4)
library(scales)

figXn <- 6
figYn <- 4
 
fix_dat <- readRDS(file="scratch/processedFixationData.Rda")
trl_dat = readRDS(file="scratch/processedRTandAccData.Rda")

# only take correct trials
trl_dat <- filter(trl_dat, accuracy == 1)
fix_dat <- filter(left_join(fix_dat, trl_dat), accuracy == 1)

# remove person 15 as they never found the hard targets
trl_dat <- filter(trl_dat, observer != 15)
fix_dat <- filter(fix_dat, observer != 15)


# classify every fixation as homo (left), central, or hetro (right)
centralWidth <- 0.05 # used to be 64 pixels! #change to 1 visual degree
fix_dat$side <- 'central'
fix_dat$side[which(fix_dat$x <(0-centralWidth/2))] <- "homo"
fix_dat$side[which(fix_dat$x >(0+centralWidth/2))] <- "hetero"
fix_dat$side <- as_factor(fix_dat$side)

agg_dat = (filter(fix_dat, side!="central", n<12, n>1, targSide=="absent") 
  %>% group_by(observer, session, n) 
    %>% summarise(
    	nTrials=length(trial),
      propHetro=mean(side=="hetero"), 
      lowerS = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
      upperS = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

plt <- ggplot(filter(agg_dat, n<=5), 
  aes(x=n, y=propHetro, ymin=lowerS, ymax=upperS, colour=session))
plt <- plt + geom_point() + geom_path() + geom_errorbar()
plt <- plt + facet_wrap(~observer, nrow=3)
plt <- plt + scale_x_continuous(
  name="fixation number", breaks=c(2,4,6,8,10))
plt <- plt + scale_y_continuous(
  name="proportion of fixations to heterogeneous side", limits=c(0,1))
plt <- plt + theme_bw() + scale_colour_ptol()
ggsave("scratch/strategyBySessionAndPerson.pdf", width=12, height=5)


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
  x = c(0.90, 0.90), y = c(5000, 2000), session = c("a", "b"))

plt <- ggplot(dat2, aes(x = prop_homo, y = median_rt, colour = session))
plt <- plt + geom_point() + geom_smooth(method = lm)
plt <- plt + theme_minimal() + scale_colour_ptol()
plt <- plt + scale_x_continuous("prop. homogeneous fixations (absent)", limits = c(0, 1))
plt <- plt + scale_y_continuous("median reaction time for hard trials (secs)")
plt <- plt + theme(
  legend.justification = c(0,1), 
  legend.position = c(0,1),
  legend.background = element_rect(fill="white"))
plt <- plt + geom_text(data=r_df, 
  aes(x = x, y = y, label = text, colour=session), show.legend = FALSE, parse = TRUE)
ggsave("./scratch/start_compare_median_rt.pdf", width = figYn, height = figYn)


# --------------------------------------------------------------------------------
# output data for cross experiment correlations!
# base statistic for correlation on only first 3 fixations
# use session B only
# --------------------------------------------------------------------------------

write_csv(filter(dat2, targSide=="b"), "scratch/lineseg_output.csv")



