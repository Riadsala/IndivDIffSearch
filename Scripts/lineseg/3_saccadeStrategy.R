library(dplyr)
library(ggplot2)
library(binom)
library(lme4)
library(scales)

cbPalette <- c("#56B4E9", "#E69F00")
 
fixDat = readRDS(file="scratch/processedFixationData.Rda")
trlDat = readRDS(file="scratch/processedRTandAccData.Rda")


# medianRT = aggregate(RT~subj, rtdat, "median")

# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 0.05 # used to be 64 pixels! #change to 1 visual degree
fixDat$side = 'central'
fixDat$side[which(fixDat$x <(0-centralWidth/2))] = "homo"
fixDat$side[which(fixDat$x >(0+centralWidth/2))] = "hetro"
fixDat$side = as.factor(fixDat$side)

aggData = (filter(fixDat, side!="central", n<12, n>1, targSide=="absent") 
  %>% group_by(observer, session, n) 
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

plt = ggplot(aggData, aes(x=n, y=propHetro, ymin=lower, ymax=upper, colour=session))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~observer, nrow=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,4,6,8,10))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side", limits=c(0,1))
ggsave("scratch/strategyBySessionAndPerson.pdf")



