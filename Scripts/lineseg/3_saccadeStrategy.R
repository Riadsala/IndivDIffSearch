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

metricVal = data.frame(nfix=as.numeric(), r=as.numeric())
for (nf in 2:12)
{
  d = aggregate(propHetro ~ subj, data=filter(aggData, n<=nf), "mean")
  cordf = merge(d, medianRT)
  r = cor(cordf$propHetro, cordf$RT)
  metricVal = rbind(metricVal, data.frame(nfix=nf, r=r))
}

plt = ggplot(metricVal, aes(x=nfix, y=r^2)) + geom_point()
plt = plt + scale_x_continuous(name = "including first x fixations", breaks=c(2,4,6,8,10))
plt = plt + scale_y_continuous(name = expression(R^2))
plt = plt + theme_bw()
ggsave("r_by_nfix.pdf", width=4, height=4)

nf = 3
d = aggregate(propHetro ~ subj, data=filter(aggData, fixNum<=nf), "mean")
cordf = merge(d, medianRT)
 plt = ggplot(cordf, aes(x=propHetro, y=RT)) + geom_point()
 plt = plt + geom_smooth(method=lm)
 plt = plt + scale_x_continuous("mean prop. of first 3 fixations on hetro. side", limits=c(0,1))
 plt = plt + scale_y_continuous("median reaction time (secs")
 plt = plt + theme_bw()
ggsave("best_r_scatter.pdf", width=4, height=4)


