library(dplyr)
library(ggplot2)
library(binom)
library(lme4)
library(scales)

# # setwd("~/Desktop/HalfPopOutAnalysisToday")
# setwd("~/Documents/HalfPopOutAnalysisToday")
#<<<<<<< Updated upstream
 # setwd("C:/Users/r02al13/Documents/GitHub/HalfScreenPopOut")
fixdat = readRDS(file="../data/processedFixData.Rda")
fixdat = (filter(fixdat, subj!=4, subj!=15)) 
#=======

fixdat = readRDS(file="../data/processedFixData.Rda")

levels(fixdat$subj) = 1:12
#>>>>>>> Stashed changes
cbPalette <- c("#56B4E9", "#E69F00")

rtdat = readRDS(file="../data/processedRTandAccData.Rda")
medianRT = aggregate(RT~subj, rtdat, "median")

# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "homo"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "hetro"
fixdat$side = as.factor(fixdat$side)

aggData = (filter(fixdat, side!="central", fixNum<12, fixNum>1, targSide=="absent") 
  %>% group_by(subj, fixNum) 
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~subj, nrow=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,4,6,8,10))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side")
# ggsave("../plots/FixXsideByFixNumAndSubjExCentral.pdf", width=9, height=4)
# ggsave("../plots/FixXsideByFixNumAndSubjExCentral.jpg",dpi=600, width=9, height=5)

metricVal = data.frame(nfix=as.numeric(), r=as.numeric())
for (nf in 2:12)
{
  d = aggregate(propHetro ~ subj, data=filter(aggData, fixNum<=nf), "mean")
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


