library(tidyverse)
library(binom)
library(ggthemes)
# library(lme4)
# library(scales)

fixdat = readRDS(file="data/processedFixData.Rda")
fixdat = (filter(fixdat, subj!=4, subj!=15)) 
#=======

fixdat = readRDS(file="data/processedFixData.Rda")

levels(fixdat$subj) = 1:14
#>>>>>>> Stashed changes
cbPalette <- c("#56B4E9", "#E69F00")

rtdat = readRDS(file="data/processedRTandAccData.Rda")
medianRT = aggregate(RT~subj, rtdat, "median")

# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "homo"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "hetro"
fixdat$side = as.factor(fixdat$side)

aggData = (filter(fixdat, side!="central", fixNum<26, fixNum>1, targSide=="absent") 
  %>% group_by(subj, fixNum) 
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper))
plt = plt + geom_errorbar(colour = "grey") + geom_point() + geom_path()
plt = plt + theme_tufte() + facet_wrap(~subj, nrow=2)
plt = plt + scale_x_continuous(name="fixation number")
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side")
ggsave("over_first_25_fixations.png", width = 10, height = 6)



one_person = filter(aggData, subj == 6)
one_person$fixNum <- one_person$fixNum - 1
plt <- ggplot(one_person, aes(x = fixNum, y = propHetro))
plt <- plt + geom_errorbar(aes( ymin = lower, ymax = upper), colour = "grey") + geom_point() 

m <- glm(propHetro ~ fixNum + I(fixNum^2) + I(fixNum^3) + I(fixNum^4) + 0, family = binomial, data = one_person)
model_dat <- tibble(fixNum = 0:max(one_person$fixNum))
model_dat$p <- predict(m , model_dat, type = "response")

plt <- plt + geom_path(data = model_dat, aes(x =fixNum+1, y = p), colour = "purple")
plt


one_person <- filter(fixdat, subj == 6)
one_person$fixNum <- one_person$fixNum - 1

one_person$half <- as.numeric(one_person$trial < 81)
m <- glm((side == "hetro") ~ half * (fixNum + I(fixNum^2) + I(fixNum^3) + I(fixNum^4)) + 0, family = binomial, data = one_person)

model_dat <- tibble(
	fixNum = rep(0:max(one_person$fixNum), 2),
	half = rep(c(0,1), each = length(0:max(one_person$fixNum))))
model_dat$p <- predict(m , model_dat, type = "response")

plt <- ggplot(model_dat, aes(x = fixNum+1, y = p, colour = half))
plt <- plt + geom_path()
plt


