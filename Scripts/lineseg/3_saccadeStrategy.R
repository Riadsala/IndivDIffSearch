library(dplyr)
library(ggplot2)
library(binom)
library(lme4)
library(scales)

cbPalette <- c("#56B4E9", "#E69F00")
 
fixDat = readRDS(file="scratch/processedFixationData.Rda")
trlDat = readRDS(file="scratch/processedRTandAccData.Rda")



# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 0.05 # used to be 64 pixels! #change to 1 visual degree
fixDat$side = 'central'
fixDat$side[which(fixDat$x <(0-centralWidth/2))] = "homo"
fixDat$side[which(fixDat$x >(0+centralWidth/2))] = "hetro"
fixDat$side = as.factor(fixDat$side)


# remove incorrect trials
fixDat$acc = 1
incorrectTrls = filter(trlDat, acc!=1)
for (ii in 1:nrow(incorrectTrls))
{
	idx = which(fixDat$observer==incorrectTrls$observer[ii] & fixDat$session==incorrectTrls$session[ii] & fixDat$trial==incorrectTrls$trial[ii])
	fixDat$acc[idx] = 0
}


trlDat = filter(trlDat, acc==1)
fixDat = filter(fixDat, acc==1)

aggData = (filter(fixDat, side!="central", n<12, n>1, targSide=="absent") 
  %>% group_by(observer, session, n) 
    %>% summarise(
    	nTrials=length(trial),
     propHetro=mean(side=="hetro"), 
     lowerS = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upperS = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))


plt = ggplot(aggData, aes(x=n, y=propHetro, ymin=lowerS, ymax=upperS, colour=session))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~observer, nrow=3)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,4,6,8,10))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side", limits=c(0,1))
ggsave("scratch/strategyBySessionAndPerson.pdf", width=12, height=5)



###########
# output data for cross experiment correlations!

# base statistic for correlation on only first 3 fixations
aggDataSS = (filter(fixDat, side!="central", n>1, n<=5, targSide=="absent") 
  %>% group_by(observer) 
    %>% summarise(
     propHetro=mean(side=="hetro")))



# base statistic for correlation on only first 3 fixations
aggDataRT = (filter(trlDat, targSide=="hard") 
  %>% group_by(observer) 
    %>% summarise(
     medianRT = median(rt),
     meanLogRT = mean(log(rt))))



aggData = merge(aggDataSS, aggDataRT)


plt = ggplot(aggData, aes(x=propHetro, y=medianRT, colour=observer))
plt = plt + geom_point() 
ggsave("scratch/stratVrt.pdf")

names(aggData) = c("observer", "ls_propHetro_absent", "ls_medianRT_hard", "ls_meanlogrt_hard")

write.csv(aggData, "scratch/lineseg_output.csv")



