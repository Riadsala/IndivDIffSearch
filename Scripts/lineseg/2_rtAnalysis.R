library(ggplot2)
library(scales)
library(dplyr)
library(lme4)
library(tidyr)


# read in processed data
trlDat = readRDS(file="scratch/processedRTandAccData.Rda")

#  set colour palette
cbPalette <- c("#E69F00", "#56B4E9","#B5CB8B")

plt = ggplot(trlDat, aes(x=log(rt), fill=targSide)) + geom_density(alpha=0.7)
plt = plt + theme_bw() + theme(legend.justification=c(1,1), legend.position=c(1,1))
plt = plt + scale_fill_brewer(name='target position', palette="Set2")
plt = plt + scale_x_continuous(name="reaction time (seconds)")
ggsave("scratch/densityRT.pdf", width=6, height=4)

# how does accuracy change from session to session
accDat  = aggregate(data=trlDat, acc ~ observer + session + targSide, FUN="mean")
plt = ggplot(accDat, aes(x=as.numeric(session), y=acc, colour=targSide, group=observer:targSide))
plt = plt + geom_point() + geom_smooth(method=lm)
plt

# remove incorrect trials
rtDat = (filter(trlDat, acc==1) 
	%>% group_by(observer, session, targSide)
	%>% summarise(
		logrt = mean(log(rt))))
		# n = length(rt),
		# sderr = sd(log(rt)/sqrt(n)),
		# upper = logrt + 1.96*sderr,
		# lower = logrt - 1.96*sderr))

rtDat = spread(rtDat,  session, logrt)
names(rtDat)[3:4] = c("session1", "session2")

plt = ggplot(rtDat, aes(x=session1, y=session2, colour=targSide))
plt = plt + geom_point()
plt = plt + geom_smooth(method="lm")
plt







# # now lets look at RTs... 
# # first we need to filter out incorrect trials
# rtdat = rtdat[which(rtdat$acc==1),]
# library(scales)     
# # rt for target present and absent
# rtdat  = aggregate(data=rtdat, RT ~ subj +targSide, FUN="median")

# ggsave("plots/RT.jpg",dpi=600, width=6, height=3)




