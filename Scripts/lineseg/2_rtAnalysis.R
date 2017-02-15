library(ggplot2)
library(scales)
library(dplyr)

# read in processed data
rtdat = readRDS(file="scratch/processedRTandAccData.Rda")

#  set colour palette
cbPalette <- c("#E69F00", "#56B4E9","#B5CB8B")

plt = ggplot(trlDat, aes(x=rt, fill=targSide)) + geom_density(alpha=0.7)
plt = plt + theme_bw() + theme(legend.justification=c(1,1), legend.position=c(1,1))
plt = plt + scale_fill_discrete(name='target position') +  scale_fill_brewer(palette="Set2")
plt = plt + scale_x_continuous(name="reaction time (seconds)")
ggsave("scratch/densityRT.pdf", width=6, height=4)

#
 library(lme4)

# # let us first look at accuracy for target present and absent
accdat  = aggregate(data=rtdat, acc ~ subj + targSide, FUN="mean")

aggregate(data=accdat, acc~targSide, FUN="mean")
write.csv(accdat, "../data/accDat12pps.txt", row.names=F)

# # now lets look at RTs... 
# # first we need to filter out incorrect trials
# rtdat = rtdat[which(rtdat$acc==1),]
# library(scales)     
# # rt for target present and absent
# rtdat  = aggregate(data=rtdat, RT ~ subj +targSide, FUN="median")

# ggsave("plots/RT.jpg",dpi=600, width=6, height=3)




