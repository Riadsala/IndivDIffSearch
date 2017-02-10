library(ggplot2)
library(scales)
#setwd("C:/Users/r02al13/Documents/GitHub/HalfScreenPopOut")
rtdat = readRDS(file="../data/processedRTandAccData.Rda")
cbPalette <- c("#E69F00", "#56B4E9","#B5CB8B")

plt = ggplot(rtdat, aes(x=targSide,y=RT)) + geom_boxplot()
plt = plt + coord_trans(y="log2") + theme_bw()
plt = plt + scale_y_continuous(name="reaction time (seconds)", breaks=c(1,2,4,8,16,32), limits=c(1,32))
ggsave("../plots/boxplotRT.pdf")

plt = ggplot(rtdat, aes(x=RT, fill=targSide)) + geom_density(alpha=0.7)
plt = plt + theme_bw() + theme(legend.justification=c(1,1), legend.position=c(1,1))
plt = plt + scale_fill_discrete(name='target position') +  scale_fill_brewer(palette="Set2")
plt = plt + scale_x_continuous(name="reaction time (seconds)")
ggsave("../plots/densityRT.pdf", width=6, height=4)

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




