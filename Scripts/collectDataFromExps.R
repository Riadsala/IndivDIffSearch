library(ggplot2)
library(dplyr)


lineseg = read.csv("lineseg/scratch/lineseg_output.csv")
adapt   = read.csv("adaptchoice/Data_AdaptChoice_allsubs_Summary.txt", sep="\t")
forage  = read.csv("foraging/Data_MultiTargForag_allsubs.txt", sep="\t")

names(adapt) = c("observer", "ac_acc", "ac_rt", "ac_propOpt", "ac_switchRate", "ac_meanlogrt", "ac_medianrt")
names(forage) = c("observer", "fg_meanrt_feature", "fg_medianrt_feature", "fg_meanlogrt_feature", "fg_runnum_feature", "fg_runlength_feature","fg_meanrt_cong", "fg_medianrt_cong", "fg_meanlogrt_cong", "fg_runnum_cong", "fg_runlength_cong")

dat = merge(lineseg, adapt)
dat = merge(dat, forage)


rtdat = select(dat, ls_meanlogrt_hard, ac_meanlogrt, fg_meanlogrt_feature, fg_meanlogrt_cong)

cor(rtdat)


str(cor(dat[,-1]))



plot(dat$lineseg_meanlogrt_hard, dat$ac_rt)

cor.test(dat$lineseg_meanlogrt_hard, dat$ac_rt)


write.csv(dat, "summaryData.csv")

plt = ggplot(dat, aes(x=ls_meanlogrt_hard, y=ac_rt)) 
plt = plt + geom_point()
plt = plt + geom_smooth(method=lm)
plt
ggsave("ls_ac.pdf")