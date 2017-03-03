library(ggplot2)
library(dplyr)


lineseg = read.csv("lineseg/scratch/lineseg_output.csv")
adapt   = read.csv("adaptchoice/Data_AdaptChoice_allsubs_Summary.txt", sep="\t")

names(adapt) = c("observer", "ac_acc", "ac_rt", "ac_propOpt", "ac_switchRate")


dat = merge(lineseg, adapt)



plot(dat$lineseg_meanlogrt_hard, dat$ac_rt)

cor.test(dat$lineseg_meanlogrt_hard, dat$ac_rt)


write.csv(dat, "summaryData.csv")

plt = ggplot(dat, aes(x=ls_meanlogrt_hard, y=ac_rt)) 
plt = plt + geom_point()
plt = plt + geom_smooth(method=lm)
plt
ggsave("ls_ac.pdf")