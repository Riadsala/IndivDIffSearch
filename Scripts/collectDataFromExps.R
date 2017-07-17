library(tidyverse)


lineseg <- read_csv("lineseg/scratch/lineseg_output.csv")
adapt   <- read_delim("adaptchoice/Data_AdaptChoice_allsubs_Summary.txt", delim = "\t")
forage  <- read_delim("foraging/Data_MultiTargForag_allsubs.txt", delim = "\t;")

names(adapt) = c("observer", "ac_acc", "ac_rt", "ac_propOpt", "ac_switchRate", "ac_meanlogrt", "ac_medianrt")
names(forage) = c("observer", "fg_meanrt_feature", "fg_medianrt_feature", "fg_meanlogrt_feature", "fg_runnum_feature", "fg_runlength_feature","fg_meanrt_cong", "fg_medianrt_cong", "fg_meanlogrt_cong", "fg_runnum_cong", "fg_runlength_cong")
lineseg <- select(lineseg, observer, prop_homo, median_rt, meanlogrt)
names(lineseg) <- c("observer", "ls_prop_homo", "ls_median_rt", "ls_mean_log_rt")

dat = full_join(lineseg, adapt)
dat = full_join(dat, forage)


write_csv(dat, "summaryData.csv")



