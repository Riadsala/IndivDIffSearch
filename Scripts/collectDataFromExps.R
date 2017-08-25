library(tidyverse)


lineseg <- read_csv("lineseg/scratch/lineseg_output.csv")
adapt   <- read_delim("adaptchoice/Data_AdaptChoice_allsubs_Summary.txt", delim = "\t")
forage  <- read_delim("foraging/Data_MultiTargForag_allsubs.txt", delim = "\t;")

adapt <- select_(adapt, "SubNo", "Acc", "RT", "ProportionOptimalChoices", "SwitchRate", "Log2RT")
names(adapt) = c("observer", "ac_acc", "ac_rt", "ac_propOpt", "ac_switchRate", "ac_meanlog2rt")

forage <- select_(forage, "SubNo", "Feature_log2RT", "Conj_log2RT", "Conj_RunNum", "Conj_RunLength")
names(forage) = c("observer", "fg_feat_log2", "fg_conj_log2", "fg_conj_run_num", "fg_conj_run_length")

lineseg <- select(lineseg, observer, prop_homo, prop_hetero, median_rt, meanlogrt)
names(lineseg) <- c("observer", "ls_prop_homo", "ls_prop_hetero", "ls_median_rt", "ls_mean_log_rt")

dat = full_join(lineseg, adapt, by = "observer")
dat = full_join(dat, forage)


write_csv(dat, "summaryData.csv")



