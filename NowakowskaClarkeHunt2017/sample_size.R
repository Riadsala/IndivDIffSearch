library(tidyverse)
library(binom)
library(ggthemes)
# library(lme4)
# library(scales)

fixdat = readRDS(file="data/processedFixData.Rda")
fixdat = (filter(fixdat, subj!=4, subj!=15)) 
#=======

fixdat = as.tibble(readRDS(file="data/processedFixData.Rda"))

levels(fixdat$subj) = 1:14
#>>>>>>> Stashed changes
cbPalette <- c("#56B4E9", "#E69F00")

rtdat = as.tibble(readRDS(file="data/processedRTandAccData.Rda"))
medianRT = aggregate(RT~subj, rtdat, "median")

# classify every fixation as homo (left), central, or hetro (right)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "homo"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "hetro"
fixdat$side = as.factor(fixdat$side)

all_scores = tibble()

for (n in 10:80) {

	# sample a subset of trials
	ss_dat <- tibble()
	for (pp in levels(rtdat$subj)) {
		ta_dat <- filter(rtdat, targSide == "absent", subj == pp)
		ss_trl <- sample_n(ta_dat, n, replace = FALSE)
		ss_dat <- bind_rows(ss_dat, filter(fixdat, subj == pp, trial %in% ss_trl$trial))

	}

	aggData = (filter(ss_dat, side!="central", fixNum<6, fixNum>1, targSide=="absent") 
	  %>% group_by(subj, fixNum) 
	    %>% summarise(
	     propHetro=mean(side=="hetro"), 
	     nTrials=length(trial)))

	scores <- aggregate(data = aggData, propHetro ~ subj, mean)
	scores$n <- n
	all_scores <- bind_rows(all_scores, scores)

}

# all_scores_ref <- filter(all_scores, n == 80)
# all_scores$error <- abs(all_scores$propHetro - rep(all_scores_ref$propHetro, 71))
# all_scores <- filter(all_scores, n < 76)
# mean_error <- aggregate(data = all_scores, error ~ n, mean)

plt <- ggplot(all_scores, aes(x = n, y = propHetro, group = subj))
plt <- plt + geom_path(colour = "darkgrey")
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("number of sampled trials", breaks = seq(10, 80, 10))
plt <- plt + scale_y_continuous("prop. hetero. fixations", labels = scales::percent)
plt
ggsave('using_smaller_sample_sizes.pdf', width = 4, height = 4)
