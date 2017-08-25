library(tidyverse)
library(forcats)


# dat <- read_delim("Data_MultiTargForag_allsubs.txt", delim="\t")

# # loot at median RT
# plt <- ggplot(dat, aes(x = Feature_medianRT, y = Conj_medianRT))
# plt <- 	plt + geom_point()
# plt


score <- read_csv("FG_DataforHist_FeatTrials.csv", col_names = FALSE)$X1
feats <- tibble(score = score, cond = "feature")


score <- read_csv("FG_DataforHist_ConjTrials.csv", col_names = FALSE)$X1
conjs <- tibble(score = score, cond = "conjunction")

d <- bind_rows(feats, conjs)

p <- ggplot(d, aes(x = score))
p <- p + geom_histogram(binwidth = 1)
p <- p + facet_grid(. ~ cond)
p <- p + theme_bw()
p <- p + scale_x_continuous("number of runs per trial", expand = c(0,0))
p <- p + scale_y_continuous("total number of trials", expand = c(0,0))
ggsave("run_length_hist.png", width = 6, height = 4)
