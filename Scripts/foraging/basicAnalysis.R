library(tidyverse)
library(forcats)


dat <- read_delim("Data_MultiTargForag_allsubs.txt", delim="\t")

# loot at median RT
plt <- ggplot(dat, aes(x = Feature_medianRT, y = Conj_medianRT))
plt <- 	plt + geom_point()
plt