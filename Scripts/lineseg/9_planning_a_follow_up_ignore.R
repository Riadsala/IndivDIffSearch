library(tidyverse)

 
fix_dat <- readRDS(file="scratch/processedFixationData.Rda")
trl_dat <- readRDS(file="scratch/processedRTandAccData.Rda")


# remove people with poor target easy/absent accuracy
fix_dat <- filter(fix_dat, !(observer %in% c(4, 21, 33, 56, 58)))
trl_dat <- filter(trl_dat, !(observer %in% c(4, 21, 33, 56, 58)))

# remove fixations falling outside of simulus
fix_dat <- filter(fix_dat, is.finite(x), is.finite(y))

# classify every fixation as homo (left), central, or hetro (right)
centralWidth <- 0.1 # used to be 64 pixels! #change to 1 visual degree
fix_dat$side <- 'central'
fix_dat$side[which(fix_dat$x <(0-centralWidth/2))] <- "homo"
fix_dat$side[which(fix_dat$x >(0+centralWidth/2))] <- "hetero"
