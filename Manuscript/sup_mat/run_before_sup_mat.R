#### To run before sup_mat ####
# this will read in all the data and produce a file of the data to be used in 
# the sup_mat for plotting etc...

#### packages ####
library(tidyverse)

#### ACS ####
#### ACS: read in data ####
# set participants to look for
participants <- 1:75
participants <- participants[!(participants %in% c(18, 32, 48, 57, 11, 31, 69))] 

# setup empty frame 
dat_ACS <- data.frame(SubNo = numeric(),
                      Trial = numeric(),
                      Block_trial = numeric(),
                      Block_num = numeric(),
                      Plateau_cond = numeric(),
                      Transition_cond = numeric(),
                      Run_number = numeric(),
                      Targ1_position = numeric(),
                      Targ2_position = numeric(),
                      FixationCheck = numeric(),
                      Red_targ_digit = numeric(),
                      Blue_targ_digit = numeric(),
                      Response = numeric(),
                      Targ_Choice = numeric(),
                      Acc = numeric(),
                      RT = numeric(),
                      Optimal_Choice = numeric(),
                      StarEndOptimal = numeric(),
                      Repeat_Switch = numeric(),
                      session = numeric())

# loop to get data 
for(pp in participants){
  # get file name
  temp <- dir(paste("../../Data/", pp, "/adaptchoice/", sep = ""))
  
  # setup temp frame
  d <- read.csv(paste("../../Data/", pp, "/adaptchoice/",
                      temp[1],
                      sep = ""),
                skip = 3, header = T, sep = "\t")
  
  # get session info
  session <- strsplit(temp[1], '[_]')[[1]]
  session <- substring(session[4],5,5)
  
  # add in session info
  d$session <- session
  
  # add to dataset 
  dat_ACS <- rbind(dat_ACS, d)
}

# tidy 
rm(d, pp, session, temp)

# sort participant labels 
dat_ACS$SubNo[dat_ACS$SubNo == "56b"] <- "56"
dat_ACS$SubNo <- as.numeric(dat_ACS$SubNo)

#### Foraging ####
#### Foraging: read in data ####
# setup empty frame 
dat_for <- data.frame(Trial = numeric(),
                      Click_Num = numeric(),
                      Target_Click_Order = numeric(),
                      Click_X = numeric(),
                      Click_y = numeric(),
                      Item_selected = numeric(),
                      Item_X = numeric(),
                      Item_Y = numeric(),
                      Target_Set = numeric(),
                      RT = numeric(),
                      sub_no = numeric(),
                      search_type = numeric(),
                      Targets = numeric())

# loop through to get all data 
for(pp in participants){ 
  for(idx in 1:2){
    # get file name
    temp <- dir(paste("../../Data/", pp, "/foraging/", sep = ""))
    
    # skip if string too short (i.e. data file isn't there)
    if(nchar(temp[idx])<25) next
    
    # search type info
    search_info <- read.csv(paste("../../Data/", pp, "/foraging/", temp[idx], sep = ""),
                            nrows = 2, header = F, sep = "\t")
    
    # store this info
    participant <- pp
    search_type <- substring(search_info[2,1],15,15)
    Targets <- substring(search_info[2,2],14,14)
    
    # read in file 
    d <- read.csv(paste("../../Data/", pp, "/foraging/", temp[idx], sep = ""),
                  skip = 2, header = T, sep = "\t")
    
    # add in new info
    d$sub_no <- pp
    d$search_type <- search_type
    d$Targets <- Targets
    
    # bind 
    dat_for <- rbind(dat_for, d)
  }
}

# tidy
rm(d, search_info, idx, participants, pp, participant, search_type, Targets, temp)


# save these files 
save(dat_ACS, file = "scratch/dat_ACS")
save(dat_for, file = "scratch/dat_for")
