library(tidyverse)
library(forcats)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# Read in and process asc file for lineseg experiment

get_detail <- function(str, detail) {
	line = str[[grep(detail, str)]]
	out = strsplit(line[[2]], " ")
	out = unlist(out)[2]
	return(out)
}

process_acs <- function(asc, ss) {
	# FACES_DRAWN marks start of trial. 
	# It is called this as matlab code was reused from Anna's old experiment
	trialStarts <- grep("FACES_DRAWN", asc)
	trialEnds   <- grep("TRIAL_OVER", asc)
	trialDone   <- grep("Done", asc)
	nTrials <- length(trialStarts)

	while (length(trialEnds)!=nTrials) 	{
		# we probably have an early response or something
		trialEnds <- trialEnds[-min(which(trialEnds[1:nTrials] < trialStarts))]
		trialDone <- trialDone[-min(which(trialDone[1:nTrials] < trialStarts))]
	}

	fixDat <- tibble(
		observer = numeric(), 
		session = character(), 
		trial = numeric(), 
		targSide = character(), 
		easySide = character(), 
		n = numeric(), 
		x = numeric(), 
		y = numeric(), 
		dur = numeric())

	trlDat <- tibble(
		observer = numeric(), 
		session = character(), 
		trial = numeric(), 
		targPres = character(), 
		targSide = character(), 
		row = character(), 
		column = character(), 
		easySide = character(),
		responseKey = character(), 
		rt = numeric())

	for (t in 1:nTrials) 	{
		# first get fixations
		trial <- asc[trialStarts[t]:trialEnds[t]]		
		fixationLines <- grep("EFIX", trial)

		# get details of trial
		details  <- asc[trialEnds[t]:trialDone[t]]		
		trial_id <- get_detail(details, "TRIALNO")
		trial_id <- as.numeric(unlist(strsplit(trial_id, "_"))[2])
		targPres <- get_detail(details, "TargPres")
		targSide <- get_detail(details, "TargSide")
		row      <- get_detail(details, "Row")
		column   <- get_detail(details, "Column")
		easySide <- get_detail(details, "Easy_Side")
		response <- get_detail(details, "Key")

		startTime <- unlist(strsplit(unlist(asc[trialStarts[t]])[2], " "))[1]
	
		if (length(fixationLines)>0) {
			fixations <- as.tibble(matrix(unlist(trial[fixationLines]), byrow=T, ncol=6))

			fixThisTrial = tibble(
				observer=pp, 
				session=ss,
				trial=trial_id,
				targSide = targSide,
				easySide = easySide, 				
				x   = as.numeric(fixations$V4), 
				y   = as.numeric(fixations$V5), 
				dur = as.numeric(fixations$V3))

			# convert to stimulus coordinates
			 		
			 fixThisTrial$n <- 1:length(fixThisTrial$x)
			 fixDat <- bind_rows(fixDat, fixThisTrial)
			 rm(fixThisTrial)
		}

		endTime = unlist(strsplit(unlist(asc[trialEnds[t]])[2], " "))[1]
		
		detTrial = data.frame(
			observer=pp, 
			session=ss,
			trial=trial_id,
			targPres=targPres,
			targSide=targSide, 
			row=row, 
			column=column, 
			easySide=easySide, 
			responseKey=response,
			rt = (as.numeric(endTime)-as.numeric(startTime)))		

		trlDat = bind_rows(trlDat, detTrial)
		rm(details, detTrial)

	}

	return(list(fixDat,trlDat))
}

participants <- 1:31
participants <- participants[-18]
datFolder <- '../../Data/'

fixDat <- tibble(observer=numeric(), session=character(), trial=numeric(), n=numeric(), x=numeric(), y=numeric(), dur=numeric())
trlDat <- tibble(observer=numeric(), session=character(), trial=numeric(), targPres=character(), targSide=character(), row=character(), column=character(), easySide=character(), responseKey=character())
resDat <- tibble(observer=numeric(), session=character(), xRes=numeric(), yRes=numeric())	

for (pp in participants) {
	# print(pp)
	for (ss in c('a', 'b')) 	{
		# print(ss)
		filename = paste(datFolder, pp, '/lineseg/Ul', pp, ss, '.asc', sep="")
		asc = strsplit(readLines(filename), "\t")
		dat = process_acs(asc, ss)

		# combine data.
		fixDat = bind_rows(fixDat, dat[[1]])
		trlDat = bind_rows(trlDat, dat[[2]])


		# get screen res for this file
		resMSG = asc[grep("GAZE_COORDS", asc )[1]]
		resolution=as.numeric(unlist(strsplit(unlist(resMSG)[2], " "))[3:6])
		resDat = bind_rows(resDat, tibble(observer=pp, session=ss, xRes=resolution[3]+1, yRes=resolution[4]+1))

		rm(dat)
	}
}

rm(ss, pp, filename)
 
##### Tidy up trlDat

# convert some things to factors
trlDat$observer    <- as.factor(trlDat$observer) # how do I do this with forcats? 
trlDat$session     <- as_factor(trlDat$session)
trlDat$responseKey <- as_factor(trlDat$responseKey)
trlDat$targPres    <- as_factor(trlDat$targPres)
trlDat$targSide    <- as_factor(trlDat$targSide)
trlDat$easySide    <- as_factor(trlDat$easySide)

fixDat$observer    <- as.factor(fixDat$observer) # how do I do this with forcats? 
fixDat$session     <- as_factor(fixDat$session)
fixDat$targSide    <- as_factor(fixDat$targSide)
fixDat$easySide    <- as_factor(fixDat$easySide)

# recode factors
levels(trlDat$targPres) <- c("present", "absent")
levels(trlDat$targSide) <- c("right", "absent", "left")
levels(trlDat$easySide) <- c("left", "right")
levels(fixDat$targSide) <- c("right", "absent", "left")
levels(fixDat$easySide) <- c("left", "right")


# recode targSide to be easy or hard!
relSide = as.character(trlDat$targSide)
relSide[trlDat$targSide=="left" & trlDat$easySide=="left"] <- "easy"
relSide[trlDat$targSide=="right" & trlDat$easySide=="right"] <- "easy"
relSide[trlDat$targSide=="right" & trlDat$easySide=="left"] <- "hard"
relSide[trlDat$targSide=="left" & trlDat$easySide=="right"] <- "hard"
relSide[trlDat$targSide=="absent"] <- "absent"
trlDat$targSide    <- as_factor(relSide)

relSide <- as.character(fixDat$targSide)
relSide[fixDat$targSide=="left" & fixDat$easySide=="left"] <- "easy"
relSide[fixDat$targSide=="right" & fixDat$easySide=="right"] <- "easy"
relSide[fixDat$targSide=="right" & fixDat$easySide=="left"] <- "hard"
relSide[fixDat$targSide=="left" & fixDat$easySide=="right"] <- "hard"
relSide[fixDat$targSide=="absent"] <- "absent"
fixDat$targSide    <- as_factor(relSide)
rm(relSide)


trlDat$targSide <- fct_relevel(trlDat$targSide, "easy", "hard", "absent")
fixDat$targSide <- fct_relevel(fixDat$targSide, "easy", "hard", "absent")
	
# code up correct or not
trlDat$accuracy <- 0
trlDat$accuracy[(trlDat$responseKey=="Key_l") & (trlDat$targPres=="present")] <- 1
trlDat$accuracy[(trlDat$responseKey=="Key_r") & (trlDat$targPres=="absent")] <- 1



##### Tidy up fixDat

# convert some things to factors
fixDat$observer = as.factor(fixDat$observer)
fixDat$session = as_factor(fixDat$session)

# centre x and y and normalise by resolution!
for (pp in participants)
{
	for (ss in c('a', 'b'))
	{
		resX <- filter(resDat, observer==pp, session==ss)$xRes
		resY <- filter(resDat, observer==pp, session==ss)$yRes
		idx <- which(fixDat$observer==pp & fixDat$session==ss)
		fixDat$x[idx] <- (fixDat$x[idx] - resX/2)/(resX/2)
		fixDat$y[idx] <- (fixDat$y[idx] - resY/2)/(resY/2)

	}
}

# flip fixations relative to easySide
fixDat$x[which(fixDat$easySide=="right")] <- -fixDat$x[which(fixDat$easySide=="right")]  

# sanity plot to check fixations are mostly within [-1, 1] x [-1, 1] 
# plt <- ggplot(fixDat, aes(x=x, y=y))
# plt <- plt + geom_point()
# plt <- plt + facet_wrap(~observer)
# plt

# only take fixations that fall within screen
fixDat$x <- with(fixDat, ifelse(x > -1 & x <1, x, NaN))
fixDat$y <- with(fixDat, ifelse(y > -1 & y <1, y, NaN))


# remove trials which are invalid for one reason or another
trlDat <- filter(trlDat, responseKey != "Key_x")
# remove trial with incredible short rt
trlDat <- filter(trlDat, rt > 100)

# now remove excluded trials from fixDat
incTrials <- with(trlDat, paste(observer, session, trial))
fixDat <- filter(fixDat, paste(observer, session, trial) %in% incTrials)
rm(incTrials)


# save
saveRDS(trlDat, "scratch/processedRTandAccData.Rda")
saveRDS(fixDat, "scratch/processedFixationData.Rda")