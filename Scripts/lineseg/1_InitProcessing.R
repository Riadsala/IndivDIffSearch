
library(dplyr)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


# Read in and process asc file for lineseg experiment


GetDetail <- function(str, detail)
{
	line = str[[grep(detail, str)]]
	out = strsplit(line[[2]], " ")
	out = unlist(out)[2]
	return(out)

}

ProcessASC <- function(asc, ss)
{
	# FACES_DRAWN marks start of trial. 
	# It is called this as matlab code was reused from Anna's old experiment
	trialStarts = grep("FACES_DRAWN", asc)
	trialEnds   = grep("TRIAL_OVER", asc)
	trialDone   = grep("Done", asc)
	nTrials = length(trialStarts)

	if (length(trialEnds)!=nTrials)
	{
		# we probably have an early response or something
		trialEnds=trialEnds[-min(which(trialEnds[1:nTrials]<trialStarts))]
		trialDone=trialDone[-min(which(trialDone[1:nTrials]<trialStarts))]
	}

	fixDat = data.frame(observer=numeric(), session=numeric(), trial=numeric(), targSide=character(), esaySide=character(), n=numeric(), x=numeric(), y=numeric(), dur=numeric())
	detDat = data.frame(observer=numeric(), session=numeric(), trial=numeric(), targPres=character(), targSide=character(), row=character(), column=character(), easySide=character(), responseKey=character(), rt=numeric())

	for (t in 1:nTrials)
	{
		# first get fixations
		trial   = asc[trialStarts[t]:trialEnds[t]]		
		fixationLines = grep("EFIX", trial)


		# get details of trial
		details   = asc[trialEnds[t]:trialDone[t]]		
		trial_id = GetDetail(details, "TRIALNO")
		trial_id = as.numeric(unlist(strsplit(trial_id, "_"))[2])
		targPres = GetDetail(details, "TargPres")
		targSide = GetDetail(details, "TargSide")
		row = GetDetail(details, "Row")
		column = GetDetail(details, "Column")
		easySide = GetDetail(details, "Easy_Side")
		response = 	GetDetail(details, "Key")

		startTime = unlist(strsplit(unlist(asc[trialStarts[t]])[2], " "))[1]
	
		if (length(fixationLines)>0)
		{
			fixations = as.data.frame(matrix(unlist(trial[fixationLines]), byrow=T, ncol=6))

			trialDat = data.frame(
				observer=pp, 
				session=ss,
				trial=trial_id,
				targSide = targSide,
				easySide = easySide, 				
				x=as.numeric.factor(fixations$V4), y=as.numeric.factor(fixations$V5), dur=as.numeric.factor(fixations$V3))

			# convert to stimulus coordinates
			 		
			 trialDat$n = 1:length(trialDat$x)
			 fixDat = rbind(fixDat, trialDat)
			 rm(trialDat)
		}

		endTime = unlist(strsplit(unlist(asc[trialEnds[t]])[2], " "))[1]
		

		trialDat = data.frame(
			observer=pp, 
			session=ss,
			trial=trial_id,
			targPres=targPres, 
			targSide=targSide, 
			row=row, 
			column=column, 
			easySide, 
			responseKey=response,
			rt = (as.numeric(endTime)-as.numeric(startTime)))		

		detDat = rbind(detDat, trialDat)
		rm(trialDat, details)

	}

	return(list(fixDat, detDat))
}

participants = c(1,2,3,4,7)
datFolder = '../../Data/'

fixDat = data.frame(observer=numeric(), trial=numeric(), n=numeric(), x=numeric(), y=numeric(), dur=numeric())
trlDat = data.frame(observer=numeric(), trial=numeric(), targPres=character(), targSide=character(), row=character(), column=character(), easySide=character(), responseKey=character())

resDat = data.frame(observer=numeric(), session=numeric(), xRes=numeric(), yRes=numeric())	
for (pp in participants)
{
	for (ss in 1:2)
	{
		filename = paste(datFolder, pp, '/lineseg/Ul', pp, ss, '.asc', sep="")
		asc = strsplit(readLines(filename), "\t")
		dat = ProcessASC(asc, ss)

		fixDat = rbind(fixDat, dat[[1]])
		trlDat = rbind(trlDat, dat[[2]])

		# get screen res for this file
		resMSG = asc[grep("GAZE_COORDS", asc )[1]]
		resolution=as.numeric(unlist(strsplit(unlist(resMSG)[2], " "))[3:6])
		resDat = rbind(resDat, data.frame(observer=pp, session=ss, xRes=resolution[3], yRes=resolution[4]))
	}
}
rm(ss, pp, filename, dat)

##### Tidy up trlDat

# recode factors
levels(trlDat$targPres) = c("present", "absent")
levels(trlDat$targSide) = c("right", "absent", "left")
levels(trlDat$easySide) = c("left", "right")
levels(fixDat$targSide) = c("right", "absent", "left")
levels(fixDat$easySide) = c("left", "right")
# convert some things to factors
trlDat$observer = as.factor(trlDat$observer)
trlDat$session = as.factor(trlDat$session)

# recode targSide to be easy or hard!
relSide =trlDat$TargSide
relSide[trlDat$targSide=="left" & trlDat$easySide=="left"] = "easy"
relSide[trlDat$targSide=="right" & trlDat$easySide=="right"] = "easy"
relSide[trlDat$targSide=="right" & trlDat$easySide=="left"] = "hard"
relSide[trlDat$targSide=="left" & trlDat$easySide=="right"] = "hard"
relSide[trlDat$targSide=="absent"] = "absent"
trlDat$targSide = as.factor(relSide)

relSide =fixDat$TargSide
relSide[fixDat$targSide=="left" & fixDat$easySide=="left"] = "easy"
relSide[fixDat$targSide=="right" & fixDat$easySide=="right"] = "easy"
relSide[fixDat$targSide=="right" & fixDat$easySide=="left"] = "hard"
relSide[fixDat$targSide=="left" & fixDat$easySide=="right"] = "hard"
relSide[fixDat$targSide=="absent"] = "absent"
fixDat$targSide = as.factor(relSide)

rm(relSide)

# code up correct or not
trlDat$acc = 0
trlDat$acc[(trlDat$responseKey=="Key_l") & (trlDat$targPres=="present")] = 1
trlDat$acc[(trlDat$responseKey=="Key_r") & (trlDat$targPres=="absent")] = 1



##### Tidy up fixDat

# convert some things to factors
fixDat$observer = as.factor(fixDat$observer)
fixDat$session = as.factor(fixDat$session)

# centre x and y 
for (pp in participants)
{
	for (ss in 1:2)
	{
		resX = filter(resDat, observer==pp, session==ss)$xRes
		resY = filter(resDat, observer==pp, session==ss)$yRes
		idx = which(fixDat$observer==pp & fixDat$session==ss)
		fixDat$x[idx] = (fixDat$x[idx] - resX/2)/(resX/2)
		fixDat$y[idx] = (fixDat$y[idx] - resY/2)/(resY/2)

	}
}


# only take fixations that fall within screen
fixDat = filter(fixDat, x>-1, x<1, y>-1, y<1)

# flip fixations relative to easySide
fixDat$x[which(fixDat$easySide=="right")] = -fixDat$x[which(fixDat$easySide=="right")]  


saveRDS(fixDat, "scratch/processedFixationData.Rda")

# remove trials which are invalid for one reason or another
trlDat = filter(trlDat, responseKey!="Key_x")

# save
saveRDS(trlDat, "scratch/processedRTandAccData.Rda")
