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

	fixDat = data.frame(observer=numeric(), session=numeric(), trial=numeric(), n=numeric(), x=numeric(), y=numeric(), dur=numeric())
	detDat = data.frame(observer=numeric(), session=numeric(), trial=numeric(), targPres=character(), targSide=character(), row=character(), column=character(), easySide=character(), responseKey=character(), rt=numeric())

	for (t in 1:nTrials)
	{
		# first get fixations
		trial   = asc[trialStarts[t]:trialEnds[t]]		
		fixationLines = grep("EFIX", trial)

		startTime = unlist(strsplit(unlist(asc[trialStarts[t]])[2], " "))[1]
	
		if (length(fixationLines)>0)
		{
			fixations = as.data.frame(matrix(unlist(trial[fixationLines]), byrow=T, ncol=6))

			trialDat = data.frame(
				observer=pp, 
				session=ss,
				trial=t, 				
				x=as.numeric.factor(fixations$V4), y=as.numeric.factor(fixations$V5), dur=as.numeric.factor(fixations$V3))

			# convert to stimulus coordinates
			 		
			 trialDat$n = 1:length(trialDat$x)
			 fixDat = rbind(fixDat, trialDat)
			 rm(trialDat)
		}

		endTime = unlist(strsplit(unlist(asc[trialEnds[t]])[2], " "))[1]
		# now get details of trial
		details   = asc[trialEnds[t]:trialDone[t]]		
		
		targPres = GetDetail(details, "TargPres")
		targSide = GetDetail(details, "TargSide")
		row = GetDetail(details, "Row")
		column = GetDetail(details, "Column")
		easySide = GetDetail(details, "Easy_Side")
		response = 	GetDetail(details, "Key")

		trialDat = data.frame(
			observer=pp, 
			session=ss,
			trial=t, 
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

participants = c(1,2,3)
datFolder = '../../Data/'

fixDat = data.frame(observer=numeric(), trial=numeric(), n=numeric(), x=numeric(), y=numeric(), dur=numeric())
trlDat = data.frame(observer=numeric(), trial=numeric(), targPres=character(), targSide=character(), row=character(), column=character(), easySide=character(), responseKey=character())
	
for (pp in participants)
{
	for (ss in 1:2)
	{
		filename = paste(datFolder, pp, '/lineseg/Ul', pp, ss, '.asc', sep="")
		asc = strsplit(readLines(filename), "\t")
		dat = ProcessASC(asc, ss)

		fixDat = rbind(fixDat, dat[[1]])
		trlDat = rbind(trlDat, dat[[2]])
	}
}


# recode factors
levels(trlDat$targPres) = c("present", "absent")
levels(trlDat$targSide) = c("right", "absent", "left")
levels(trlDat$easySide) = c("left", "right")

# record targSide to be easy or hard!
relSide =trlDat$TargSide
relSide[trlDat$targSide=="left" & trlDat$easySide=="left"] = "easy"
relSide[trlDat$targSide=="right" & trlDat$easySide=="right"] = "easy"
relSide[trlDat$targSide=="right" & trlDat$easySide=="left"] = "hard"
relSide[trlDat$targSide=="left" & trlDat$easySide=="right"] = "hard"
relSide[trlDat$targSide=="absent"] = "absent"
trlDat$targSide = as.factor(relSide)


