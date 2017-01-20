
library(dplyr)
# TODO: check subjects to replace!
 # setwd("C:/Users/r02al13/Documents/GitHub/HalfScreenPopOut")

saccInfo <- function(trialDat)
{
	# this is a funtion that calculates saccade amplitude and angle for a sequenece of fixations in a  trial
	nFix = max(trialDat$fixNum)
	saccInfo  = trialDat
	saccAmp2 = vector()
	theta = vector()
	for (f in 1:(nFix-1))
	{
		dx = trialDat$fixX[f] - trialDat$fixX[f+1]
		dy = trialDat$fixY[f] - trialDat$fixY[f+1]
		saccAmp2[f] 	= dx^2 + dy^2
		theta[f] 		= atan2(dx, dy)
	}
	saccAmp2[nFix] = NaN
	theta[nFix]    = NaN
	saccInfo$amp   = sqrt(saccAmp2)
	saccInfo$ang   = theta
	return(saccInfo)
}


subjectsToRemove = c(2)#one experimenter nr.2, only completed a few trials, and two at chance in parallel, 4 and 15 at chance
# max fixation duration - remove trial if it is exceeded 
maxFixDur = 2000

# read in reaction time and acc data:
# this will allow us to remove fixation data for incorrect trials
print("Processing RT and Acc data")
dat <- read.csv("../data/RtAcc.txt", sep="\t")
names(dat) = c("subj", "trialNum", "hemiType", "hemiSide","easySide", "targPresent", "targSide", "RT", "X", "acc")
dat = select(dat, subj, trialNum, easySide, targPresent, targSide, RT, acc)

# Turn categorical data into factor
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
levels(dat$targSide) = c('left', 'right', 'absent')
dat$easySide = as.factor(dat$easySide)
levels(dat$easySide) = c("left", "right")

# remove some subjects

dat$subj = as.factor(dat$subj)
dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = factor(dat$subj)
levels(dat$subj)

# refdefine targSide relative to easySide
dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("hetrogeneous", "homogeneous", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"

# make a new, tidier version of dataframe only including the stuff we want!
rtdat = data.frame(subj=dat$subj, trial=dat$trial, targSide=dat$targSideRel, RT=dat$RT, acc=dat$acc, easySide=dat$easySide)
# we don't want to be looking at RTs for incorrect trials
rtdat$RT[rtdat$acc==0] = NaN

# save!!!
saveRDS(rtdat,file="../data/processedRTandAccData.Rda")

# remove data for now
rm(dat, rtdat)


#############################
# now read in fixation data #
#############################

print("Processing Fix data...")
dat <- read.csv("../data/Fix.txt", header=T, sep="\t",
	colClass = c(
		"subj"="factor", 
		"trialNum"="numeric", 
		"fixNum"="numeric", 
		"hemiType"="factor",   
		"fixX" = "numeric",
		"fixY" = "numeric",
		"fixOn" = "numeric",
		"fixOff" = "numeric",
		"hemiSide" = "factor",
		"targPresent" = "factor",
		"targSide" = "factor",
		"row" = "numeric",
		"column" = "numeric",
		"easySide"="factor",
		"name"="character"))
names(dat) = c("subj", "trialNum","fixNum", "hemiType", "fixX","fixY","fixOn","fixOff","hemiSide", "targPresent", "targSide", "row", "column", "easySide","name")


levels(dat$targPresent) = c("absent", "present")
levels(dat$targSide) = c("left", "right", "absent")
levels(dat$easySide) = c("left", "right")

dat$row[which(dat$targPresent=='absent')] = NaN;
dat$column[which(dat$targPresent=='absent')] = NaN;

dat = select(dat, subj, trialNum, fixNum, easySide, fixX, fixY, fixOn, fixOff, targPresent, targSide, row, column)

# refdefine targSide relative to easySide - ie, hetrogeneous array always on left
dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("hetrogeneous", "homogeneous", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"

# calcualte fixation durations
dat$fixDur = with(dat, fixOff - fixOn)

# remove unwanted participants

dat$subj = as.factor(dat$subj)
dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = factor(dat$subj)
levels(dat$subj)


# #we want to filter out all incorrect trials!
 print("...removing fixation for incorrect trials and fix.dur exceptions")
 accdat = readRDS(file="../data/processedRTandAccData.Rda")
 dat$acc = 0
 for (s in levels(dat$subj))
 {
 	subjDat = filter(dat, subj==s)
      subjDat$trialNum = factor(subjDat$trialNum)
 	for (t in levels(subjDat$trialNum))
 	{
  		j = which(accdat$trial==t & accdat$subj==s)
  		idx = which(dat$subj==s & dat$trialNum==t)
      	if (accdat$acc[j]==1 & max(dat$fixDur[idx]) <= maxFixDur )
      	{
      		dat$acc[idx] = 1
      	}
      }
  }
  print(paste("... keeping ", 100*mean(dat$acc), "% of fixations"))
  dat = filter(dat, acc==1)

saveRDS(dat,file="../data/processedFixData.Rda")
 rm(dat)

fixdat = readRDS(file="../data/processedFixData.Rda")


#
# flip hemiSide = right so we can pretend hemiSide isn't a condition
#
# print("...flipping trials for hemi==right")

 fixdat$fixX = fixdat$fixX - 512 -128
# itemdat$itemX = itemdat$itemX - 512 + 64
  
for (s in levels(fixdat$subj))
{
	subjDat = fixdat[which(fixdat$subj==s),]
	subjDat$trialNum = factor(subjDat$trialNum)
	for (t in levels(subjDat$trialNum))
	{
		if (subjDat$easySide[which(subjDat$trialNum==t)][1]=="right")
		{
			idx = which(fixdat$subj==s & fixdat$trialNum==t)
			fixdat$fixX[idx] = - fixdat$fixX[idx]
		}
	}
	rm(subjDat)
}
rm(s,t, idx)

 fixdat$fixX = fixdat$fixX + 512
 fixdat$fixY = fixdat$fixY -256
# itemdat$itemX = itemdat$itemX + 512 - 64


# define itemID for each face
# qx = 8*itemdat$itemX/1024
# qy = 6*itemdat$itemY/768
# itemdat$isTarget = (itemdat$itemID == " Target")
# itemdat$itemID = as.factor((qx)*10 + (qy))

#
# get saccade info
#
print("...calcualting sacc amp and ang")
fixdat$saccAmp = NaN
fixdat$saccAng = NaN
for (s in levels(fixdat$subj))
{
	subjdat = fixdat[which(fixdat$subj==s),]
	subjdat$trialNum = factor(subjdat$trialNum)
	for (t in levels(subjdat$trialNum))
	{
		if (length(which(fixdat$subj==s & fixdat$trialNum==t))>0)
		{
			saccDat    = saccInfo(fixdat[which(fixdat$subj==s & fixdat$trialNum==t),])		
			fixdat$saccAmp[which(fixdat$subj==s & fixdat$trialNum==t)] = saccDat$amp
			fixdat$saccAng[which(fixdat$subj==s & fixdat$trialNum==t)] = saccDat$ang	
			rm(saccDat)	
		}
	}
	rm(subjdat)
}
rm(s, t)

#
# assigning fixations to regions
#
# print("...assigning fixations to regions")
# nItemsX = 8
# nItemsY = 6
# # quantise fix!
# qx = ceiling(nItemsX*fixdat$fixX/1024)
# qy = ceiling(nItemsY*fixdat$fixY/768)
# qx[qx<1] = NaN
# qy[qy<1] = NaN
# qx[qx>7] = NaN
# qy[qy>5] = NaN

# fixdat$fixRegion = as.factor((qx-1)*10 + (qy-1))

# for (s in levels(fixdat$subj))
# {
# 	subjdat = fixdat[which(fixdat$subj==s),]
# 	subjdat$trial = factor(subjdat$trial)
#  	for (t in levels(subjdat$trial))
# 	{
# 		itemTrialDat = itemdat[itemdat$subj==s & itemdat$trial==t,] 		
# 		trialDat = fixdat[s==fixdat$subj & t==fixdat$trial,]
# 		trialDat = removeregionsthatwerentpresent(itemTrialDat, trialDat)
# 		fixdat[s==fixdat$subj & t==fixdat$trial,] = trialDat
# 	}
# 	rm(subjdat)
# }
 dat = fixdat
fixdat = data.frame(subj=dat$subj, trial=dat$trialNum, targSide=dat$targSideRel, fixNum=dat$fixNum, fixX=dat$fixX, fixY=dat$fixY, fixDur=dat$fixDur, saccAmp=dat$saccAmp, saccAng=dat$saccAng, easySide=dat$easySide)


saveRDS(fixdat,file="../data/processedFixData.Rda")
write.table(fixdat, "../data/processedFixData.txt", sep=",")




