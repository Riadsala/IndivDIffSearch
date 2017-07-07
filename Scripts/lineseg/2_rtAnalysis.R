library(tidyverse)
library(forcats)
library(ggthemes)
library(scales)   

figXn = 6
figYn = 4
# read in processed data
trlDat <- readRDS(file="scratch/processedRTandAccData.Rda")


# look at accuracy ---------------------------------------------------------------
accDat  = aggregate(data=trlDat, accuracy ~ observer + session + targSide, FUN="mean")

plt <- ggplot(accDat, aes(x = targSide, y = accuracy, fill = session))
plt <- plt + geom_bar(stat = "identity", position = position_dodge()) 
plt <- plt + scale_x_continuous(name = "target condition")
plt <- plt + facet_wrap( ~ observer)
plt <- plt + theme_hc() + scale_fill_ptol()
ggsave("scratch/acc_by_session_by_person.pdf", width=2*figXn, height=2*figYn)

# remove person 15 as they never found the hard targets
trlDat <- filter(trlDat, observer != 15)
fixDat <- filter(fixDat, observer != 15)

# plot boxplots
plt <- ggplot(accDat, aes(x = session, y = accuracy, fill = targSide))
plt <- plt + geom_boxplot()
plt <- plt + theme_hc() + scale_fill_ptol()
ggsave("scratch/acc_by_session.pdf", width=figXn, height=figYn)

# only take correct trials
trlDat <- filter(trlDat, accuracy == 1)

# look at RT ---------------------------------------------------------------------

plt <- ggplot(trlDat, aes(x = rt, fill = targSide)) + geom_density(alpha=0.7)
plt <- plt + scale_fill_ptol(name = "target position") + theme_minimal()
plt <- plt + scale_x_continuous(name = "reaction time (ms)", 
	trans = log2_trans(), 
	breaks = c(250, 1000, 4000, 16000, 64000)) 
plt <- plt + theme(
	legend.justification = c(1,1), 
	legend.position = c(1,1),
	legend.background = element_rect(fill="white"))
ggsave("scratch/densityRT.pdf", width=figXn, height=figYn)
ggsave("scratch/densityRT.png", width=figXn, height=figYn)

rtDat = (filter(trlDat) 
	%>% group_by(observer, session, targSide)
	%>% summarise(
		logrt = mean(log(rt,2)),
		n = length(rt),
		sderr = sd(log(rt,2)/sqrt(n)),
		upper = logrt + 1.96*sderr,
		lower = logrt - 1.96*sderr))

rtDat = select(rtDat, observer, session, targSide, logrt, lower, upper)
rtDat %>% unite(logrt, logrt, lower, upper) -> rtDat
rtDat <- spread(rtDat, key = session, value = logrt )
rtDat %>% 
	separate(a, into = c("a_log2rt", "a_lower", "a_upper"), sep = "_", convert = TRUE) %>%
	separate(b, into = c("b_log2rt", "b_lower", "b_upper"), sep = "_", convert = TRUE) -> rtDat


plt <- ggplot(rtDat, aes(
	x = a_log2rt, xmin = a_lower, xmax = a_upper,
	y = b_log2rt, ymin = b_lower, ymax = b_upper,
	colour=targSide))
plt <- plt + geom_point() 
plt <- plt + geom_errorbar(lineWidth=0.5) + geom_errorbarh(lineWidth=0.5)
plt <- plt + geom_smooth(method="lm", se=FALSE)
plt <- plt + theme_minimal() + scale_color_ptol()
plt <- plt + geom_abline(slope=1, linetype=2)
plt <- plt + scale_x_continuous("session 1: log reaction time (ms)")
plt <- plt + scale_y_continuous("session 2: log reaction time (ms)")
plt <- plt + theme(
	legend.justification = c(0,1), 
	legend.position = c(0,1),
	legend.background = element_rect(fill="white"))
ggsave("scratch/rt_correlation.pdf", width=figYn, height=figYn)







# # now lets look at RTs... 
# # first we need to filter out incorrect trials
# rtdat = rtdat[which(rtdat$acc==1),]
# library(scales)     
# # rt for target present and absent
# rtdat  = aggregate(data=rtdat, RT ~ subj +targSide, FUN="median")

# ggsave("plots/RT.jpg",dpi=600, width=figXn, height=figYn)




