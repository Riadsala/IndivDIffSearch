library(tidyverse)
library(forcats)
library(ggthemes)
library(scales)   
library(binom)

figXn <- 6
figYn <- 4

# read in processed data
trlDat <- readRDS(file = "scratch/processedRTandAccData.Rda")
fixDat <- readRDS(file = "scratch/processedFixationData.Rda")

# -----------------------------------------------------------------------------
# look at accuracy 
# -----------------------------------------------------------------------------

acc_dat  <- (trlDat %>% 
	group_by(observer, session, targSide) %>% 
	summarise(
		nTrials = length(accuracy),
		accuracy = mean(accuracy),
		lower = binom.confint(nTrials*accuracy, nTrials, method = 'exact')$lower,
		upper = binom.confint(nTrials*accuracy, nTrials, method = 'exact')$upper))
acc_dat <- select(acc_dat, -nTrials)
	
plt <- ggplot(acc_dat, aes(x = targSide, y = accuracy, fill = session))
plt <- plt + geom_bar(stat = "identity", position = position_dodge()) 
plt <- plt + scale_x_discrete(name = "target condition")
plt <- plt + facet_wrap( ~ observer)
plt <- plt + theme_hc() + scale_fill_ptol()
ggsave("scratch/acc_by_session_by_person.pdf", width=2*figXn, height=2*figYn)

acc_dat %>% unite(accuracy, accuracy, lower, upper) -> acc_dat
acc_dat <- spread(acc_dat, key = session, value = accuracy )

#  Why are there NAs???

acc_dat %>% 
	separate(a, into = c("a_acc", "a_lower", "a_upper"), sep = "_", convert = TRUE) %>%
	separate(b, into = c("b_acc", "b_lower", "b_upper"), sep = "_", convert = TRUE) -> acc_dat

r <- with(filter(acc_dat, targSide == "hard"), cor.test(a_acc, b_acc)$estimate)
r <- round(r, 2)

# compare hard accuracy between session a and b
plt <- ggplot(filter(acc_dat, targSide == "hard"), 
	aes(x = a_acc, xmin = a_lower, xmax = a_upper,
	  y = b_acc, ymin = b_lower, ymax = b_upper))
plt <- plt + geom_abline(slope=1, linetype=2)
plt <- plt + geom_errorbar(size=0.25) + geom_errorbarh(size=0.25)
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = "lm", se=FALSE, colour="black")
plt <- plt + geom_text(
	label = paste("italic(r) == ",r), x = 0.25, y = 0.8, parse = TRUE)
plt <- plt + scale_x_continuous(name = "accuracy session a ", limits = c(0, 1))
plt <- plt + scale_y_continuous(name = "accuracy session b ", limits = c(0, 1))
plt <- plt + theme_minimal() + scale_color_ptol(name = "target condition")
ggsave("scratch/acc_correlation.pdf", width=figYn, height=figYn)


# remove person 15 as they never found the hard targets
trlDat <- filter(trlDat, observer != 15)
fixDat <- filter(fixDat, observer != 15)

acc_dat  <- (trlDat %>% 
	group_by(observer, session, targSide) %>% 
	summarise(
		nTrials = length(accuracy),
		accuracy = mean(accuracy),
		lower = binom.confint(nTrials*accuracy, nTrials, method = 'exact')$lower,
		upper = binom.confint(nTrials*accuracy, nTrials, method = 'exact')$upper))
acc_dat <- select(acc_dat, -nTrials)

# plot boxplots
plt <- ggplot(acc_dat, aes(x = session, y = accuracy, fill = targSide))
plt <- plt + geom_boxplot()
plt <- plt + theme_hc() + scale_fill_ptol()
ggsave("scratch/acc_by_session.pdf", width=figXn, height=figYn)

# only take correct trials
trlDat <- filter(trlDat, accuracy == 1)
fixDat <- filter(left_join(fixDat, trlDat), accuracy==1)

# -----------------------------------------------------------------------------
# look at RT 
# -----------------------------------------------------------------------------

plt <- ggplot(trlDat, aes(x = rt/1000, fill = targSide)) 
plt <- plt + geom_density(alpha=0.7)
plt <- plt + scale_fill_ptol(name = "target position") + theme_hc()
plt <- plt + scale_x_continuous(name = "reaction time (seconds)", 
	trans = log2_trans(), 
	breaks = c(250, 1000, 4000, 16000, 64000)/1000) 
plt <- plt + theme(
	legend.justification = c(1,1), 
	legend.position = c(1,1),
	legend.background = element_rect(fill="white"))
ggsave("scratch/densityRT.pdf", width=figXn, height=figYn)


rt_dat <- (filter(trlDat) 
	%>% group_by(observer, session, targSide)
	%>% summarise(
		logrt = mean(log(rt,2)),
		n = length(rt),
		sderr = sd(log(rt,2)/sqrt(n)),
		upper = logrt + 1.96*sderr,
		lower = logrt - 1.96*sderr))

rt_dat <- select(rt_dat, observer, session, targSide, logrt, lower, upper)
rt_dat %>% unite(logrt, logrt, lower, upper) -> rt_dat
rt_dat <- spread(rt_dat, key = session, value = logrt )
rt_dat %>% 
	separate(a, into = c("a_log2rt", "a_lower", "a_upper"), sep = "_", convert = TRUE) %>%
	separate(b, into = c("b_log2rt", "b_lower", "b_upper"), sep = "_", convert = TRUE) -> rt_dat

# correlations between session a and b
easy_r = round(with(filter(rt_dat, targSide == "easy"), 
	cor.test(a_log2rt, b_log2rt)$estimate),2)

hard_r = round(with(filter(rt_dat, targSide == "hard"), 
	cor.test(a_log2rt, b_log2rt)$estimate),2)

absent_r = round(with(filter(rt_dat, targSide == "absent"), 
	cor.test(a_log2rt, b_log2rt)$estimate),2)

r_df = tibble(
	text = c(
		paste("r = ", easy_r), paste("r = ", hard_r), paste("r = ", absent_r) ),
	x = c(9.5, 10.55, 13.75), y = c(10.25, 11.5, 11.25), targSide = c("easy", "hard", "absent"))

plt <- ggplot(rt_dat, aes(
	x = a_log2rt,#, ,
	y = b_log2rt,#, ,
	colour=targSide))
plt <- plt + geom_abline(slope=1, linetype=2)
plt <- plt + geom_point() 
plt <- plt + geom_errorbar(size=0.25, aes(ymin = b_lower, ymax = b_upper)) + geom_errorbarh(size=0.25, aes(xmin = a_lower, xmax = a_upper))
plt <- plt + geom_smooth(method="lm", se=FALSE)
plt <- plt + theme_minimal() + scale_color_ptol(name = "target condition")
plt <- plt + scale_x_continuous("session 1: log reaction time (ms)")
plt <- plt + scale_y_continuous("session 2: log reaction time (ms)")
plt <- plt + theme(
	legend.justification = c(0,1), 
	legend.position = c(0,1),
	legend.background = element_rect(fill="white"))
plt <- plt + geom_text(data=r_df, aes(x = x, y = y, label = text, colour=targSide), show.legend = FALSE)
ggsave("scratch/rt_correlation.pdf", width=figYn, height=figYn)






