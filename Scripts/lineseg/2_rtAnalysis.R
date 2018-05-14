library(tidyverse)
library(forcats)
library(ggthemes)
library(scales)   
library(binom)

figXn <- 3.5
figYn <- 2.5

# read in processed data
trl_dat <- readRDS(file = "scratch/processedRTandAccData.Rda")

# -----------------------------------------------------------------------------
# look at accuracy 
# -----------------------------------------------------------------------------

acc_dat  <- (trl_dat %>% 
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
plt <- plt + scale_y_continuous(breaks = c(0,1))
plt <- plt + facet_wrap( ~ observer)
plt <- plt + theme_bw() + scale_fill_ptol() + theme(legend.position="top")
plt <- plt + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
ggsave("scratch/acc_by_session_by_person.pdf", width=2*figXn, height=2.5*figYn)

# remove people with poor target easy/absent accuracy
acc_dat <- filter(acc_dat, !(observer %in% c(4, 21, 33, 56, 58)))
trl_dat <- filter(trl_dat, !(observer %in% c(4, 21, 33, 56, 58)))

acc_dat %>% unite(accuracy, accuracy, lower, upper) -> acc_dat
acc_dat <- spread(acc_dat, key = session, value = accuracy )


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
plt <- plt + geom_errorbar(size=0.25, colour = "grey") 
plt <- plt + geom_errorbarh(size=0.25, colour = "grey")
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = "lm", colour="black", fullrange = TRUE)
plt <- plt + coord_cartesian(xlim=c(0,1), ylim=c(0,1)) 
# plt <- plt + geom_text(
# 	label = paste("r = ",r), x = 0.25, y = 0.875, family="Times")
plt <- plt + scale_x_continuous(name = "a: accuracy", limits = c(-0.50, 1.5), breaks = c(0,1))
plt <- plt + scale_y_continuous(name = "b: accuracy", limits = c(-0.50, 1.5), breaks = c(0,1))
plt <- plt + theme_bw() + scale_color_ptol(name = "target condition")
plt <- plt + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("scratch/acc_correlation.pdf", width=figYn, height=figYn)
ggsave("scratch/acc_correlation.png", width=figYn, height=figYn)


# remove person 15 as they never found the hard targets
# trl_dat <- filter(trl_dat, observer != 15)

acc_dat  <- (trl_dat %>% 
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
trl_dat <- filter(trl_dat, accuracy == 1)


# -----------------------------------------------------------------------------
# look at RT 
# -----------------------------------------------------------------------------

# look at distributions

plt <- ggplot(trl_dat, aes(x = rt))
plt <- plt + geom_histogram(binWidth = 1)
plt <- plt + facet_wrap(~ targSide)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous(name = "reaction time (seconds)", limits = c(0, 50000))
ggsave("scratch/rt_dists.pdf", width = 2*figXn, height = figYn)



plt <- ggplot(trl_dat, aes(x = rt/1000, fill = targSide)) 
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


rt_dat <- (filter(trl_dat) 
	%>% group_by(observer, session, targSide)
	%>% summarise(
		logrt = mean(log(rt, 2)),
		n = length(rt),
		sderr = sd(log(rt,2)/sqrt(n)),
		upper = logrt + 1.96*sderr,
		lower = logrt - 1.96*sderr))


plt <- ggplot(rt_dat, aes(x = logrt))
plt <- plt + geom_histogram(binwidth = 0.25)
plt <- plt + facet_wrap(~ targSide)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous(name = "mean log2 reaction time (ms)")
ggsave("scratch/rt_dists_mean.pdf", width = 2*figXn, height = figYn)



plt <- ggplot(trl_dat, aes(x = logrt/1000))
plt <- plt + geom_histogram(binWidth = 1)
plt <- plt + facet_wrap(~ targSide)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous(name = "reaction time (seconds)", limits = c(0, 50))
ggsave("scratch/rt_dists.pdf", width = 2*figXn, height = figYn)



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
	x = c(12.5, 10.55, 13.75), y = c(10.75, 12, 11.25), targSide = c("easy", "hard", "absent"))

a_labels = c(1, 2, 4, 8, 16)
a_breaks = log(1000*a_labels, 2)
levels(rt_dat$targSide) = c("homo", "hetero", "absent")

plt <- ggplot(rt_dat, aes(
	x = a_log2rt,#, ,
	y = b_log2rt,#, ,
	colour=targSide))
plt <- plt + geom_abline(slope=1, linetype=2)
plt <- plt + geom_point() 
plt <- plt + geom_errorbar(size=0.25, alpha = 0.5, aes(ymin = b_lower, ymax = b_upper))
plt <- plt + geom_errorbarh(size=0.25, alpha = 0.5, aes(xmin = a_lower, xmax = a_upper))
plt <- plt + geom_smooth(method="lm", se=FALSE)
plt <- plt + theme_bw() + scale_color_ptol(name = "target condition")
plt <- plt + scale_x_continuous("a: reaction time (s)", 
	limits = c(10.7, 14.5), breaks = a_breaks, labels = a_labels)
plt <- plt + scale_y_continuous("b: reaction time (s)", 
	limits = c(10.7, 14.5), breaks = a_breaks, labels = a_labels)
plt <- plt + theme_bw()
# plt <- plt + theme(
# 	legend.justification = c(-0.05,1), 
# 	legend.position = c(0,0.99),
# 	legend.background = element_rect(fill="white"),
# 	panel.grid.major = element_blank(), 
# 	panel.grid.minor = element_blank())
plt <- plt + theme(legend.justification=c(0,1), legend.position=c(0,1),legend.title=element_blank(),
	legend.background = element_rect(fill = alpha("white",0.1)),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# plt <- plt + geom_text(data=r_df, aes(x = x, y = y, label = text, colour=targSide), 
	# show.legend = FALSE, family="Times")
ggsave("scratch/rt_correlation.pdf", width=figYn, height=figYn)
ggsave("scratch/rt_correlation.png", width=figYn, height=figYn)
plt



