library(tidyverse)


figXn <- 3.5
figYn <- 2.5

dat <- read_csv("summaryData_75.csv")


# remove people who had the wrong resolution!
idx <- c(1, 2, 3, 4, 11, 31, 69)

dat <- filter(dat, !(observer %in% idx))

# numbers taken from inital parsing script for split half paradigm


############################################################
# ACVS opt v rt
############################################################

r <- (with(dat, cor.test(ac_propOpt, ac_meanlog2rt)))

y_labels = seq(2,5, 0.5)
y_breaks = log(1000* y_labels, 2)



plt <- ggplot(dat, aes(x = ac_propOpt, y = ac_meanlog2rt))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype = 1)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("ACVS proportion optimal",
	  limits = c(0, 1 ), breaks = c(0, 1), expand = c(0, 0))
plt <- plt + scale_y_continuous("ACVS reaction time (s)",
	  breaks = y_breaks, labels = y_labels, expand = c(0, 0))
plt <- plt + coord_cartesian(xlim=c(0, 1), ylim=c(10.8, 12.3))
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
# plt <- plt + ggtitle('Split-half and Attentional Control')
ggsave("scratch/acvs_opt_rt.pdf", width = figYn, height = figYn)
plt

############################################################
# MCFT opt v rt
############################################################


r <- (with(dat, cor.test(fg_conj_run_length, fg_conj_log2)))

y_labels = seq(24, 60, 6)
y_breaks = log(1000* y_labels, 2)


plt <- ggplot(dat, aes(x = fg_conj_run_length, y = fg_conj_log2))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype = 1)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("MCFT run length",
    limits = c(1, 20), breaks = c(1, 20), expand = c(0, 0))
plt <- plt + scale_y_continuous("MCFT reaction time (s)",
    breaks = y_breaks, labels = y_labels, expand = c(0, 0))
plt <- plt + coord_cartesian(xlim=c(0.9, 20), ylim=c(14.45, 15.87))
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
# plt <- plt + ggtitle('Split-half and Attentional Control')
ggsave("scratch/mcft_rl_rt.pdf", width = figYn, height = figYn)













r_feat <- round(with(dat, cor.test(ls_mean_log_rt, fg_feat_log2)$estimate), 3)
r_conj <- round(with(dat, cor.test(ls_mean_log_rt, fg_conj_log2)$estimate), 3)

y_breaks = c(14.13571, 14.70995, 15.28771)
y_labels = round(2^y_breaks)

plt <- ggplot(dat, aes(x = ls_mean_log_rt, y = fg_feat_log2))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype=0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: mean log rt (ms)",
    limits = c(8, 14.1), breaks = x_breaks, labels = x_labels, expand = c(0, 0))
plt <- plt + scale_y_continuous("foraging (feature): mean log rt (ms)",
    limits = c(y_breaks[1], y_breaks[3]), breaks = y_breaks, labels = y_labels,expand = c(0, 0))
plt <- plt + coord_cartesian(xlim=c(min(x_breaks),max(x_breaks)+0.2), ylim=c(y_breaks[1],max(y_breaks)))
plt <- plt + geom_text(label=paste("r =", r_feat), x= 12, y=15.1)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
ggsave("scratch/ls_v_fg_feature_rt.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_fg_feature_rt.png", width = figYn, height = figYn)

#  split-half with forgaging conjunction

y_breaks = c(14.28771, 14.91998, 15.55075)
y_labels = round(2^y_breaks)

plt <- ggplot(dat, aes(x = ls_mean_log_rt, y = fg_conj_log2))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype=0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: mean log rt (ms)",
   limits = c(8, 14.1), breaks = x_breaks, labels = x_labels, expand = c(0, 0))
plt <- plt + scale_y_continuous("foraging (conj.): mean log rt (ms)",
    limits = c(y_breaks[1], y_breaks[3]), breaks = y_breaks, labels = y_labels,expand = c(0, 0))
# plt <- plt + geom_text(label=paste("r =", r_conj), x= 12, y=14.5)
plt <- plt + coord_cartesian(xlim=c(min(x_breaks),max(x_breaks)+0.2), ylim=c(y_breaks[1],max(y_breaks)))
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
ggsave("scratch/ls_v_fg_conj_rt.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_fg_conj_rt.png", width = figYn, height = figYn)

#  adaptive choice with forgaging feature

x_labels = c(1500, 3000, 6000)
x_breaks = log(x_labels, 2)
y_breaks = c(14.13571, 14.71171, 15.28771)
y_labels = round(2^y_breaks)

r_feat <- round(with(dat, cor.test(ac_meanlog2rt, fg_feat_log2)$estimate), 3)
r_conj <- round(with(dat, cor.test(ac_meanlog2rt, fg_conj_log2)$estimate), 3)

plt <- ggplot(dat, aes(x = ac_meanlog2rt, y = fg_feat_log2))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype=0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("adaptive choice: mean log rt (ms)",
    limits = c(x_breaks[1], x_breaks[3]+0.05), breaks = x_breaks, labels = x_labels, expand = c(0, 0))
plt <- plt + scale_y_continuous("foraging (feature): mean log rt (ms)",
    limits = c(y_breaks[1], y_breaks[3]), breaks = y_breaks, labels = y_labels, expand = c(0, 0))
# plt <- plt + geom_text(label=paste("r =", r_feat), x= 11.1, y= 15)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
ggsave("scratch/ac_v_fg_feature_rt.pdf", width = figYn, height = figYn)
ggsave("scratch/ac_v_fg_feature_rt.png", width = figYn, height = figYn)


y_breaks = c(14.28771, 14.91998, 15.55075)
y_labels = round(2^y_breaks)
plt <- ggplot(dat, aes(x = ac_meanlog2rt, y = fg_conj_log2))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype=0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("adaptive choice: mean log rt (ms)",
    limits = c(x_breaks[1], x_breaks[3]+0.05), breaks = x_breaks, labels = x_labels, expand = c(0, 0))
plt <- plt + scale_y_continuous("foraging (conj.): mean log rt (ms)",
    limits = c(y_breaks[1], y_breaks[3]), breaks = y_breaks, labels = y_labels, expand = c(0, 0))
plt <- plt + geom_text(label=paste("r =", r_conj), x= 11, y= 15.2)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
ggsave("scratch/ac_v_fg_conj_rt.pdf", width = figYn, height = figYn)
ggsave("scratch/ac_v_fg_conj_rt.png", width = figYn, height = figYn)


############################################################
# now look at strategy metrics
############################################################

#  split-half v adaptive choice
r <- round(with(dat, cor.test(ls_prop_hetero, ac_propOpt)$estimate), 3)

plt <- ggplot(dat, aes(x = ls_prop_hetero, y = ac_propOpt))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype = 0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: prop. hetero. fix.",
	  limits = c(0, 1), expand = c(0, 0), breaks = c(0,1))
plt <- plt + scale_y_continuous("adaptive choice: prop. optimal",
	  limits = c(0, 1), expand = c(0, 0), breaks = c(0,1))
# plt <- plt + geom_text(label=paste("r =", r), x = 0.5, y= 0.25)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
ggsave("scratch/ls_v_ac_opt.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_ac_opt.png", width = figYn, height = figYn)

r <- round(with(dat, cor.test(ls_prop_hetero, ac_switchRate)$estimate), 3)

plt <- ggplot(dat, aes(x = ls_prop_hetero, y = ac_switchRate))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype = 0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: prop. hetero. fix.",
	  limits = c(0, 1), expand = c(0, 0), breaks = c(0,1))
plt <- plt + scale_y_continuous("adaptive choice: switch rate")
# plt <- plt + geom_text(label=paste("r =", r), x= 0.25, y=0.18)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())

ggsave("scratch/ls_v_ac_sr.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_ac_sr.png", width = figYn, height = figYn)


# split-half and forgaing conjunction
r <- round(with(dat, cor.test(ls_prop_hetero, fg_conj_run_num)$estimate), 3)

plt <- ggplot(dat, aes(x = ls_prop_hetero, y = fg_conj_run_num))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype = 0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: prop. hetero. fix.",
    limits = c(0, 1), expand = c(0, 0), breaks = c(0,1))
plt <- plt + scale_y_continuous("foraging (conj.): number of runs",
    limits = c(0, 20), expand = c(0, 0))
# plt <- plt + geom_text(label=paste("r =", r), x = 0.25, y= 2)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
ggsave("scratch/ls_v_fg_runnum.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_fg_runnum.png", width = figYn, height = figYn)

r <- round(with(dat, cor.test(ls_prop_hetero, fg_conj_run_length)$estimate), 3)

plt <- ggplot(dat, aes(x = ls_prop_hetero, y = fg_conj_run_length))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype = 0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: prop. hetero. fix.",
    limits = c(0, 1), expand = c(0, 0), breaks = c(0,1))
plt <- plt + scale_y_continuous("foraging (conj.): run length",
    limits = c(0, 20), expand = c(0, 0))
# plt <- plt + geom_text(label=paste("r =", r), x = 0.25, y= 2)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
ggsave("scratch/ls_v_fg_runlength.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_fg_runlength.png", width = figYn, height = figYn)


# adaptive choice and foraging
r <- round(with(dat, cor.test(ac_propOpt, fg_conj_run_num)$estimate), 3)

plt <- ggplot(dat, aes(x = ac_propOpt, y = fg_conj_run_num))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype = 0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("adaptive choice: prop. optimal",
    limits = c(0, 1), expand = c(0, 0), breaks = c(0,1))
plt <- plt + scale_y_continuous("foraging (conj.): number of runs",
    limits = c(0, 20), expand = c(0, 0))
# plt <- plt + geom_text(label=paste("r =", r), x = 0.25, y= 2)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
ggsave("scratch/ac_v_fg_runnum.pdf", width = figYn, height = figYn)
ggsave("scratch/ac_v_fg_runnum.png", width = figYn, height = figYn)

r <- round(with(dat, cor.test(ac_propOpt, fg_conj_run_length)$estimate), 3)

plt <- ggplot(dat, aes(x = ac_propOpt, y = fg_conj_run_length))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype = 0)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("adaptive choice: prop. optimal",
    limits = c(0, 1), expand = c(0, 0), breaks = c(0,1))
plt <- plt + scale_y_continuous("foraging (conj.): run length",
    limits = c(-10, 30), expand = c(0, 0))
# plt <- plt + geom_text(label=paste("r =", r), x = 0.25, y= 2)
plt <- plt + coord_cartesian(xlim=c(0, 1), ylim=c(0, 20))

plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
ggsave("scratch/ac_v_fg_runlength.pdf", width = figYn, height = figYn)
ggsave("scratch/ac_v_fg_runlength.png", width = figYn, height = figYn)

