library(tidyverse)

figXn <- 6
figYn <- 4

dat <- read_csv("summaryData.csv")

############################################################
# first look at rt
############################################################

#  split-half v adaptive choice
r <- round(with(dat, cor.test(ls_mean_log_rt, ac_meanlog2rt)$estimate), 3)

x_labels = c(500, 1000, 2000, 4000, 8000)
x_breaks = log(x_labels, 2)

y_labels = c(1500, 3000, 6000)
y_breaks = log(y_labels, 2)

plt <- ggplot(dat, aes(x = ls_mean_log_rt, y = ac_meanlog2rt))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: mean log rt (ms)",
	  limits = c(8, 14), breaks = x_breaks, labels = x_labels, expand = c(0, .1))
plt <- plt + scale_y_continuous("adaptive choice: mean log rt (ms)",
	  limits = c(min(y_breaks), 14), breaks = y_breaks, labels = y_labels, expand = c(0, 0))
plt <- plt + geom_text(label=paste("r =", r), x= 10.25, y=12)
plt <- plt + coord_cartesian(xlim=c(min(x_breaks),max(x_breaks)), ylim=c(y_breaks[1],max(y_breaks)))
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
# plt <- plt + ggtitle('Split-half and Attentional Control')
ggsave("scratch/ls_v_ac_mean_log2rt.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_ac_mean_log2rt.png", width = figYn, height = figYn)

r_feat <- round(with(dat, cor.test(ls_mean_log_rt, fg_feat_log2)$estimate), 3)
r_cong <- round(with(dat, cor.test(ls_mean_log_rt, fg_conj_log2)$estimate), 3)

y_labels = c(1500, 3000, 6000)
y_breaks = log(y_labels, 2)

plt <- ggplot(dat, aes(x = ls_mean_log_rt, y = fg_feat_log2))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: median rt (ms)")
plt <- plt + scale_y_continuous("foraging - feature: xxxx rt (ms)")
plt <- plt + geom_text(label=paste("italic(r) ==", r_feat), parse = TRUE, x= 4000, y=22500)
# plt <- plt + ggtitle('Attentional Control and Foraging (feautre)')
ggsave("scratch/ls_v_fg_feature_rt.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_fg_feature_rt.png", width = figYn, height = figYn)

plt <- ggplot(dat, aes(x = ls_mean_log_rt, y = fg_conj_log2))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black")
plt <- plt + theme_minimal()
plt <- plt + scale_x_continuous("split-half: median rt (ms)")
plt <- plt + scale_y_continuous("foraging - conjunction: xxxx rt (ms)")
plt <- plt + geom_text(label=paste("italic(r) ==", r_cong), parse = TRUE, x= 4000, y=22500)
# plt <- plt + ggtitle('Attentional Control and Foraging (conjunction)')
ggsave("scratch/ls_v_fg_conj_rt.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_fg_conj_rt.png", width = figYn, height = figYn)



r_feat <- round(with(dat, cor.test(ac_meanlog2rt, fg_feat_log2)$estimate), 3)
r_cong <- round(with(dat, cor.test(ac_meanlog2rt, fg_conj_log2)$estimate), 3)

plt <- ggplot(dat, aes(x = ac_meanlog2rt, y = fg_feat_log2))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black")
plt <- plt + theme_minimal()
plt <- plt + scale_x_continuous("adaptive choice: median rt (ms)")
plt <- plt + scale_y_continuous("foraging - feature: xxxx rt (ms)")
plt <- plt + geom_text(label=paste("italic(r) ==", r_feat), parse = TRUE, x= 4000, y=22500)
# plt <- plt + ggtitle('Attentional Control and Foraging (feautre)')
ggsave("scratch/ac_v_fg_feature_rt.pdf", width = figYn, height = figYn)
ggsave("scratch/ac_v_fg_feature_rt.png", width = figYn, height = figYn)

plt <- ggplot(dat, aes(x = ac_meanlog2rt, y = fg_conj_log2))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black")
plt <- plt + theme_minimal()
plt <- plt + scale_x_continuous("adaptive choice: median rt (ms)")
plt <- plt + scale_y_continuous("foraging - conjunction: xxxx rt (ms)")
plt <- plt + geom_text(label=paste("italic(r) ==", r_cong), parse = TRUE, x= 4000, y=22500)
# plt <- plt + ggtitle('Attentional Control and Foraging (conjunction)')
ggsave("scratch/ac_v_fg_conj_rt.pdf", width = figYn, height = figYn)
ggsave("scratch/ac_v_fg_conj_rt.png", width = figYn, height = figYn)


############################################################
# now look at strategy metrics
############################################################

#  split-half v adaptive choice
r <- round(with(dat, cor.test(ls_prop_hetero, ac_propOpt)$estimate), 3)

plt <- ggplot(dat, aes(x = ls_prop_hetero, y = ac_propOpt))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: prop. hetero. fixations",
	  limits = c(0, 1), expand = c(0, 0))
plt <- plt + scale_y_continuous("adaptive choice: prop. optimal",
	  limits = c(0, 1), expand = c(0, 0))
plt <- plt + geom_text(label=paste("r =", r), x= 10.25, y=12)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
# plt <- plt + ggtitle('Split-half and Attentional Control')
ggsave("scratch/ls_v_ac_opt.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_ac_opt.png", width = figYn, height = figYn)

r <- round(with(dat, cor.test(ls_prop_hetero, ac_switchRate)$estimate), 3)

plt <- ggplot(dat, aes(x = ls_prop_hetero, y = ac_switchRate))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE)
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("split-half: prop. hetero. fixations",
	  limits = c(0, 1), expand = c(0, 0))
plt <- plt + scale_y_continuous("adaptive choice: switch rate")
plt <- plt + geom_text(label=paste("r =", r), x= 10.25, y=12)
plt <- plt + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())
# plt <- plt + ggtitle('Split-half and Attentional Control')
ggsave("scratch/ls_v_ac_sr.pdf", width = figYn, height = figYn)
ggsave("scratch/ls_v_ac_sr.png", width = figYn, height = figYn)
