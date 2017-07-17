library(tidyverse)

figXn <- 6
figYn <- 4

dat <- read_csv("summaryData.csv")


# first look at rt

r <- round(with(dat, cor.test(ls_median_rt, ac_rt)$estimate), 2)

plt <- ggplot(dat, aes(x = ls_median_rt, y = ac_rt))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black")
plt <- plt + theme_minimal()
plt <- plt + scale_x_continuous("lineseg: median rt (ms)")
plt <- plt + scale_y_continuous("adaptive choice: xxxx rt (ms)")
plt <- plt + geom_text(label=paste("italic(r) ==", r), parse = TRUE, x= 4000, y=5000)
ggsave("scratch/ls_v_ac_rt.pdf", width = figYn, height = figYn)

r_feat <- round(with(dat, cor.test(ls_median_rt, fg_medianrt_feature)$estimate), 2)
r_cong <- round(with(dat, cor.test(ls_median_rt, fg_medianrt_cong)$estimate), 2)

plt <- ggplot(dat, aes(x = ls_median_rt, y = fg_medianrt_feature))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black")
plt <- plt + theme_minimal()
plt <- plt + scale_x_continuous("lineseg: median rt (ms)")
plt <- plt + scale_y_continuous("foraging - feature: xxxx rt (ms)")
plt <- plt + geom_text(label=paste("italic(r) ==", r_feat), parse = TRUE, x= 4000, y=22500)
ggsave("scratch/ls_v_fg_feature_rt.pdf", width = figYn, height = figYn)

plt <- ggplot(dat, aes(x = ls_median_rt, y = fg_medianrt_cong))
plt <- plt + geom_point() 
plt <- plt + geom_smooth(method = lm, colour = "black")
plt <- plt + theme_minimal()
plt <- plt + scale_x_continuous("lineseg: median rt (ms)")
plt <- plt + scale_y_continuous("foraging - conjunction: xxxx rt (ms)")
plt <- plt + geom_text(label=paste("italic(r) ==", r_cong), parse = TRUE, x= 4000, y=22500)
ggsave("scratch/ls_v_fg_conj_rt.pdf", width = figYn, height = figYn)
