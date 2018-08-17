library(tidyverse)
library(forcats)
library(ggthemes)

figXn <- 3.5
figYn <- 2.5

dat <- read_csv("summaryData.csv")


cor_dat <- tibble(
	comparison = c(
		"sh-rt absent", 
		"sh-rt a versus b hard", 
		"sh-rt a versus b easy",
		"sh - strategy",
		"acvs prop optimal",
		"acvs switch rate",
		"MCFT (rt)",
		"MCFT (rn)"),
	estimate = c(0,0,0, 0.73, 0.83, 0.77, 0, 0),
	lower = c(0.65, 0.66, 0.56, 0.585, 0.72, 0.63, -1, -1),
	upper = c(0.86, 0.87, 0.82, 0.832, 0.90, 0.87, 1, 1),
	type = rep('test-retest', 8))

add_r_95 <- function(x1, x2, df, t) {
	ci95 <- cor.test(x1[[1]], x2[[1]])

	new_r <- tibble(
			comparison = paste(names(x1), names(x2), sep = "-"),
			estimate = ci95$estimate,
			lower = ci95$conf.int[1],
			upper = ci95$conf.int[2],
			type = t)

	df <- bind_rows(df, new_r)

	return(df)
}

cor_dat <- add_r_95(dat[3], dat[5], cor_dat, 'rt-strat')
cor_dat <- add_r_95(dat[8], dat[7], cor_dat, 'rt-strat')
cor_dat <- add_r_95(dat[13], dat[12], cor_dat, 'rt-strat')
cor_dat <- add_r_95(dat[14], dat[12], cor_dat, 'rt-strat')

cor_dat <- add_r_95(dat[3], dat[8], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat[3], dat[9], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat[3], dat[13], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat[3], dat[14], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat[8], dat[13], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat[8], dat[14], cor_dat, 'cross paradigm')

cor_dat <- add_r_95(dat[5], dat[7], cor_dat,  'cross paradigm')
# cor_dat <- add_r_95(dat[5], dat[11], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat[5], dat[12], cor_dat, 'cross paradigm')
# cor_dat <- add_r_95(dat[7], dat[11], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat[7], dat[12], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat[9], dat[13], cor_dat, 'cross paradigm')

cor_dat$comparison <- fct_rev(as_factor(cor_dat$comparison))

cor_dat$comparison <- fct_recode(cor_dat$comparison, 
	`ACVS (opt) - MCFT (rl)`  = "ac_propOpt-fg_conj_run_length",
	`ACVS (opt) - MCFT (rn)`  = "ac_propOpt-fg_conj_run_num",
	`SHLS (opt) - MCFT (rl)`  = "ls_prop_hetero-fg_conj_run_length",
	`SHLS (opt) - MCFT (rn)`  = "ls_prop_hetero-fg_conj_run_num",
	`SHLS (opt) - ACVS (sw)`  = "ls_prop_hetero-ac_switchRate",
	`SHLS (opt) - ACVS (opt)` = "ls_prop_hetero-ac_propOpt",
	`ACVS (rt) - ACVS (opt)`  = "ac_propOpt-ac_rt",
	`SHLS (opt) - SHLS (rt)`  = "ls_prop_hetero-ls_mean_log_rt",
	`ACVS (sw)`   = "acvs switch rate",
	`ACVS (opt)`  = "acvs prop optimal",
	`SHLS absent (rt)`  = "sh-rt absent",
	`SHLS easy (rt)`  = "sh-rt a versus b easy",
	`SHLS hard(rt)`  = "sh-rt a versus b hard",
	`SHLS (opt)` = "sh - strategy",
	`MCFT (rn) - MCFT (rt)`  = "fg_conj_run_num-fg_conj_log2",
	`MCFT (rl) - MCFT (rt)`  = "fg_conj_run_length-fg_conj_log2",
	`ACVS (rt) - MCFT (rt)` = "ac_rt-fg_conj_log2",          
    `SHLS (rt) - MCFT (rt)` = "ls_mean_log_rt-fg_conj_log2", 
  	`SHLS (rt) - ACVS (r)`   = "ls_mean_log_rt-ac_rt", 
  	`ACVS (sw) - MCFT (rn)`  = "ac_switchRate-fg_conj_run_num")




plt <- ggplot(cor_dat, aes(x = comparison, ymin = lower, ymax = upper, colour = type))
plt <- plt + geom_hline(yintercept = 0, linetype = 2)
plt <- plt + geom_linerange(size = 1)
plt <- ggplot(cor_dat, aes(x = comparison, ymin = lower, ymax = upper, colour = type))
plt <- plt + geom_hline(yintercept = 0, linetype = 2)
plt <- plt + geom_linerange(size = 1.5)
plt <- plt + scale_y_continuous(limits = c(-1, 1), expand = c(0,0))
plt <- plt + scale_color_ptol()
plt <- plt + theme_bw() 
plt <- plt + coord_flip()
plt <- plt + theme(
	legend.title = element_blank(),
	legend.position = "top",
	axis.title.y = element_blank())

ggsave("scratch/cor_comparison.pdf", width = 5, height = 7)
ggsave("scratch/cor_comparison.png", width = 5, height = 7)