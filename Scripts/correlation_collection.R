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
		"MCFT conjunction (rn)"),
	estimate = c(0,0,0, 0.73, 0.83,  0.88),
	lower = c(0.65, 0.66, 0.56, 0.585, 0.72,  .81),
	upper = c(0.86, 0.87, 0.82, 0.832, 0.90, .92),
	type = rep('test-retest', 6))

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

cor_dat <- add_r_95(dat["ls_prop_hetero"], dat["ls_mean_log_rt"], cor_dat, 'rt-strat')
cor_dat <- add_r_95(dat["ac_propOpt"], dat["ac_meanlog2rt"], cor_dat, 'rt-strat')
cor_dat <- add_r_95(dat["fg_conj_run_length"], dat["fg_conj_log2"], cor_dat, 'rt-strat')


cor_dat <- add_r_95(dat["ls_prop_hetero"], dat["ac_propOpt"], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat["ls_prop_hetero"], dat["fg_conj_run_length"], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat["ac_propOpt"], dat["fg_conj_run_length"], cor_dat, 'cross paradigm')

cor_dat <- add_r_95(dat["ls_mean_log_rt"], dat["ac_meanlog2rt"], cor_dat,  'cross paradigm')
cor_dat <- add_r_95(dat["ls_mean_log_rt"], dat["fg_conj_log2"], cor_dat, 'cross paradigm')
cor_dat <- add_r_95(dat["ac_meanlog2rt"], dat["fg_conj_log2"], cor_dat, 'cross paradigm')

cor_dat$comparison <- fct_rev(as_factor(cor_dat$comparison))

cor_dat$comparison <- fct_recode(cor_dat$comparison, 
	`ACVS (opt) - MCFT (rl)`  = "ac_propOpt-fg_conj_run_length",
	`ACVS (opt) - MCFT (rl)`  = "ac_propOpt-fg_conj_run_length",
	`SHLS (opt) - MCFT (rl)`  = "ls_prop_hetero-fg_conj_run_length",
	`SHLS (opt) - MCFT (rl)`  = "ls_prop_hetero-fg_conj_run_length",
	`SHLS (opt) - ACVS (sw)`  = "ls_prop_hetero-ac_switchRate",
	`SHLS (opt) - ACVS (opt)` = "ls_prop_hetero-ac_propOpt",
	`ACVS (opt) - ACVS (rt)`  = "ac_propOpt-ac_meanlog2rt",
	`SHLS (opt) - SHLS (rt)`  = "ls_prop_hetero-ls_mean_log_rt",
	`ACVS (sw)`   = "acvs switch rate",
	`ACVS (opt)`  = "acvs prop optimal",
	`SHLS absent (rt)`  = "sh-rt absent",
	`SHLS easy (rt)`  = "sh-rt a versus b easy",
	`SHLS hard(rt)`  = "sh-rt a versus b hard",
	`SHLS (opt)` = "sh - strategy",
	`MCFT (rl) - MCFT (rt)`  = "fg_conj_run_length-fg_conj_log2",
	`MCFT (rl) - MCFT (rt)`  = "fg_conj_run_length-fg_conj_log2",
	`ACVS (rt) - MCFT (rt)` = "ac_rt-fg_conj_log2",          
    `SHLS (rt) - MCFT (rt)` = "ls_mean_log_rt-fg_conj_log2", 
  	`SHLS (rt) - ACVS (rt)`   = "ls_mean_log_rt-ac_meanlog2rt", 
  	`ACVS (rt) - MCFT (rt)`  = "ac_meanlog2rt-fg_conj_log2")


cor_dat$type <- as_factor(cor_dat$type)

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
plt
ggsave("scratch/cor_comparison.pdf", width = 5, height = 7)
ggsave("scratch/cor_comparison.png", width = 5, height = 7)