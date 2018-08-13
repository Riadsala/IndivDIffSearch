library(tidyverse)
library(forcats)

figXn <- 3.5
figYn <- 2.5

dat <- read_csv("summaryData.csv")


cor_dat <- tibble(
	comparison = c(
		"test-retest - ls-rt absent", 
		"test-retest - ls-rt a versus b hard", 
		"test-retest - ls-rt a versus b easy",
		"test-retest - acvs prop optimal",
		"test-retest - acvs switch rate"),
	estimate = c(0,0,0,0.83, 0.77),
	lower = c(0.65, 0.66, 0.56, 0.72, 0.63),
	upper = c(0.86, 0.87, 0.82, 0.90, 0.87))

add_r_95 <- function(x1, x2, df) {
	ci95 <- cor.test(x1[[1]], x2[[1]])

	df <- bind_rows(df, 
		tibble(
			comparison = paste(names(x1), names(x2), sep = "-"),
			estimate = ci95$estimate,
			lower = ci95$conf.int[1],
			upper = ci95$conf.int[2]))

	return(df)
}

cor_dat <- add_r_95(dat[3], dat[8], cor_dat)


cor_dat$comparison <- as_factor(cor_dat$comparison)

plt <- ggplot(cor_dat, aes(x = comparison, ymin = lower, ymax = upper))
plt <- plt + geom_errorbar()
plt <- plt + geom_hline(yintercept = 0)
plt <- plt + scale_y_continuous(limits = c(-0.5, 1), expand = c(0,0))
plt <- plt + theme_bw() 
plt <- plt + theme(
	axis.title.x = element_blank(),
	axis.text.x = element_text(angle = 60, hjust = 1))
ggsave("scratch/cor_comparison.pdf", width = 4, height = 4)