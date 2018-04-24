library(tidyverse)
library(rethinking)
library(ggridges)

figXn <- 6
figYn <- 4

dat <- as.data.frame(read_csv("summaryData.csv"))
dat <- dat[complete.cases(dat),]


bayes_R2 <- function(x, y) {

	x <- scale(x)
	y <- scale(y)
	df <- data.frame(x = x, y = y)

	m <- map2stan(
		alist(
			y ~ dnorm(mu, sigma),
			mu <- a + b*x,
			a ~ dnorm(0, 1),
			b ~ dnorm(0, 1),
			sigma ~ dcauchy(0, 1)),
		start = list(a = mean(y), b = 0, sigma = sd(y)),
		data = df)

	print(precis(m))

	post <- extract.samples(m)

	R2 = array(NA, 1000)
	for (ii in 1:1000) {
		R2[ii] <- 1 - sum((post$a[ii]+ post$b[ii] * x - y)^2) /  sum((y - mean(y))^2)
	}

	return(R2)

}

 summary(lm(scale(dat$ls_mean_log_rt) ~ scale(dat$fg_conj_log2)))

R2_ls_ac <- bayes_R2(dat$ls_mean_log_rt, dat$ac_meanlog2rt)

R2_ls_fgf <- bayes_R2(dat$ls_mean_log_rt, dat$fg_feat_log2)

R2_ls_fgc <- bayes_R2(dat$ls_mean_log_rt, dat$fg_conj_log2)

R2_ac_fgf <- bayes_R2(dat$ac_meanlog2rt, dat$fg_conj_log2)

R2_ac_fgc <- bayes_R2(dat$ac_meanlog2rt, dat$fg_conj_log2)

results <- tibble(comparison = rep(c("ls v ac", "ls v fg feat", "ls v fg conj", "ac v fg feat", "ac v fg conj"), each = 1000),
	R2 = c(R2_ls_ac, R2_ls_fgf, R2_ls_fgc, R2_ac_fgf, R2_ac_fgc ))

plt <- ggplot(results, aes(x = R2, y = comparison)) + geom_density_ridges()
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous(limits = c(-0.3, 0.1), expand = c(0, 0))
plt <- plt + scale_y_discrete(expand = c(0, 0.5))
ggsave("ridge_plot_cor_rt.pdf")
cor.test(dat$ls_mean_log_rt, dat$ac_meanlog2rt)
cor.test(dat$ls_mean_log_rt, dat$fg_feat_log2)
cor.test(dat$ls_mean_log_rt, dat$fg_conj_log2)
cor.test(dat$ac_meanlog2rt, dat$fg_conj_log2)
cor.test(dat$ac_meanlog2rt, dat$fg_conj_log2)