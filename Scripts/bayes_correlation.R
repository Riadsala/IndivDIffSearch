library(tidyverse)
library(rethinking)

figXn <- 6
figYn <- 4

dat <- as.data.frame(read_csv("summaryData.csv"))
dat <- dat[complete.cases(dat),]

m <- map(
	alist(
		ac_meanlog2rt ~ dnorm(mu, sigma),
		mu <- a + b * ls_mean_log_rt,
		a ~ dnorm(0, 10),
		b ~ dnorm(0, 10),
		sigma ~ dcauchy(0, 1)),
	start = list(a = mean(dat$ac_meanlog2rt), b = 0, sigma = sd(dat$ac_meanlog2rt)),
	data = dat)

post <- extract.samples(m)