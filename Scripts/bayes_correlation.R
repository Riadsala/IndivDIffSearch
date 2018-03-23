library(tidyverse)
library(rethinking)

figXn <- 6
figYn <- 4

dat <- as.data.frame(read_csv("summaryData.csv"))
dat <- dat[complete.cases(dat),]



bayes_r <- function(x, y) {

	x <- scale(x)
	y <- scale(y)
	df <- data.frame(x = x, y = y)

	m <- map2stan(
		alist(
			y ~ dnorm(mu, sigma),
			mu <- a + b*x,
			a ~ dnorm(0, 10),
			b ~ dnorm(0, 10),
			sigma ~ dcauchy(0, 1)),
		start = list(a = mean(y), b = 0, sigma = sd(y)),
		data = df)

	post <- extract.samples(m)

	R2 = array(NA, 1000)
	for (ii in 1:1000) {
		R2[ii] <- 1 - sum((post$a[ii]+ post$b[ii] * x - y)^2) /  sum((y - mean(y))^2)

	}

	return(R2)

}


summary(lm(dat$ls_mean_log_rt~ dat$ac_meanlog2rt))

R2 <- bayes_r(dat$ls_mean_log_rt, dat$ac_meanlog2rt)
