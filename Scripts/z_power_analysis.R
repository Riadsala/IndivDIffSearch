library(tidyverse)
library(rstan)
library(rethinking)

# simple power analysis for the line segment / icon search experiment
# we will fit beta distributions to the proportion data

# baseline data taken from Clarke et al (2018)
read_csv("summaryData_75.csv") %>%
	select(score = ls_prop_hetero) %>%
	mutate(stimulus = "lines") %>%
	filter(is.finite(score)) -> d

clarke2018_sample_mean <- mean(d$score)
print('***************')
print(paste("full sample mean = ", format(clarke2018_sample_mean, digits = 3)))
print(paste("full sample sd = ", format(sd(d$score), digits = 3)))

# assume new proportions come from a beta distribution 
alpha <- 8
beta <- 3

sim_mean <- 1/ (1 + beta/alpha)
sim_sd  <-  sqrt((alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)))
print(paste("full sample mean = ", format(sim_mean, digits = 3)))
print(paste("full sample sd = ", format(sim_sd, digits = 3)))
print('***************')


# in each simulation, we will sample 30 participants
n <-30

# create empty array to put probabilities in
probs = array()

for (jj in 1:100)
{

	# generate pretend data for icon search
	d_sim <- rbind(
		sample_n(d, n),
		tibble(
			score = rbeta(n=n, shape1=alpha, shape2=beta), 
			stimulus = "icons"))

	# create dummy variable
	d_sim$x <- ifelse(d_sim$stimulus == "icons", 0.5, -0.5)
	X <- cbind(1, d_sim$x)

	# create list of data to pass to Sta
	stan_df <- list(
	  N = nrow(d_sim),
	  K = ncol(X),
	  y = d_sim$score,
	  X = X)

	# fit model in Stan
	m <- stan(
	  file = "beta_simple.stan", 
	  data = stan_df,
	  chains = 1,
	  warmup = 2000,
	  iter = 5000,
	  refresh = 1000
	)

	# exract samples 
	samples <- rstan::extract(m)

	# generate posterior predictions from model
	pred_dat <- tibble(x = c(-0.5, 0.5))

	X = as.matrix(pred_dat)
	X <- cbind(1, X)

	mu <- array(0, dim = c(nrow(samples$beta), nrow(X)))
	for (ii in 1:nrow(samples$beta)) {
		mu[ii, ] <- plogis(X %*% samples$beta[ii, ])
	}

	# compute prob(group mean of group2 is larger than group1 | data)
	probs[jj] <- mean((mu[,2]-mu[,1])>0)

	rm(m, d_sim, mu, samples, pred_dat, X)
}

print(mean(probs > 0.95))