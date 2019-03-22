# simple power analysis for the line segment / icon search experiment
library(tidyverse)
library(rstan)
library(rethinking)

probs = array()

for (jj in 1:10)
{
	print (ii)
n <- 30
shape1 = 8
shape2 = 3
read_csv("summaryData_75.csv") %>%
	select(score = ls_prop_hetero) %>%
	mutate(stimulus = "lines") %>%
	filter(is.finite(score)) -> d




ohio_sample_mean <- mean(d$score)
sim_line_mean <- 1/ (1 + shape2/shape1)
# generate pretend data for icon search


d <- rbind(
	sample_n(d, n),
	tibble(score = rbeta(n=n, shape1=shape1, shape2=shape2), stimulus = "icons"))

d$x <- ifelse(d$stimulus == "icons", 0.5, -0.5)
X <- cbind(1, d$x)

stan_df <- list(
  N = nrow(d),
  K = ncol(X),
  y = d$score,
  X = X)

m <- stan(
  file = "beta_simple.stan", 
  data = stan_df,
  chains = 4,
  warmup = 2000,
  iter = 5000,
  refresh = 1000
)

samples <- rstan::extract(m)

pred_dat <- tibble(x = c(-0.5, 0.5))

X = as.matrix(pred_dat)
X <- cbind(1, X)

# Look at prior predictions for mmu
mu <- array(0, dim = c(100000, nrow(X)))
for (ii in 1:100000) {
	# generate a random beta from priors
	beta <- as.vector(c(
		rnorm(1, mean=0, sd=.5), 
		rnorm(1, mean=0, sd=.1)
		))
	mu[ii, ] <- plogis(X %*% beta)
}



# Look at posterior predictions for mu

mu <- array(0, dim = c(nrow(samples$beta), nrow(X)))
for (ii in 1:nrow(samples$beta)) {
	mu[ii, ] <- plogis(X %*% samples$beta[ii, ])
}


hpdi <- purrr::map_df(as.tibble(mu), HPDI, prob = 0.90)


mu_df <- tibble(
	stimulus = rep(c("lines", "icons", "difference"), each = 12000),
	mu  = c(mu[,1], mu[,2], mu[,2] - mu[,1]))

mu_df$stimulus <- as_factor(mu_df$stimulus)
mu_df$stimulus <- fct_relevel(mu_df$stimulus, c("lines", "difference", "icons"))

plt <- ggplot(mu_df, aes(x = mu, fill = stimulus))
plt <- plt + geom_density(alpha = 0.5)
plt <- plt + geom_vline(xintercept = 0, linetype = 1)
plt <- plt + geom_vline(xintercept = ohio_sample_mean, linetype = 2)
plt <- plt + geom_vline(xintercept = sim_line_mean, linetype = 2)
plt <- plt + scale_x_continuous(
	"group mean proportion posterior estimates", limits = c(-0.1, 1.0))
# plt <- plt + scale_y_continuous(limits = c(0, 10), expand = c(0, 0))
plt <- plt + scale_fill_viridis_d()
plt <- plt + theme_bw() #+ theme(legend.position="none")
plt
ggsave("scratch/strat_diff.pdf", width = 4, height = 3)

probs[jj] <- mean((mu[,2]-mu[,1])>0)
}