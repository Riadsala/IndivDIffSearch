data {
  int<lower=1> N; // number of data points
  int<lower=1> K; // number of predictors
  vector<lower=0,upper=1>[N] y; // dependant variable 
  matrix[N,K] X; // predictors 
}

parameters {
  vector[K] beta; //coefs for mu
  vector[K] gamma; // coefs for phi
}

transformed parameters{
  vector<lower=0,upper=1>[N] mu;    // transformed linear predictor for mean of beta distribution
  vector<lower=0>[N] phi;           // transformed linear predictor for precision of beta distribution
  vector<lower=0>[N] A;             // parameter for beta distn
  vector<lower=0>[N] B;             // parameter for beta distn

  for (i in 1:N) {
    mu[i]  = inv_logit(X[i,] * beta);   
    phi[i] = exp(X[i,] * gamma);
  }

  A = mu .* phi;
  B = (1.0 - mu) .* phi;
}

model {
  // priors
  beta[1] ~ normal(0, 0.5);
  beta[2] ~ normal(0, 0.5);

  gamma[1] ~ normal(0, 1);
  gamma[2] ~ normal(0, 1);
  // likelihood
  y ~ beta(A, B);
}
