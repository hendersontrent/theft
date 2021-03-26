//
// This Stan program defines a the model for the mixed-effects Bayesian
// logistic regression method for feature-based classification inference.
// This model uses reasonable priors for general-purpose logit
// regression modelling with some advice on prior distributions 
// adapted from Vehtari and Goodrich (2017):
// https://avehtari.github.io/modelselection/diabetes.html"
//

//
// Author: Trent Henderson, 25 March 2021
//

data {
  
  // Define input variables
  
  int<lower=0> N; // Number of observations
  int<lower=0> K; // Number of parameters
  int y[N]; // Group variable
  matrix[N,K] X; // Design matrix of predictors
  int id[N]; // ID variable
}

parameters {
  
  // Define quantities to be estimated
  
  real alpha; // Intercept
  real a[N]; // Random effects intercept
  vector[K] beta; // Regression coefficients
}

model {
  
  // Priors
  
  alpha ~ normal(0,5); // Wide due to uncertainty
  a ~ normal(0,5); // Wide due to uncertainty
  beta ~ student_t(7,0,2.5); // Vehtari and Goodrich (2017)
  
  // Likelihood
  
  for(n in 1:N) {
    y[n] ~ bernoulli(inv_logit(alpha + a[id[n]] + X[n]*beta));
    }
}

generated quantities {
  
  // Instantiate variables to be estimated
  
  vector[N] y_rep; // PPC
  vector[N] log_lik; // Log-likelihood
  
  // Simulate data from the posterior
    
  for(i in 1:N){
    y_rep[i] = bernoulli_rng(inv_logit(alpha + a[id[i]] + X[i]*beta));
  }
  
  // Log-likelihood posterior
  
  for(i in 1:N){
    log_lik[i] = bernoulli_logit_lpmf(y[i] | alpha + a[id[i]] + X[i]*beta);
  }
}

