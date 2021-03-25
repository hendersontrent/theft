//
// This Stan program defines a the model for the mixed-effects Bayesian
// logistic regression method for feature-based classification.
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
  int y[N]; // Outcome variable of remoteness category
  matrix[N,K] X; // Design matrix of predictors
}

parameters {
  
  // Define quantities to be estimated
  
  real alpha; // Intercept
  vector[K] beta; // Regression coefficients
}

transformed parameters {
  
  // Instantiate variable to hold real output
  
  vector[N] eta;
  
  // Fit linear model
  
  eta = alpha + beta*X;
}

model {
  
  // Priors
  
  alpha ~ normal(0,5); // Wide due to uncertainty
  beta ~ student_t(7,0,2.5); // Vehtari and Goodrich (2017)
  
  // Likelihood
  
  y ~ bernoulli_logit(eta);
}

generated quantities {
  
  // Simulate data from the posterior
    
  vector[N] y_rep;
  
  // Log-likelihood posterior
  
  vector[N] log_lik;
  
  for(i in 1:N){
    y_rep[i] = bernoulli_rng(inv_logit(eta[i]));
  }
  
  for(i in 1:N){
    log_lik[i] = bernoulli_logit_lpmf(y[i] | eta[i]);
  }
}

