//
// This Stan program defines a the model for the Gaussian
// Process regression classification option. The model follows
// advice from MC Stan itself:
// https://mc-stan.org/docs/2_22/stan-users-guide/fit-gp-section.html
// and from:
// https://luiarthur.github.io/TuringBnpBenchmarks/gpclassify

//
// Author: Trent Henderson, 4 April 2021
//

data {
  int<lower=0> N; // Number of observations
  int<lower=0> P; // Number of parameters
  int<lower=0, upper=1> y[N]; // Group variable
  matrix[N, P] X; // Design matrix
  real<lower=0> eps;
}

parameters {
  real<lower=0> rho;   // Range parameter in GP covariance fn
  real<lower=0> alpha; // Covariance scale parameter in GP covariance fn
  vector[N] eta;
  real beta;
}

transformed parameters {
  vector[N] f;
  {
    matrix[N, N] K; // GP covariance matrix
    matrix[N, N] LK; // cholesky of GP covariance matrix
    row_vector[N] row_x[N];
    
    // Using exponential quadratic covariance function
    
    for (n in 1:N) {
      row_x[n] = to_row_vector(X[n, :]);
    }
    K = cov_exp_quad(row_x, alpha, rho); 

    // Add small values along diagonal elements for numerical stability
    
    for (n in 1:N) {
        K[n, n] = K[n, n] + eps;
    }

    // Cholesky of K (lower triangle)
    
    LK = cholesky_decompose(K); 
  
    f = LK * eta;
  }
}

model {
  
  // Priors
  
  rho ~ inv_gamma(5, 5);
  alpha ~ std_normal();
  eta ~ std_normal();
  beta ~ std_normal();
 
  // Model
  
  y ~ bernoulli_logit(beta + f);
}
