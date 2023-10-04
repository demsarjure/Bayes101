data {
  int n;        // total number of data points
  vector[n] y;  // dependent variable
}

parameters {
  real mu;     // mean
  real sigma;  // stdev
  real lambda; // rate
}

model {
  y ~ exp_mod_normal(mu, sigma, lambda);
}
