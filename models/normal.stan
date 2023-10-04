data {
  int n;        // total number of data points
  vector[n] y;  // data points
}

parameters {
  real mu;              // mean
  real<lower=0> sigma;  // stdev
}

model {
  y ~ normal(mu, sigma);
}
