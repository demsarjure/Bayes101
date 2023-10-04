data {
  int n;        // total number of data points
  vector[n] y;  // data points
}

parameters {
  real mu;    // mean
  real sigma; // stdev
}

model {
  y ~ normal(mu, sigma);
}
