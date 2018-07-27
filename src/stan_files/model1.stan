data {
  int<lower=1> N; // total number of samples
  int count[N]; // observed counts
  real log_bsi[N]; // observed counts
}

parameters {
 real lambda; // rate (on a log scale)
}

model {
  for (i in 1:N) {
    count[i] ~ poisson_log(log_bsi[i] + lambda);
  }
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = poisson_log_lpmf(count[i] | log_bsi[i] + lambda);
  }
}
