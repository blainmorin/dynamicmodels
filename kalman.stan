data {
  int M; // number of agencies
  int which_pos;
  int Time; // number of years
  int J; // number of measurements
  real z[M, Time, J]; // observations
  int start_time[M];
  int end_time[M];
}
parameters {
  vector[J] alpha;
  vector[J] beta;
  vector<lower=0>[J] sigma;
  real x_start_mean;
  real<lower=0> x_start_sd;
  real<lower=0> change_sd;
}
model {
  for (agency in 1:M) {
    real x;//mean belief about latent process
    real P; //variance belief about latent process
    x = x_start_mean;
    P = x_start_sd ^ 2;
    for (t in start_time[agency]:end_time[agency]) {
      P = P + change_sd;
      for (j in 1:J) {
	real ytilde;
	real S;
	ytilde = z[agency, t, j] - beta[j] * x - alpha[j]; // residual (observed minus predicted x)
	S = (beta[j] ^ 2) * P + sigma[j] ^ 2; // what is beta[j]^2 * P? = uncertainty about the expectation, sigma2 is the variance of the measurement error
	target += - log(sigma) + 0.5 * ytilde ^ 2 / S; // normal log liklihood
	if (j < J) {
	  real K;
	  K = P * beta[j] / S;
	  x += K * ytilde;
	  P *= 1 - K * beta[j]; // c++ synatx, p * this
	}
      }
    }
  }
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 2.5);
  x_start_mean ~ normal(0, 1);
  x_start_sd ~ cauchy(0, 2.5);
  change_sd ~ cauchy(0, 2.5);
}
generated quantities {
  //vector[J] beta_standardized = beta;
  matrix[M, Time] x_samples = rep_matrix(0.0, M, Time);
  int observed_M = sum(end_time) - sum(start_time) + M;
  for (agency in 1:M) {
    real x;
    real P;
    vector[Time] x_mean;
    vector[Time] x_var;
    x = x_start_mean;
    P = x_start_sd ^ 2;
    for (t in start_time[agency]:end_time[agency]) {
      P += change_sd;
      for (j in 1:J) {
	real ytilde;
	real S;
	real K;
	ytilde = z[agency, t, j] - beta[j] * x - alpha[j];
	S = (beta[j] ^ 2) * P + sigma[j] ^ 2;
	K = P * beta[j] / S;
	x = x + K * ytilde;
	P = P * (1 - K * beta[j]);
      }
      x_mean[t] = x;
      x_var[t] = P;
    }
    x_samples[agency, end_time[agency]] = normal_rng(x, sqrt(P));
    for (t in 1:(end_time[agency]-start_time[agency])) {
      real ytilde;
      real K;
      real S;
      x = x_mean[end_time[agency] - t];
      P = x_var[end_time[agency] - t];
      ytilde = x_samples[agency, end_time[agency] - t + 1] - x;
      S = P + change_sd ^ 2;
      K = P / S;
      x += K * ytilde;
      P *= 1 - K;
      x_samples[agency, end_time[agency] - t] = normal_rng(x, sqrt(P));
    }
  }
  x_samples -= sum(x_samples) / observed_M;
  //beta_standardized *= ( beta[which_pos] >= 0.0 ? 1.0 : -1.0 ) * sqrt(sum(square(x_samples)) / (observed_M - 1)); // add ( beta[which_pos] >= 0.0 ? 1.0 : -1.0 )
  x_samples /= sqrt(sum(square(x_samples)) / (observed_M - 1)); //x_samples /= ( beta[which_pos] >= 0.0 ? 1.0 : -1.0 ) * sqrt ........

  
}
