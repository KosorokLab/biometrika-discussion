library(tidyverse)

source("dgm_binary_categorical_covariate.R")
source("estimators.R")

set.seed(2022)

control_vars <- "S"
moderator_vars <- "S"
delta <- 6

dta_orig <- data_generating_process(10000, 10)
dta_updated <- dgm_update(dta_orig)

beta_hats <- matrix(nrow = delta, ncol = 2, data = NA)
colnames(beta_hats) <- c("intercept", "slope")

for (i in 1:delta) {
  print(paste0("Delta: ", i))
  dta_delta <- as.data.frame(dgm_delta(dta_updated, del = i))
  fit <- weighted_centered_least_square(
    dta = dta_delta,
    id_varname = "userid",
    decision_time_varname = "day",
    treatment_varname = "A",
    outcome_varname = "Y_trajectory",
    control_varname = control_vars,
    importance_varname = "importance",
    moderator_varname = moderator_vars,
    rand_prob_varname = "prob_A",
    rand_prob_tilde_varname = NULL,
    rand_prob_tilde = 0.2,
    estimator_initial_value = NULL
  )
  beta_hats[i, ] <- fit$beta_hat
}

beta_hats <- as.data.frame(beta_hats)

print(beta_hats)

with(beta_hats, plot(intercept, slope))
with(beta_hats, lines(intercept, slope))