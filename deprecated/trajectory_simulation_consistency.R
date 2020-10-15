compute_result_beta <- function(beta_true, beta, beta_se, beta_se_adjusted, moderator_vars, control_vars, significance_level,
                                na.rm = FALSE) {
  
  beta_true_array <- array(NA, dim = dim(beta), dimnames = dimnames(beta))
  for (ind1 in 1:dim(beta_true_array)[1]) {
    for (ind3 in 1:dim(beta_true_array)[3]) {
      beta_true_array[ind1, , ind3] <- beta_true
    }
  }
  
  p <- length(moderator_vars) + 1
  q <- length(control_vars) + 1
  
  bias <- apply(beta - beta_true_array, c(1,2), mean, na.rm = na.rm)
  sd <- apply(beta, c(1,2), sd, na.rm = na.rm)
  rmse <- apply(beta - beta_true_array, c(1,2), function(v) sqrt(mean(v^2, na.rm = na.rm)))
  
  critical_factor <- qnorm(1 - significance_level/2)
  ci_left <- beta - critical_factor * beta_se
  ci_right <- beta + critical_factor * beta_se
  coverage_prob <- apply((ci_left < beta_true_array) & (ci_right > beta_true_array),
                         c(1,2), mean, na.rm = na.rm)
  
  critical_factor_adj <- qt(1 - significance_level/2, df = sample_size - 1 - q)
  ci_left_adj <- beta - critical_factor_adj * beta_se_adjusted
  ci_right_adj <- beta + critical_factor_adj * beta_se_adjusted
  coverage_prob_adj <- apply((ci_left_adj < beta_true_array) & (ci_right_adj > beta_true_array),
                             c(1,2), mean, na.rm = na.rm)
  
  return(list(bias = bias, sd = sd, rmse = rmse, coverage_prob = coverage_prob, coverage_prob_adjusted = coverage_prob_adj))
}

source("dgm_binary_categorical_covariate.R")
source("estimators.R")
source("estimators_robust_adhocery.R")

library(foreach)
library(doMC)
library(doRNG)

dat_orig <- dgm_binary_categorical_covariate(30, 30)
dat <- dgm_update(dat_orig)

control_vars <- "S"
moderator_vars <- c()

result_df_collected <- data.frame()

result_list <- list()

for (i in 1:5) {
  dat_delta <- as.data.frame(dgm_delta(dat, del = i))
  result_list[[i]] <- weighted_centered_least_square(
    dta = dat_delta,
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
}