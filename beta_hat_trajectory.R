library(tidyverse)
library(latex2exp)

source("dgm_binary_categorical_covariate.R")
source("estimators.R")

set.seed(4)
control_vars <- "S"
moderator_vars <- "S"
delta <- 6

dta_orig <- dgm_binary_continuous_covariate(1000, 10)
dta_updated <- dgm_update_continuous_covariate(dta_orig)

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

### RESULT OUTPUT ###

print(beta_hats)

plot(beta_hats$intercept, beta_hats$slope,
     xlab = TeX("$\\hat{\\beta}_0$"),
     ylab = "",
     ylim = c(min(beta_hats$slope)-.03, max(beta_hats$slope)+.08),
     las = 1)
title(ylab = TeX("$\\hat{\\beta}_1$"), line = 2.5)
lines(beta_hats$intercept, beta_hats$slope)
text(x = beta_hats$intercept,
     y = beta_hats$slope+.05, labels = 1:delta)

# 3D plot of trajectory
#rgl::plot3d(x = beta_hats$intercept,
#            y = beta_hats$slope, 
#            z = 1:delta, 
#            type = "s", 
#            xlab = TeX("$\\hat{\\beta}_0$"), 
#            ylab = TeX("$\\hat{\\beta}_1$"), 
#            zlab = TeX("Delta"))
