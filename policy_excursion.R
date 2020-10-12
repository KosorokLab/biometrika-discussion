# policy excursion effect
# Hunyong Cho

library(tidyverse)
library(latex2exp)
library(ggplot2)
library(cowplot)

source("dgm_binary_categorical_covariate.R")
source("estimators.R")

nrep = 10
control_vars <- "S"
moderator_vars <- "S"
delta <- c(1,5,10, 20)

# skeleton
beta_hats_policy <- beta_hats_fixed <- 
  array(NA, dim = c(length(delta), 3, nrep), 
        dimnames = list(delta, c("beta0", "beta1", "rule"), 1:nrep))

# initialize args.
args <- list(
  dta = NULL,
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

for (k in 1:nrep) {
  cat("\nreplicate ", k, "\n")
  set.seed(k)
  
  dta_orig <- dgm_binary_continuous_covariate(1000, 100)
  dta_updated <- dgm_update_continuous_covariate(dta_orig, gam = 4)
  
  for (i in seq_along(delta)) {
    cat(paste0("  Delta = ", delta[i], " "))
    args$dta <- as.data.frame(dgm_delta(dta_updated, del = delta[i]))
    
    # fixed excursion
    fit <- do.call(weighted_centered_least_square, args)
    beta_hats_fixed[i, 1:2, k] <- fit$beta_hat
    
    # policy excursion
    args$dta$importance = 1
    fit <- do.call(weighted_centered_least_square, args)
    beta_hats_policy[i, 1:2, k] <- fit$beta_hat
  }
}
beta_hats_fixed[, 3, ]  = - beta_hats_fixed[, 1, ] /  beta_hats_fixed[, 2, ]
beta_hats_policy[, 3, ] = - beta_hats_policy[, 1, ] /  beta_hats_policy[, 2, ]

beta_hats_fixed[,1:2,10]
beta_hats_policy[,1:2,10]
apply(beta_hats_fixed, c(1:2), mean, na.rm = TRUE)
apply(beta_hats_policy, c(1:2), mean, na.rm = TRUE)
apply(beta_hats_fixed, c(1:2), sd, na.rm = TRUE)
apply(beta_hats_policy, c(1:2), sd, na.rm = TRUE)

# reformulating the results
result <-
  data.frame(beta0 = rep(NA, length(delta) * nrep * 2), beta1 = NA, Delta = delta, rep = rep(1:nrep, each = length(delta)), 
             contrast = rep(c("fixed", "policy"), each = length(delta) * nrep)) %>% 
  mutate(Delta = factor(Delta, levels = delta, labels = c(paste0("\u0394 = ", delta[1]), delta[-1])))
result[,"beta0"] <- c(beta_hats_fixed[, "beta0",], beta_hats_policy[, "beta0",])
result[,"beta1"] <- c(beta_hats_fixed[, "beta1",], beta_hats_policy[, "beta1",])

# summary stats of the results
result.summary <-
  result %>% 
  group_by(Delta, contrast) %>% 
  summarise(beta0_mean = mean(beta0),
            beta1_mean = mean(beta1),
            beta0_sd = sd(beta0),
            beta1_sd = sd(beta1),
            sd = paste0("se = (", beta0_sd %>% round(3), ", ", beta1_sd %>% round(3),")")) %>% 
  ungroup

result %>% 
  group_by(Delta, contrast) %>% summarise(range0 = range(beta0), range1 = range(beta1))
result$beta0 %>% range
result$beta1 %>% range

p1 <-
  ggplot(result, aes(beta0, beta1, col = contrast)) +
  stat_density2d(aes(fill=..level..,alpha=..level..), contour_var = "ndensity") +
  geom_point() +
  facet_grid(ifelse(contrast == "fixed",  "fixed sequences", "stochastic sequences") ~ Delta) +
  xlab(TeX("$\\hat{\\beta}_0$")) + ylab(TeX("$\\hat{\\beta}_1$")) +
  scale_x_continuous(breaks = c(-0.1, 0.0, 0.1), labels = c("-0.1", "0.0", "0.1")) +
  theme_bw() + guides(color = FALSE, alpha = FALSE, fill = FALSE) +
  geom_point(data = result.summary, mapping = aes(beta0_mean, beta1_mean), color = "black", shape = "square")
  # geom_text(data = result.summary, mapping = aes(beta0_mean + beta0_sd, beta1_mean + beta1_sd, label = sd), 
  #           color = "black", size = 2)
  
  sd.max <- result.summary$beta0_sd %>% max # 0.09
p2 <- 
  ggplot(result.summary, aes(Delta, beta0_sd, col = contrast, group = contrast)) + 
  geom_point() + geom_line() + ylab(TeX("$se(\\hat{\\beta}_0)$")) + xlab("") + ylim(c(0, sd.max * 1.02)) +
  theme_bw() + guides(color = FALSE) +
  annotate(geom = "text", x = 2, y = 0.048, label = "fixed sequences", col = "#F8766D") +
  annotate(geom = "text", x = 3, y = 0.018, label = "stochastic sequences", col = "#00BFC4")

p3 <- 
  plot_grid(p1, p2, align = "h", nrow = 1, ncol = 2, rel_widths = c(2, 1),
            labels = list("A.", "B."), 
            vjust = 1.2, hjust = -0.8)
save_plot("Fig2.png", p3, base_width = 10, base_height = 3.5)
