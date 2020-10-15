dgm_binary_categorical_covariate <- function(sample_size, total_T) {
  # same DGM as dgm_binary above, but faster
  
  baseline_Y_S0 <- 0.2
  baseline_Y_S1 <- 0.5
  baseline_Y_S2 <- 0.4
  
  beta_0 <- -0.3
  beta_1 <- 0.3
  
  prob_a <- 0.2
  
  df_names <- c("userid", "day", "Y", "A", "S", "S2", "prob_Y", "prob_Y_A0", "prob_A")
  
  dta <- data.frame(matrix(NA, nrow = sample_size * total_T, ncol = length(df_names)))
  names(dta) <- df_names
  
  dta$userid <- rep(1:sample_size, each = total_T)
  dta$day <- rep(1:total_T, times = sample_size)
  
  for (t in 1:total_T) {
    # row index for the rows corresponding to day t for every subject
    row_index <- seq(from = t, by = total_T, length = sample_size)
    
    
    dta$S[row_index] <- sample(c(0,1,2), sample_size, replace = TRUE)
    dta$S2[row_index] <- ifelse(dta$S[row_index] == 2, 1, 0) 
    dta$prob_A[row_index] <- rep(prob_a, sample_size)
    dta$A[row_index] <- rbinom(sample_size, 1, dta$prob_A[row_index])
    dta$prob_Y_A0[row_index] <- ifelse(dta$S[row_index] == 0, baseline_Y_S0, 
                                       ifelse(dta$S[row_index] == 1, baseline_Y_S1, baseline_Y_S2))
    dta$prob_Y[row_index] <- dta$prob_Y_A0[row_index] * exp(dta$A[row_index] * (beta_0 + beta_1 * dta$S[row_index]))
    dta$Y[row_index] <- rbinom(sample_size, 1, dta$prob_Y[row_index])
  }
  
  return(dta)
}

# Takes in the dataset created by dgm_binary_categorical_covariate() and updates prob_Y to reflect trajectory
# Regenerates outcome based on new values of prob_Y
dgm_update <- function(dat, gam = 1) {
  beta_0 <- -0.3 #unused at the moment
  beta_1 <- 0.3 #unused at the moment
  
  # dat <- dat %>%
  #   group_by(userid) %>%
  #   mutate(d = (NA^!cummax(A)) * sequence(table(cumsum(A)))) %>%
  #   ungroup() %>%
  #   mutate(prob_Y_trajectory = ifelse(A == 1 | is.na(d), prob_Y, prob_Y_A0 * exp((1 / (gam*d)) * (beta_0 + beta_1*S))))
  
  dat <- dat %>%
    group_by(userid) %>%
    mutate(d = (NA^!cummax(A)) * sequence(table(cumsum(A)))) %>%
    ungroup() %>%
    mutate(prob_Y_trajectory = ifelse(S==0, ifelse(is.na(d), expit(-2), expit(-2 - 1/d)),
                                      ifelse(S==1, ifelse(is.na(d), expit(-1), expit(-1)), 
                                             ifelse(S==2, ifelse(is.na(d), expit(-1.5), expit(-1.5 + 1/d)), NA))))
  
  dat$Y_trajectory <- rbinom(nrow(dat), 1, dat$prob_Y_trajectory)
  return(dat)
}