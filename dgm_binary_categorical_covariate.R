library(tidyverse)
library(zoo) # to compute rolling sum, used in creation of delta-treatment indicator variable

expit <- function(x){
    return(exp(x)/(1+exp(x)))
}

dgm_binary_continuous_covariate <- function(sample_size, total_T) {
  prob_a <- 0.1
  
  df_names <- c("userid", "day", "Y", "A", "S", "S2", "prob_Y", "prob_Y_A0", "prob_A")
  
  dta <- data.frame(matrix(NA, nrow = sample_size * total_T, ncol = length(df_names)))
  names(dta) <- df_names
  
  dta$userid <- rep(1:sample_size, each = total_T)
  dta$day <- rep(1:total_T, times = sample_size)
  
  for (t in 1:total_T) {
    # row index for the rows corresponding to day t for every subject
    row_index <- seq(from = t, by = total_T, length = sample_size)
    
    dta$S[row_index] <- runif(sample_size, 0, 4)
    dta$S2[row_index] <- ifelse(dta$S[row_index] > 1, 1, 0) 
    
    dta$prob_A[row_index] <- rep(prob_a, sample_size)
    dta$A[row_index] <- rbinom(sample_size, 1, dta$prob_A[row_index])
    
    dta$prob_Y_A0[row_index] <- expit(2 * sin(dta$S[row_index]*3))  # no attenuated effects assumed
    dta$prob_Y[row_index] <- expit(2 * sin(dta$S[row_index]*3) + dta$A[row_index] * (1-dta$S[row_index]))  # no attenuated effects assumed
    
    dta$Y[row_index] <- rbinom(sample_size, 1, dta$prob_Y[row_index])
  }
  
  return(dta)
}

dgm_update_continuous_covariate <- function(dat, gam = 1) {
  # gam = attenuation factor. The bigger, the effects last longer.
  dat <- dat %>%
    group_by(userid) %>%
    mutate(cumHaz = cumsum(A)) %>%
    ungroup() %>%
    mutate(prob_Y_trajectory = 
             expit(2 * sin(dat$S*3) +  (2-dat$S)* cumHaz))
  
  dat$Y_trajectory <- rbinom(nrow(dat), 1, dat$prob_Y_trajectory)
  return(dat)
}

dgm_delta <- function(dat, del) {
  del1 = del - 1
  dat <- dat %>%
    group_by(userid) %>%
    mutate(indicator_del_trt = if(del == 1) {1} else {+(dplyr::lead(zoo::rollsum(A, del - 1, fill = NA, align = "left")) == 0)}) %>%
    ungroup() %>%
    mutate(indicator_del_trt = ifelse(is.na(indicator_del_trt), 0, indicator_del_trt)) %>%
    mutate(importance = indicator_del_trt * (1 / (1 - prob_A))^(del - 1)) # this code is valid only when probA is a constant.
  if (del1)
    dat <- # shifting the outcome by 'delta - 1'
    dat %>% 
    group_by(userid) %>% 
    mutate(Y_trajectory = c(Y_trajectory[-(1:del1)], rep(NA, del1)),
           prob_Y_trajectory = c(prob_Y_trajectory[-(1:del1)], rep(NA, del1)))
  dat <- 
    dat %>% 
    dplyr::filter(!is.na(Y_trajectory))
  return(dat)
}