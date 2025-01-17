### Plot output for paper ###
source("beta_hat_trajectory.R")

label_positions <- c(beta_hats$slope[1]+.04,
                     beta_hats$slope[2]-.04,
                     beta_hats$slope[3]+.04,
                     beta_hats$slope[4]+.03,
                     beta_hats$slope[5]-.03,
                     beta_hats$slope[6]-.03)

# Trajectory Plot
plot(beta_hats$intercept, beta_hats$slope,
     xlab = TeX("$\\hat{\\beta}_0$"),
     ylab = "",
     ylim = c(min(beta_hats$slope)-.03, max(beta_hats$slope)+.08),
     las = 1)
title(ylab = TeX("$\\hat{\\beta}_1$"), line = 2.5)
lines(beta_hats$intercept, beta_hats$slope)
text(x = c(beta_hats$intercept[1:4], beta_hats$intercept[5]-.015, beta_hats$intercept[6]+.015), y = label_positions, labels = c(1, 2, 3, 4, 5, 6))

# Optimal Treatment Regime Plot
delta <- c("1", "1", "2", "2", "3", "3", "4", "4", "5", "5", "6", "6")
type <- c(rep(c("Trt 1", "Trt 2"), 3), rep(c("Trt 1 - Flip", "Trt 2 - Flip"), 3))
value <- c(1.464, 2.536, 1.638, 2.362, 3.460, .540, 1.89, 2.11, 3.09, .91, 3.96, .04) # beta_hats result from original beta_hat_trajectory.R file setup
dat <- data.frame(delta, type, value)

ggplot(dat, aes(fill=type, y=value, x=delta)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Delta") +
  ylab("Covariate S") +
  labs(fill = "") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"), breaks = c("Trt 1", "Trt 2"), labels = c("Treatment \nRecommended", "Treatment \nNot Recommended"))

#rgl::plot3d(x = beta_hats$intercept,
#            y = beta_hats$slope, 
#            z = 1:delta, 
#            type = "s", 
#            xlab = TeX("$\\hat{\\beta}_0$"), 
#            ylab = TeX("$\\hat{\\beta}_1$"), 
#            zlab = TeX("Delta"))
