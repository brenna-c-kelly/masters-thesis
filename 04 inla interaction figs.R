
library(grid)
library(ggpubr)
library(ggplot2)
library(RColorBrewer)

fixed <- data.frame(res$summary.fixed)

fixed <- tibble::rownames_to_column(fixed, "coef")

fixed_z <- df %>%
  filter(grepl("z", coef))
fixed_y <- df %>%
  filter(grepl("y", coef))

###   Gamma interaction (only credible one, asian:poverty)
coefs <- res$summary.fixed

## Intercept
b0 <- coefs[which(rownames(coefs) == "mu_y"), 1]

## violent crime coefficient and prediction values (x-axis)
b1 <- coefs[which(rownames(coefs) == "x_pov_y"), 1]
x1 <- seq(-3, 2, length = 100) ## Violent crime

## percent white coefficient and prediction values (colors)
b2 <- coefs[which(rownames(coefs) == "x_asi_y"), 1]
x2 <- seq(-3, 3) ## PerWhite

## interaction coefficient
b3 <- coefs[which(rownames(coefs) == "x_pov_y:x_asi_y"), 1]

## Prediction grid
myx <- expand.grid(x_asi_y = x1, x_pov_y = x2)

## Expected log counts for grid
myx$log_theta <- b0 + b1 * myx$x_pov_y + b2 * myx$x_asi_y +
  b3 * (myx$x_pov_y * myx$x_asi_y)

myx$theta_fix <- exp(myx$log_theta)# / (1 + exp(myx$log_theta))

## Inverse link transform
#myx$theta <- exp(myx$log_theta)
summary(myx$theta_fix)
## Plots - convert perwhite to factors
#myx$nonwhite_c <- as.factor(myx$nonwhite_c)
myx$x_pov_y <- as.factor(myx$x_pov_y)

summary(myx$theta_fix)

## Plot it
asian_poverty <- ggline(myx, x = "x_asi_y", y = "theta_fix", 
             col = "x_pov_y", numeric.x.axis = TRUE, 
             size = 1.5, plot_type = 'l',
             xlab = "Standardized Percent Asian Population",
             ylab = "Risk Potential", alpha = 0.5) +
  scale_colour_manual(values = rev(brewer.pal(8, "RdYlBu")),) +
  labs(color = "Standardized \nPercent Below \nPoverty Line") +
  #geom_hline(yintercept = mean(pop_tox$rsei.score), linetype = "dashed") +
  #ggtitle("Race/Poverty: All") +
  theme(legend.position = "right")
asian_poverty




###     Binomial interactions
figs_z = vector("list", length = 6)
z_intx <- c("x_aia_z", "x_asi_z",
            "x_nhp_z", "x_tom_z", "x_oth_z",
            "x_his_z")
for(i in z_intx) {
  b0 <- coefs[which(rownames(coefs) == "mu_z"), 1]
  
  ## violent crime coefficient and prediction values (x-axis)
  b1 <- coefs[which(rownames(coefs) == "x_pov_z"), 1]
  x1 <- seq(-4, 2, length = 100) ## Violent crime
  
  ## percent white coefficient and prediction values (colors)
  b2 <- coefs[which(rownames(coefs) == i), 1]
  x2 <- seq(-4, 2) ## PerWhite
  
  ## interaction coefficient
  b3 <- coefs[which(rownames(coefs) == paste("x_pov_z:", i, sep = "")), 1]
  
  ## Prediction grid
  myx <- expand.grid(i = x1, x_pov_z = x2)
  
  str(myx)
  ## Expected log counts for grid
  myx$log_theta <- b0 + b1 * myx$x_pov_z + b2 * myx$i +
    b3 * (myx$x_pov_z * myx$i)
  
  myx$theta_fix <- exp(myx$log_theta) / (1 + exp(myx$log_theta))
  
  ## Inverse link transform
  #myx$theta <- exp(myx$log_theta)
  summary(myx$theta_fix)
  ## Plots - convert perwhite to factors
  #myx$nonwhite_c <- as.factor(myx$nonwhite_c)
  myx$x_pov_z <- as.factor(myx$x_pov_z)
  
  summary(myx$theta_fix)
  #races <- c("American Indian/Alaska Native",
  #           "Asian", "Native Hawaiian/Pacific Islander",
  #           "Two or More Races", "Some Other Race", "Hispanic/Latinx")
  ## Plot it
  
  myx$sig <- ifelse(i %in% c("x_bla_z", "x_his_z"), 1, 0.5)
  race_poverty <- ggline(myx, x = "i", y = "theta_fix", 
                        col = "x_pov_z", numeric.x.axis = TRUE, 
                        size = 1.5, plot_type = 'l',
                        alpha = sig,
                        xlab = paste("Percent", i),
                        ylab = "Probability of Being Polluted", alpha = 0.5) +
    scale_colour_manual(values = rev(brewer.pal(8, "RdYlBu")),) +
    labs(color = "Percent Below \nPoverty Line") +
    #geom_hline(yintercept = mean(pop_tox$rsei.score), linetype = "dashed") +
    #ggtitle("Race/Poverty: All") +
    theme(legend.position = "right")
  race_poverty
  figs_z[[i]] <- race_poverty
}


ggarrange(figs_z$x_oth_z,
          legend = "right",
          common.legend = TRUE)



figs_y = vector("list", length = 6)
y_intx <- c("x_aia_y", "x_asi_y",
            "x_nhp_y", "x_tom_y", "x_oth_y",
            "x_his_y")
for(i in y_intx) {
  b0 <- coefs[which(rownames(coefs) == "mu_y"), 1]
  
  ## violent crime coefficient and prediction values (x-axis)
  b1 <- coefs[which(rownames(coefs) == "x_pov_y"), 1]
  x1 <- seq(-4, 2, length = 100) ## Violent crime
  
  ## percent white coefficient and prediction values (colors)
  b2 <- coefs[which(rownames(coefs) == i), 1]
  x2 <- seq(-4, 2) ## PerWhite
  
  ## interaction coefficient
  b3 <- coefs[which(rownames(coefs) == paste("x_pov_y:", i, sep = "")), 1]
  
  ## Prediction grid
  myy <- expand.grid(i = x1, x_pov_y = x2)
  
  str(myy)
  ## Expected log counts for grid
  myy$log_theta <- b0 + b1 * myy$x_pov_y + b2 * myx$i +
    b3 * (myy$x_pov_y * myx$i)
  
  myy$theta_fix <- exp(myy$log_theta) / (1 + exp(myy$log_theta))
  
  ## Inverse link transform
  #myx$theta <- exp(myx$log_theta)
  summary(myx$theta_fix)
  ## Plots - convert perwhite to factors
  #myx$nonwhite_c <- as.factor(myx$nonwhite_c)
  myy$x_pov_y <- as.factor(myy$x_pov_y)
  
  summary(myy$theta_fix)
  #races <- c("American Indian/Alaska Native",
  #           "Asian", "Native Hawaiian/Pacific Islander",
  #           "Two or More Races", "Some Other Race", "Hispanic/Latinx")
  ## Plot it
  
  #myx$sig <- ifelse(i %in% c("x_bla_y", "x_his_y"), 1, 0.5)
  race_poverty <- ggline(myy, x = "i", y = "theta_fix", 
                         col = "x_pov_y", numeric.x.axis = TRUE, 
                         size = 1.5, plot_type = 'l',
                         alpha = sig,
                         xlab = paste("Percent", i),
                         ylab = "Change in Risk Potential", alpha = 0.5) +
    scale_colour_manual(values = rev(brewer.pal(8, "RdYlBu")),) +
    labs(color = "Percent Below \nPoverty Line") +
    #geom_hline(yintercept = mean(pop_tox$rsei.score), linetype = "dashed") +
    #ggtitle("Race/Poverty: All") +
    theme(legend.position = "right")
  race_poverty
  figs_y[[i]] <- race_poverty
}
figs_y$x_tom_y

ggarrange(figs_y$x_tom_y,
          legend = "right",
          common.legend = TRUE)




