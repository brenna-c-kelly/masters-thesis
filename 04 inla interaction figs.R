

library(ggpubr)
library(ggplot2)
library(RColorBrewer)

fixed <- data.frame(res$summary.fixed)

fixed <- tibble::rownames_to_column(fixed, "coef")

fixed_z <- df %>%
  filter(grepl("z", coef))
fixed_y <- df %>%
  filter(grepl("y", coef))

#load(file = "inla_results.RData")
coefs <- res$summary.fixed

## Intercept
b0 <- coefs[which(rownames(coefs) == "mu_y"), 1]

## violent crime coefficient and prediction values (x-axis)
b1 <- coefs[which(rownames(coefs) == "x_pov_y"), 1]
x1 <- seq(-3, 3, length = 100) ## Violent crime

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
myx$theta <- exp(myx$log_theta)
summary(myx$theta_fix)
## Plots - convert perwhite to factors
#myx$nonwhite_c <- as.factor(myx$nonwhite_c)
myx$x_pov_y <- as.factor(myx$x_pov_y)

summary(myx$theta_fix)

## Plot it
p1 <- ggline(myx, x = "x_asi_y", y = "theta_fix", 
             col = "x_pov_y", numeric.x.axis = TRUE, 
             size = 1.5, plot_type = 'l',
             xlab = "Asian Population (log units)",
             ylab = "Risk Potential", alpha = 0.5) +
  scale_colour_manual(values = rev(brewer.pal(8, "RdYlBu"))) +
  #geom_hline(yintercept = mean(pop_tox$rsei.score), linetype = "dashed") +
  ggtitle("Race/Poverty: All")
p1

