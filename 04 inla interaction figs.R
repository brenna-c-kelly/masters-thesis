

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
b0 <- coefs[which(rownames(coefs) == "mu_z"), 1]

## violent crime coefficient and prediction values (x-axis)
b1 <- coefs[which(rownames(coefs) == "x_pov_z"), 1]
x1 <- seq(-2, 2, length = 100) ## Violent crime

## percent white coefficient and prediction values (colors)
b2 <- coefs[which(rownames(coefs) == "x_aia_z"), 1]
x2 <- seq(-2, 2) ## PerWhite

## interaction coefficient
b3 <- coefs[which(rownames(coefs) == "x_pov_z:x_aia_z"), 1]

## Prediction grid
myx <- expand.grid(x_aia_z = x1, x_pov_z = x2)

## Expected log counts for grid
myx$log_theta <- b0 + b1 * myx$x_pov_z + b2 * myx$x_aia_z +
  b3 * (myx$x_pov_z * myx$x_aia_z)

## Inverse link transform
myx$theta <- exp(myx$log_theta)

## Plots - convert perwhite to factors
#myx$nonwhite_c <- as.factor(myx$nonwhite_c)
myx$x_pov_z <- as.factor(myx$x_pov_z)

## Plot it
p1 <- ggline(myx, x = "x_aia_z", y = "theta", 
             col = "x_pov_z", numeric.x.axis = TRUE, 
             size = 1.5, plot_type = 'l',
             xlab = "AIAN",
             ylab = "Relative rate", alpha = 0.5) +
  scale_colour_manual(values = rev(brewer.pal(6, "Spectral"))) +
  geom_hline(yintercept = mean(pop_tox$rsei.score), linetype = "dashed") +
  ggtitle("Hispanic/Poverty: All")
p1

