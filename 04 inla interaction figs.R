


library(ggpubr)
library(ggplot2)
library(RColorBrewer)

save(res, file = "inla_results.RData")
#res <- load(file = "inla_results.RData")
coefs <- res$summary.fixed

## Intercept
b0 <- coefs[which(rownames(coefs) == "mu_y"), 1]

## violent crime coefficient and prediction values (x-axis)
b1 <- coefs[which(rownames(coefs) == "x_pop_y"), 1]
x1 <- seq(-3, 3, length = 100) ## Violent crime

## percent white coefficient and prediction values (colors)
b2 <- coefs[which(rownames(coefs) == "x_gin_y"), 1]
x2 <- seq(-3, 3) ## PerWhite

## interaction coefficient
b3 <- coefs[which(rownames(coefs) == "x_pop_y:x_gin_y"), 1]

## Prediction grid
myx <- expand.grid(x_pop_y = x1, x_gin_y = x2)

## Expected log counts for grid
myx$log_theta <- b0 + b1 * myx$x_pop_y + b2 * myx$x_gin_y +
  b3 * (myx$x_pop_y * myx$x_gin_y)

## Inverse link transform
myx$theta <- exp(myx$log_theta)

## Plots - convert perwhite to factors
#myx$nonwhite_c <- as.factor(myx$nonwhite_c)
myx$x_gin_y <- as.factor(myx$x_gin_y)

## Plot it
p1 <- ggline(myx, x = "x_pop_y", y = "theta", 
             col = "x_gin_y", numeric.x.axis = TRUE, 
             size = 1.5, plot_type = 'l',
             xlab = "population 10k",
             ylab = "Relative rate") +
  scale_colour_manual(values = rev(brewer.pal(7, "RdYlGn"))) +
  geom_hline(yintercept = mean(pop_tox$rsei_noncancer_bin), linetype = "dashed") +
  ggtitle("Population/Gini Index: All")
p1

hist(myx$pov_c)

mean(pop_tox_2$rsei.score.cancer_log)

hist(pop_tox_2$pov_c)
hist(pop_tox_2$nonwhite_c)

table(pop_tox_2$rsei.score.cancer == 1, pop_tox_2$rsei.score.noncancer == 1)
table(pop_tox_2$rsei.score == 1)



