



library(ggplot2)
library(viridis)
library(ggridges)
library(hrbrthemes)

smoothed_margs <- data.frame(NA, NA, NA)
names(smoothed_margs) <- c("x", "y", "term")

names(inla.smarginal(res$marginals.fixed$mu_z))

n = length(res$marginals.fixed)
marglist = vector("list", length = n)

marg_names <- names(res$marginals.fixed)

inla.smarginal(res$marginals.fixed[["mu_z"]])

for(i in marg_names) {
  marg <- data.frame(inla.smarginal(res$marginals.fixed[[i]]))
  #marg <- data.frame(res$marginals.fixed[[i]])
  marg$var <- i
  marglist[[i]] <- marg
  }
  
smoothed_margs = do.call(rbind, marglist)
rownames(smoothed_margs) <- NULL

means <- aggregate(smoothed_margs$x, by = list(smoothed_margs$var), FUN = mean) %>%
  arrange(x)

means$Group.1

binomials <- c("mu_z", "x_gin_z", "x_his_z", "x_aia_z", "x_oth_z", "x_nhp_z",
               "x_asi_z", "x_tom_z", "x_bla_z", "x_pov_z", "x_pop_z")
gamma <- c("mu_y", "x_pop_y", "x_pov_y", "x_gin_y", "x_tom_y", "x_oth_y", 
             "x_nhp_y", "x_aia_y", "x_asi_y", "x_his_y", "x_bla_y")
myorder <- c("mu_z", "x_gin_z", "x_his_z", "x_aia_z", "x_oth_z", "x_nhp_z",
             "x_asi_z", "x_tom_z", "x_bla_z", "x_pov_z", "x_pop_z",
             "mu_y", "x_pop_y", "x_pov_y", "x_gin_y", "x_tom_y", "x_oth_y", 
             "x_nhp_y", "x_aia_y", "x_asi_y", "x_his_y", "x_bla_y")

smoothed_margs$var <- as.factor(smoothed_margs$var)
smoothed_margs$family <- as.factor(ifelse(smoothed_margs$var %in% binomials, "binomial", "gamma"))

smoothed_margs <- smoothed_margs %>% 
  mutate(var = factor(var, levels = rev(myorder)))
#smoothed_margs$x <- exp(smoothed_margs$x)



ggplot(smoothed_margs) +
  geom_vline(xintercept = 0) +
  ggridges::geom_density_ridges_gradient(aes(x = x, y = var, fill = ..x..), 
                                         scale = 3, rel_min_height = 0.01,
                                         lwd = 0.5) +
  scale_fill_viridis(aes(x = x, y = var, fill = ..x..), 
                      discrete = FALSE, option = "C") +
  labs(title = "Marginal Densities") +
  hrbrthemes::theme_ipsum()# +
  #facet_grid(rows = vars(family))

bin_margs <- smoothed_margs %>%
  filter(family == "binomial")
gam_margs <- smoothed_margs %>%
  filter(family == "gamma")

ggplot(bin_margs) +
  geom_vline(xintercept = 0) +
  ggridges::geom_density_ridges_gradient(aes(x = x, y = var, fill = ..x..), 
                                         scale = 2, rel_min_height = 0.01,
                                         lwd = 0.5) +
  scale_fill_viridis(aes(x = x, y = var, fill = ..x..), 
                     discrete = FALSE, option = "C") +
  labs(title = "Marginal Densities") +
  hrbrthemes::theme_ipsum()


# significant effects
aggregate(smoothed_margs$x, by = list(smoothed_margs$var), FUN = min)
inla.hpdmarginal(0.99, res$marginals.fixed$x_gin_y)

print(res$summary.fixed)

smoothed_margs$ci[smoothed_margs$x > inla.hpdmarginal(0.99, res$marginals.fixed$x_gin_y)[1] &
                    smoothed_margs$x < inla.hpdmarginal(0.99, res$marginals.fixed$x_gin_y)[2]] <- "99%"

smoothed_margs$cred <- ifelse(smoothed_margs$var %in% c("x_pop_y", "x_pov_y", "x_gin_y"), "cred", "not cred")

cred <- smoothed_margs %>%
  filter(cred == "cred")# %>%
  #mutate(var = factor(var, levels = ("x_pop_y", "x_pov_y", )))

cred$x <- exp(cred$x)

ggplot(cred) +
  geom_vline(xintercept = 0) +
  ggridges::geom_density_ridges_gradient(aes(x = x, y = var, fill = ..x..), 
                                         scale = 2, rel_min_height = 0.01,
                                         lwd = 0.5) +
  scale_fill_viridis(aes(x = x, y = var, fill = ..x..), 
                     discrete = FALSE, option = "C") +
  labs(title = "Marginal Densities") +
  hrbrthemes::theme_ipsum()

# +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
