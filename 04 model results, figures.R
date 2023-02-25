

library(tmap)
library(stats)
library(lattice)
library(viridis)

load(file = "inla_results.RData")


fitted <- res1$summary.fitted.values
fitted$id <- 1:nrow(fitted)

bin <- fitted[1:3108, ]




summary(fitted$mean)
summary(fitted$sd)

matrix <- res1$model.matrix



#


res1$marginals.fixed$mu_y %>%
  as.data.frame() %>%
  mutate(x = exp(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()


# mapping means
pop_tox$mean_z <- res$summary.fitted.values[1:31080, "mean"]
pop_tox$mean_y <- res$summary.fitted.values[31081:62160, "mean"]
summary(res1$marginals.fixed$x_epa_zeight)

pop_tox$sd_z <- res$summary.fitted.values[1:31080, "sd"]
pop_tox$sd_y <- res$summary.fitted.values[31081:62160, "sd"]

res$summary.fitted.values

summary(res$marginals.fixed$mu_z)
summary(res$marginals.fixed$mu_y)

pop_tox_shp <- merge(geometry, pop_tox, by = "geoid")

pop_tox_yr <- split(pop_tox_shp, pop_tox_shp$year)


tm_shape(pop_tox_yr$`2020`) +
  tm_polygons(col = "sd_y", lwd = 0, style = "cont", palette = "viridis")# +
  tm_facets(by = "year")
  
pop_tox_shp$cancer_log <- log(pop_tox$rsei.score.cancer + 0.01)
tm_shape(pop_tox_shp) +
    tm_polygons(col = "cancer_log", lwd = 0, style = "cont", palette = "viridis") +
  tm_facets(by = "year")

tm_shape(pop_tox_shp) +
  tm_polygons(col = "epa_region", lwd = 0, style = "cont", palette = "viridis") +
  tm_facets(by = "year")

