

library(lattice)

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
pop_tox_2$RW2D <- xx$summary.fitted.values[, "mean"]

tm_shape(pop_tox_2) +
  tm_polygons(col = "RW2D", lwd = 0, style = "cont")