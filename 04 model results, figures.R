

library(tmap)
library(stats)
library(lattice)
library(viridis)

load(file = "inla_results.RData")


fitted <- res1$summary.fitted.values
fitted$id <- 1:nrow(fitted)

bin <- fitted[1:3108, ]

# gg
plot_df <- as.data.frame(res$marginals.fixed)

ggplot(plot_df, aes(x = exp(x_pop_z.x), y = x_pop_z.y))  +
  geom_line(size = 2) +
  scale_x_continuous("Population (binomial, back-transformed)") +
  scale_y_continuous("density") +
  theme_bw()


avg <- pop_tox %>%
  filter(black <= (mean(pop_tox$black))+0.5 & black >= (mean(pop_tox$black))-0.5) %>%
  filter(white <= (mean(pop_tox$white))+0.5 & white >= (mean(pop_tox$white))-0.5) %>%
  filter(pov <= (mean(pop_tox$pov))+0.5 & pov >= (mean(pop_tox$pov))-0.5) %>%
  filter(hisp <= (mean(pop_tox$hisp))+0.5 & black >= (mean(pop_tox$hisp))-0.5)# %>%
  #filter(population_10k <= (mean(pop_tox$population_10k))+1 & population_10k >= (mean(pop_tox$population_10k))-1) %>%
  #filter(asian <= (mean(pop_tox$asian))+0.5 & asian >= (mean(pop_tox$asian))-0.5) %>%
  #filter(other <= (mean(pop_tox$other))+0.5 & other >= (mean(pop_tox$other))-0.5)



res1$marginals.fixed$mu_y %>%
  as.data.frame() %>%
  mutate(x = exp(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()

29037, 2016

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

ggplot(pop_tox, aes(year, rsei.score.cancer, colour = epa_region)) +
  geom_point(alpha = 0.25, cex = 2)

tm_shape(pop_tox_yr$`2020`) +
  tm_polygons(col = "rsei.score.cancer", lwd = 0, palette = "plasma")

rsei_mean_epa <- aggregate(pop_tox$rsei.score.cancer, by = list(pop_tox$epa_region), FUN = mean)
pop_tox$mean_epa <- NA

pop_tox <- pop_tox %>%
  mutate(mean_epa = ifelse(epa_region == "one", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "one"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "two", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "two"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "three", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "three"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "four", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "four"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "five", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "five"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "six", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "six"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "seven", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "seven"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "eight", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "eight"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "nine", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "nine"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "ten", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "ten"), 2], mean_epa)) %>%
  mutate(mean_epa = ifelse(epa_region == "twenty-five", rsei_mean_epa[which(rsei_mean_epa$Group.1 == "twenty-five"), 2], mean_epa))
 

score_time <- aggregate(pop_tox$rsei.score.cancer, by = list(pop_tox$year, pop_tox$epa_region), FUN = mean)
ggplot(score_time, aes(x = Group.1, y = x, colour = Group.2)) +
  geom_line(cex = 1) +
  ylim(0, 40000) +
  theme_minimal()#+
  scale_colour_viridis_d(option = "plasma")
  
test <- score_time %>%
  filter(Group.1 = 2011)


tm_shape(pop_tox_shp) +
  tm_polygons(col = "mean_epa", lwd = 0, style = "cont", palette = "viridis") +
tm_facets(by = "year")

pop_tox_shp$cancer_log <- log(pop_tox_shp$rsei.score.cancer + 0.01)
tm_shape(pop_tox_shp) +
  tm_polygons(col = "cancer_log", lwd = 0, style = "cont", palette = "plasma") +
  tm_facets(by = "year")
  
pop_tox_shp$cancer_log <- log(pop_tox$rsei.score.cancer + 0.01)
tm_shape(pop_tox_shp) +
    tm_polygons(col = "cancer_log", lwd = 0, style = "cont", palette = "viridis") +
  tm_facets(by = "year")

tm_shape(pop_tox_shp) +
  tm_polygons(col = "epa_region", lwd = 0, style = "cont", palette = "viridis") +
  tm_facets(by = "year")



# map spatial effect
head(pop_tox_shp$idarea)

id_sre <- data.frame(res$summary.random$id_space_z$ID,
                     res$summary.random$id_space_z$mean)
names(id_sre) <- c("id", "mean")
test <- merge(pop_tox_shp, id_sre, by.x = "idarea", by.y = "id")

test <- split(test, test$year)

pop_tox_shp$u <- res$summary.random$id_space_z$mean[1:31080]
head(res$summary.random$id_space_z)
length(res$summary.random$id_space_y$mean)
head(res$summary.random$id_space_z)
head(geometry$geoid)
head(pop_tox$geoid)

tm_shape(test) + 
  tm_fill("mean", style = "cont", lwd = 0, palette = "viridis") +
  tm_facets(by = "year")
  tm_borders() +
  tm_layout(main.title = "UFO model spatial random effect")


