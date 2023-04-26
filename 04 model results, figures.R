

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

ggplot(plot_df, aes(x = exp(x_blac_z), y = x_pop_z.y))  +
  geom_line(lwd = 2) +
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


score_time <- aggregate(pop_tox$rsei.score, by = list(pop_tox$year), FUN = mean)

bin_time <- aggregate(pop_tox$rsei_cancer_bin, by = list(pop_tox$year), FUN = mean, na.rm = TRUE)
bin_time$Group.1 <- as.factor(bin_time$Group.1)
bin_time$Group.1 <- relevel(bin_time$Group.1, ref = "2015")
bin_time <- pop_tox %>%
  filter(rsei_cancer_bin == 1)
chisq.test(bin_time$rsei.score.cancer, bin_time$year)
summary(glm(rsei.score.cancer ~ year, data = pop_tox))
summary(glm(rsei_cancer_bin ~ year, data = pop_tox))

score_time <- aggregate(pop_tox$rsei.score.cancer, by = list(pop_tox$epa_region), FUN = mean)
ggplot(score_time, aes(x = Group.1, y = x)) +
  geom_line(cex = 1) +
  ylim(0, 10000) +
  theme_minimal()#+
#  scale_colour_viridis_d(option = "plasma")
names(pop_tox)
hist(pop_tox$black)
hist(pop_tox$nhpi)

bins <- pop_tox %>%
  select(geoid, black, aian, asian, nhpi, other, tom, hisp,
           black_lc, aian_lc, asian_lc, nhpi_lc, other_lc, tom_lc, hisp_lc) %>%
  group_by(geoid) %>%
  gather(c(black, aian, asian, nhpi, other, tom, hisp,
           black_lc, aian_lc, asian_lc, nhpi_lc, other_lc, tom_lc, hisp_lc),
         key = geoid)

# tri facilities map
tri <- read.csv("/Users/brenna/Downloads/EPA_Facility_Registry_Service_-_Toxic_Release_Inventory_(TRI).csv")
names(tri) <- tolower(names(tri))
length(unique(tri$pgm_sys_id))
tri <- tri %>%
  filter(active_status == "ACTIVE")
table(tri$active_status)
d <- st_as_sf(tri, coords = c(1:2))
head(d)
tm_shape(st) +
  tm_polygons(col = "gray95", border.col = "gray10") +
  tm_shape(d) +
  tm_dots(col = "red", alpha = 0.15, shape = 18, size = 0.5)



ggplot(pop_tox, aes(x=black, colour = aian)) + 
  geom_histogram()

ggplot(pop_tox, aes("aian")) +
  geom_histogram(binwidth = 0.1)

test <- score_time %>%
  filter(Group.1 == 2013)

bin <- pop_tox %>%
  filter(rsei_cancer_bin == 1)

summary(bin$rsei.score.cancer)
bin[which(bin$rsei.score.cancer == max(bin$rsei.score.cancer)), ]

bin[which(bin$rsei.score.cancer == min(bin$rsei.score.cancer)), ]

tm_shape(pop_tox_yr$`2011`) +
  tm_polygons(col = "epa_region", lwd = 0, style = "cont", palette = "plasma") +
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


