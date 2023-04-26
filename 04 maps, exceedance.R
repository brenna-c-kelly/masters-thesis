
library(purrr)
library(wesanderson)

mean(res$summary.random$id_space_z[, "mean"], na.rm = TRUE)


re_space_z <- data.frame(prob = NA,
                         row_id = NA)

for(i in 1:length(res$marginals.random$id_space_z)) {
  id = round(i, 0)
  #print(id)
  df <- c(prob = 1 - inla.pmarginal(mean(res$summary.random$id_space_z[, "mean"], na.rm = TRUE), 
                                         res$marginals.random$id_space_z[[i]]),
                   row_id = id)
  re_space_z <- rbind(re_space_z, df)
}



re_space_y <- data.frame(prob = NA,
                         row_id = NA)

for(i in 1:length(res$marginals.random$id_space_y)) {
  id = round(i, 0)
  #print(id)
  df <- c(prob = 1 - inla.pmarginal(mean(res$summary.random$id_space_y[, "mean"], na.rm = TRUE), 
                                    res$marginals.random$id_space_y[[i]]),
          row_id = id)
  re_space_y <- rbind(re_space_y, df)
}

re_time_z <- data.frame(prob_1 = NA,
                        prob_2 = NA,
                         row_id = NA)
summary(res)

inla.pmarginal(0, 
               res$marginals.random$id_time_z[[9]])

?inla.pmarginal
res$summary.random$id_time_z
for(i in 1:length(res$marginals.random$id_time_z)) {
  id = round(i, 0)
  #print(id)
  df <- c(prob_1 = 1 - inla.pmarginal(mean(res$summary.random$id_time_z[, "mean"], na.rm = TRUE), 
                                    res$marginals.random$id_time_z[[i]]),
          prob_2 = inla.pmarginal(mean(res$summary.random$id_time_z[, "mean"], na.rm = TRUE), 
                                  res$marginals.random$id_time_z[[i]]),
          row_id = id)
  re_time_z <- rbind(re_time_z, df)
}
re_time_z$prob_1 <- round(re_time_z$prob_1, 3)
re_time_z$prob_2 <- round(re_time_z$prob_2, 3)
re_time_y <- data.frame(prob_1 = NA,
                        prob_2 = NA,
                        row_id = NA)
for(i in 1:length(res$marginals.random$id_time_y)) {
  id = round(i, 0)
  #print(id)
  df <- c(prob_1 = 1 - inla.pmarginal(mean(res$summary.random$id_time_y[, "mean"], na.rm = TRUE), 
                                    res$marginals.random$id_time_y[[i]]),
          prob_2 = inla.pmarginal(mean(res$summary.random$id_time_y[, "mean"], na.rm = TRUE), 
                                      res$marginals.random$id_time_y[[i]]),
          row_id = id)
  re_time_y <- rbind(re_time_y, df)
}
re_time_y$prob_1 <- round(re_time_y$prob_1, 3)
re_time_y$prob_2 <- round(re_time_y$prob_2, 3)
res$marginals.random$id_time_z[1]
inla.pmarginal(res$marginals.random$id_time_z$index.1,
               res$marginals.random$id_time_z$index.10)
inla.pmarginal(0, res$marginals.random$id_time_z$index.1)
1 - inla.pmarginal(mean(res$summary.random$id_time_y[, "mean"], na.rm = TRUE), 
                   res$marginals.random$id_time_y[[10]])

re_time_z
res$summary.random$id_time_z[, "mean"]
res$summary.random$id_time_y[, "mean"]

inla.pmarginal(mean(res$summary.random$id_space_z[, "mean"], na.rm = TRUE), 
               res$marginals.random$id_space_z$index.1)
inla.pmarginal(1, res$marginals.random$id_space_z)
str(res$marginals.random$id_space_z)
str(res$marginals.fixed$`x_pov_y:x_aia_y`)

space_re_z <- res$summary.random$id_space_z[, "mean"]
space_re_y <- res$summary.random$id_space_y[, "mean"]
time_re_z <- res$summary.random$id_time_z[, "mean"]
time_re_y <- res$summary.random$id_time_y[, "mean"]

space_re_z <- map_df(space_re_z, ~as.data.frame(t(.)))
space_re_z$row_id <- rownames(space_re_z)

space_re_y <- map_df(space_re_y, ~as.data.frame(t(.)))
space_re_y$row_id <- rownames(space_re_y)

time_re_z <- map_df(time_re_z, ~as.data.frame(t(.)))
time_re_z$row_id <- rownames(time_re_z)

time_re_y <- map_df(time_re_y, ~as.data.frame(t(.)))
time_re_y$row_id <- rownames(time_re_y)

names(re_space_z)
pop_tox_1yr <- pop_tox %>%
  filter(year == 2020)

space_z <- merge(pop_tox_1yr, re_space_z, by.x = "idarea", by.y = "row_id")
space_z <- merge(geometry, space_z, by = "geoid")
space_z$exceedance <- case_when(space_z$prob > 0.97 ~ "Very High, >0.97",
                                space_z$prob > 0.89 ~ "High, >0.89",
                                space_z$prob > 0.67 ~ "Moderate, >0.67",
                                space_z$prob < 0.67 ~ "Low, <0.67")
space_z$exceedance <- factor(space_z$exceedance, levels = c("Very High, >0.97", "High, >0.89",
                                                            "Moderate, >0.67", "Low, <0.67"))

space_y <- merge(pop_tox_1yr, re_space_y, by.x = "idarea", by.y = "row_id")
space_y <- merge(geometry, space_y, by = "geoid")
space_y$exceedance <- case_when(space_y$prob > 0.97 ~ "Very High, >0.97",
                                space_y$prob > 0.89 ~ "High, >0.89",
                                space_y$prob > 0.67 ~ "Moderate, >0.67",
                                space_y$prob < 0.67 ~ "Low, <0.67")
space_y$exceedance <- factor(space_y$exceedance, levels = c("Very High, >0.97", "High, >0.89",
                                                            "Moderate, >0.67", "Low, <0.67"))

wes_palette("Royal1")
pal <- wes_palette(4, name = "Royal1", type = "continuous")
Royal1 = c("#C93312", "#DC863B", "#FAEFD1", "#899DA4")
Zissou <- c("#F21A00", "#E1AF00","#EBCC2A","#3B9AB2")
"#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF"
t <- c("#47039FFF", "#BD3786FF", "#FA9E3BFF", "#F7E225FF")
"#0D0887FF" "#47039FFF" "#7301A8FF" "#9C179EFF" "#BD3786FF" "#D8576BFF" "#ED7953FF" "#FA9E3BFF" "#FDC926FF"
"#F0F921FF" "gray"
viridis(20, option = "C")

"#C5407EFF" "#D14E72FF" "#DD5E66FF" "#E76E5BFF" "#EF7F4FFF" "#F79044FF" "#FBA238FF" "#FEB72DFF" "#FDCB26FF"
[19] "#F7E225FF" "#F0F921FF"

z <- tm_shape(space_z) +
  tm_polygons(col = "exceedance", lwd = 0, style = "cont", palette = t, #midpoint = 0.95,
              title = "Credibility Being Polluted \nMore Than Average (Bin)")
y <- tm_shape(space_y) +
  tm_polygons(col = "exceedance", lwd = 0, style = "cont", palette = t,
              title = "Credibility Amount Polluted \nMore Than Average (Gam)")
tmap_arrange(z, y)
#dev.off()

space_y_no_geom <- st_drop_geometry(space_y)
space_y_no_geom <- space_y_no_geom[, c(1, 50:53)]
names(space_y_no_geom) <- c("geoid", "idarea1", "idtime", "prob_y", "exceedance_y")
space_re <- cbind(space_z, space_y_no_geom)

space_re$exc_diff <- paste(space_re$exceedance, space_re$exceedance_y)
table(space_re$exc_diff)

#ca, or, wa, id, nv, az, ut, co, wy, nm, mt, nd, sd, ne, ks, ok, tx, la, ak, mo, io, mn
west <- c(05, 04, 06, 08, 19, 16, 20, 22, 27, 29, 30, 38, 31, 35, 32, 40, 41, 46, 48, 49, 53, 56)
west <- as.character(west)
west <- str_pad(west, 2, pad = "0")

space_re$west_east <- ifelse(space_re$state %in% west, "west", "east")
table(space_re$west_east)
tm_shape(space_re) +
  tm_polygons(col = "west_east")

prop.table(table(space_re$exceedance, space_re$west_east))
table(space_re$state, space_re$exceedance_y)

test <- data.frame(table(space_re$state, space_re$exceedance)) %>%
  group_by(Var1) %>%
  spread(Var2, Freq) %>%
  mutate(total = `Very High, >0.97` + `High, >0.89` + `Moderate, >0.67` + `Low, <0.67`) %>%
  mutate(perc_high = `Very High, >0.97` / total)

table(space_re$exc_diff)
aggregate(space_re$prob_y, by = list(space_re$west_east), FUN = mean)
table(space_re$exceedance, space_re$west_east)
table(space_re$exceedance_y, space_re$west_east)

tm_shape(space_re) +
  tm_polygons(col = "exc_diff", lwd = 0, style = "cont", palette = t,
              title = "Credibility Amount Polluted \nMore Than Average (Gam)")

high_low <- space_re %>%
  filter(exc_diff == "Very High, >0.97 Low, <0.67")
low_high <- space_re %>%
  filter(exc_diff == "Low, <0.67 Very High, >0.97")
high_high <- space_re %>%
  filter(exc_diff == "Very High, >0.97 Very High, >0.97")
low_low <- space_re %>%
  filter(exc_diff == "Low, <0.67 Low, <0.67")
14/3108

low_high
head(space_re)
state_exc <- data.frame(table(space_re$state, space_re$exc_diff))
n_counties <- aggregate(state_exc$Freq, by = list(state_exc$Var1), FUN = sum)

round(prop.table(table(space_re$exc_diff)), 3)

prop.table(table(space_re$exceedance))

state_exc <- state_exc %>%
  group_by(Var1) %>%
  spread(Var2, Freq) %>%
  
names(n_counties)
state_exc <- merge(n_counties, state_exc, by.x = "Group.1", by.y = "Var1", all = TRUE)

state_exc <- cbind(state_exc[, 1], round((state_exc[, c(3:17)] / state_exc[, 2]), 3))

# high high 42107 Schuylkill PA; 49035 SLC UT

"#0D0887FF" "#47039FFF" "#7301A8FF" "#9C179EFF" "#BD3786FF" "#D8576BFF" "#ED7953FF" "#FA9E3BFF" "#FDC926FF"
"#F0F921FF"

"r"

high_eg <- space_re %>%
  filter(geoid == "49035") # 42107
high_eg_map <- tm_shape(high_eg) +
  tm_polygons(col = "exc_diff", palette = "#e40301", lwd = 0.25, title = "", legend.show = F) +
  tm_text("name", size = 1/2) +
  tm_add_legend(labels = c("High, High"), col = c("#e40301"), border.lwd = 0)
low_eg <- space_re %>%
  filter(geoid == "30035") # 42107
low_eg_map <- tm_shape(low_eg) +
  tm_polygons(col = "exc_diff", palette = "#feffff", lwd = 0.25, title = "", legend.show = F) +
  tm_text("name", size = 1/2) +
  tm_add_legend(labels = c("Low, Low"), col = c("#feffff"), border.lwd = 0.25)
hl_eg <- space_re %>%
  filter(geoid == "23005") # 23005 Maine; 09001 Fairfield County, CT
hl_eg_map <- tm_shape(hl_eg) +
  tm_polygons(col = "exc_diff", palette = "#eb9b4f", lwd = 0.25, title = "", legend.show = F) +
  tm_text("name", size = 1/2, just = c(0.25, 0)) +
  tm_add_legend(labels = c("High, Low"), col = c("#eb9b4f"), border.lwd = 0)
lh_eg <- space_re %>%
  filter(geoid == "24005") # Baltimore County, MD
lh_eg_map <- tm_shape(lh_eg) +
  tm_polygons(col = "exc_diff", palette = "#01a2b7", lwd = 0.25, title = "", legend.show = F) +
  tm_text("name", size = 1/2) +
  tm_add_legend(labels = c("Low, High"), col = c("#01a2b7"), border.lwd = 0)

tmap_arrange(lh_eg_map, high_eg_map, 
             low_eg_map, hl_eg_map)


# bivariate
table(space_re$exc_diff)
biv <- c("#f11b07", "#bc1e19", "#2c173c", "#001142",
                     "#e63a32",  "#38295b", "#26398e", #"#a95a77", wouldve been second
                     "#cb96b3", "#aa85b2", "#4857be", "#4860e6",
                     "#c1ccfb", "#a8b0f1", "#6f7fee", "#4e68f9")
                  
space_y$exc_diff <- factor(space_y$exceedance, levels = c("Low, <0.67 Very High, >0.97",
                                                          "Moderate, >0.67 Very High, >0.97",
                                                          "High, >0.89 Very High, >0.97",
                                                          "Very High, >0.97 Very High, >0.97", #
                                                          "Low, <0.67 High, >0.89",
                                                          "Moderate, >0.67 High, >0.89",# missing high high
                                                          "Very High, >0.97 High, >0.89", #
                                                          "Low, <0.67 Moderate, >0.67",
                                                          "Moderate, >0.67 Moderate, >0.67",
                                                          "High, >0.89 Moderate, >0.67",
                                                          "Very High, >0.97 Moderate, >0.67", #
                                                          "Low, <0.67 Low, <0.67",
                                                          "Moderate, >0.67 Low, <0.67",
                                                          "High, >0.89 Low, <0.67",
                                                          "Very High, >0.97 Low, <0.67"))

tm_shape(space_re) +
  tm_polygons(col = "exc_diff", lwd = 0, style = "cont", palette=c("Low, <0.67 Very High, >0.97"='#01a2b7', 
                                                                   "Moderate, >0.67 Very High, >0.97" = "#347988",
                                                                   "High, >0.89 Very High, >0.97" = "#bd4e43", 
                                                                   "Very High, >0.97 Very High, >0.97" = "#e40301", #
                                                                   "Low, <0.67 High, >0.89" = "#4dbdca",
                                                                   "Moderate, >0.67 High, >0.89" = "#898483",# missing high high
                                                                   "Very High, >0.97 High, >0.89" = "#e14d2c", #
                                                                   "Low, <0.67 Moderate, >0.67" = "#9bd4d9",
                                                                   "Moderate, >0.67 Moderate, >0.67" = "#deb99e",
                                                                   "High, >0.89 Moderate, >0.67" = "#e49362",
                                                                   "Very High, >0.97 Moderate, >0.67" = "#e78346", #
                                                                   "Low, <0.67 Low, <0.67" = "#feffff",
                                                                   "Moderate, >0.67 Low, <0.67" = "#f1c9a5",
                                                                   "High, >0.89 Low, <0.67" = "#eeae74",
                                                                   "Very High, >0.97 Low, <0.67" = "#eb9b4f"),
              #title = "Credibility Amount Polluted \nMore Than Average (Gam)",
              ) + #legend.show = F
  tm_shape(st) +
  tm_layout(legend.outside = TRUE, legend.outside.size = 0.5) +
  tm_borders(col = "gray50", lwd = 0.5)


flint <- space_re %>%
  filter(geoid == "26049")
flint$exceedance
flint$exc_diff
flint$prob
flint$prob_y

philly <- space_re %>%
  filter(geoid == "42101")
philly$exceedance
philly$exc_diff
philly$prob
philly$prob_y

schu <- space_re %>%
  filter(geoid == "42107")
schu$exceedance
schu$exc_diff
schu$prob
schu$prob_y

table(pop_tox$rsei.score < 10, pop_tox$rsei_score_bin)

pop_tox_20_geom <- merge(geometry, pop_tox_20, by = "geoid")
tm_shape(pop_tox_20_geom) +
  tm_polygons(col = "gray20", border.col = "gray100")


pop_tox_20_geom <- pop_tox_20_geom[!st_is_empty(pop_tox_20_geom),,drop=FALSE]

pop_tox_20_geom$rsei_bin <- ifelse(pop_tox_20_geom$rsei.score == 0, 0, 1)
pop_tox_20_geom$rsei.score_log <- log(pop_tox_20_geom$rsei.score + 1)

table(is.na(pop_tox_20_geom$rsei.score))
tm_shape(pop_tox_20_geom) +
  tm_polygons(col = "rsei_bin", lwd = 0, palette = "PuRd", legend.show = FALSE)

tm_shape(pop_tox_20_geom) +
  tm_polygons(col = "rsei.score_log", lwd = 0, palette = "PuRd", legend.show = FALSE)

summary(pop_tox_20)

