
library(dplyr)

nri <- read.csv("/Users/brenna/Downloads/NRI_Table_Counties/NRI_Table_Counties.csv") %>%
  select(c(STATE,
           STATEABBRV,
           STATEFIPS,
           COUNTY,
           COUNTYTYPE,
           COUNTYFIPS,
           STCOFIPS,
           NRI_ID,
           CFLD_EVNTS,
           CFLD_EXPP,
           CFLD_RISKV,
           CFLD_RISKS,
           CFLD_RISKR,
           DRGT_EVNTS,
           DRGT_EXP_AREA,
           DRGT_RISKV,
           DRGT_RISKS,
           DRGT_RISKR,
           RFLD_EVNTS,
           RFLD_EXPP,
           RFLD_RISKV,
           RFLD_RISKS,
           RFLD_RISKR))

names(nri) <- tolower(names(nri))
head(nri)

nri$stcofips <- str_pad(nri$stcofips, 5, pad = "0")

table(nri$drgt_riskr)
table(nri$rfld_riskr)
table(nri$cfld_riskr)

space_re <- st_read("data/space_re.shp")

nri_tox <- merge(space_re, nri, by.x = "geoid", by.y = "stcofips")

nri_tox$coastal <- ifelse(nri_tox$cfld_riskr %in% c("Insufficient Data",
                                            "No Rating",
                                            "Not Applicable"),
                      NA, nri_tox$cfld_riskr)
nri_tox$coastal <- factor(nri_tox$coastal, levels = c("Very Low",
                                              "Relatively Low",
                                              "Relatively Moderate",
                                              "Relatively High",
                                              "Very High"))
nri_tox$drought <- ifelse(nri_tox$drgt_riskr %in% c("Insufficient Data",
                                            "No Rating"),
                      NA, nri_tox$drgt_riskr)
nri_tox$drought <- factor(nri_tox$drought, levels = c("Very Low",
                                              "Relatively Low",
                                              "Relatively Moderate",
                                              "Relatively High",
                                              "Very High"))
nri_tox$river <- ifelse(nri_tox$rfld_riskr %in% c("Insufficient Data",
                                            "No Rating"),
                      NA, nri_tox$rfld_riskr)
nri_tox$river <- factor(nri_tox$river, levels = c("Very Low",
                                          "Relatively Low",
                                          "Relatively Moderate",
                                          "Relatively High",
                                          "Very High"))

nri_tox$river_coast <- paste(nri_tox$river, nri_tox$coastal, sep = ", ")
table(nri_tox$river_coast)

tm_shape(nri_tox) +
  tm_polygons(col = "river", palette = "Reds")
tm_shape(nri_tox) +
  tm_polygons(col = "cfld_riskr", palette = "Reds")

tm_shape(nri_tox) +
  tm_polygons(col = "river_coast")

nri_tox$drought[is.na(nri_tox$drought)] <- "Very Low"
nri_tox$drought_txt <- paste(nri_tox$drought, "dr")
nri_tox$tox_txt <- paste(nri_tox$excdnc_, "tox")
nri_tox$drought_tox <- paste(nri_tox$drought_txt, nri_tox$tox_txt, sep = " | ")
table(nri_tox$drought_tox)

nri_tox$river[is.na(nri_tox$river)] <- "Very Low"
nri_tox$river_txt <- paste(nri_tox$river, "riv")
nri_tox$tox_txt <- paste(nri_tox$excdnc_, "tox")
nri_tox$river_tox <- paste(nri_tox$river_txt, nri_tox$tox_txt, sep = " | ")
table(nri_tox$river_tox)


tm_shape(nri_tox) +
  tm_polygons(col = "drought_tox", lwd = 0, style = "cont", palette=c("Very Low dr | Low, <0.67 tox"='#feffff', 
                                                                      "Relatively Low dr | Low, <0.67 tox" = "#f9f6d3",
                                                                      "Relatively Moderate dr | Low, <0.67 tox" = "#f5eb85", 
                                                                      "Relatively High dr | Low, <0.67 tox" = "#fcef37", #
                                                                      "Very High dr | Low, <0.67 tox" = "#fef519",
                                                                      "Very Low dr | Moderate, >0.67 tox" = "#fdbcb3",# missing high high
                                                                      "Relatively Low dr | Moderate, >0.67 tox" = "#f4baa8",
                                                                      "Relatively Moderate dr | Moderate, >0.67 tox" = "#cba96b", #
                                                                      "Relatively High dr | Moderate, >0.67 tox" = "#a3a42c",
                                                                      # no very high | moderate
                                                                      "Very Low dr | High, >0.89 tox" = "#fd7969",
                                                                      "Relatively Low dr | High, >0.89 tox" = "#d6745a",
                                                                      "Relatively Moderate dr | High, >0.89 tox" = "#723b36", #
                                                                      "Relatively High dr | High, >0.89 tox" = "#453138",
                                                                      # no very high | high
                                                                      "Very Low dr | Very High, >0.97 tox" = "#fe1303",
                                                                      "Relatively Low dr | Very High, >0.97 tox" = "#d01609",
                                                                      "Relatively Moderate dr | Very High, >0.97 tox" = "#40122e",
                                                                      "Relatively High dr | Very High, >0.97 tox" = "#210e35",
                                                                      "Very High dr | Very High, >0.97 tox" = "#000443"),
              #title = "Credibility Amount Polluted \nMore Than Average (Gam)",
  ) +
  tm_shape(state_geo) +
  tm_layout(legend.outside = TRUE, legend.outside.size = 0.5) +
  tm_borders(col = "gray50", lwd = 0.5)



tm_shape(nri_tox) +
  tm_polygons(col = "river_tox", lwd = 0, style = "cont", palette=c("Very Low riv | Low, <0.67 tox"='#feffff', 
                                                                      "Relatively Low riv | Low, <0.67 tox" = "#f1fefd",
                                                                      "Relatively Moderate riv | Low, <0.67 tox" = "#d1fcfb", 
                                                                      "Relatively High riv | Low, <0.67 tox" = "#9dfcfc", #
                                                                      "Very High riv | Low, <0.67 tox" = "#00fefe",
                                                                      "Very Low riv | Moderate, >0.67 tox" = "#fae9e6",# missing high high
                                                                      "Relatively Low riv | Moderate, >0.67 tox" = "#d6b9b2",
                                                                      "Relatively Moderate riv | Moderate, >0.67 tox" = "#98b5b5", #
                                                                      "Relatively High riv | Moderate, >0.67 tox" = "#54b4ba",
                                                                      # no very high | moderate
                                                                      "Very Low riv | High, >0.89 tox" = "#f18070",
                                                                      "Relatively Low riv | High, >0.89 tox" = "#d8493a",
                                                                      "Relatively Moderate riv | High, >0.89 tox" = "#5b1a2d", #
                                                                      "Relatively High riv | High, >0.89 tox" = "#3f1833",
                                                                      # no very high | high
                                                                      "Very Low riv | Very High, >0.97 tox" = "#fe0d00",
                                                                      "Relatively Low riv | Very High, >0.97 tox" = "#c60c05",
                                                                      "Relatively Moderate riv | Very High, >0.97 tox" = "#990b0f",
                                                                      "Relatively High riv | Very High, >0.97 tox" = "#350525",
                                                                      "Very High riv | Very High, >0.97 tox" = "#000032"),
              #title = "Credibility Amount Polluted \nMore Than Average (Gam)",
  ) +
  tm_shape(state_geo) +
  tm_layout(legend.outside = TRUE, legend.outside.size = 0.5) +
  tm_borders(col = "gray50", lwd = 0.5)


aggregate(nri_tox$total, by = list(nri_tox$drought), FUN = sum)
aggregate(nri_tox$total, by = list(nri_tox$river), FUN = sum)

aggregate(nri_tox$total, by = list(nri_tox$river_txt,
                                   nri_tox$excdnc_), FUN = sum)

nri_tox[which(nri_tox$river_tox == "Very High riv | Very High, >0.97 tox"), ]
max_tox <- nri_tox[which(nri_tox$prob_y == max(nri_tox$prob_y)), ]
prop.table(table(max_tox$river))
prop.table(table(nri_tox$river))


aggregate(nri_tox$nonwhit, by = list(nri_tox$drought_tox), FUN = sum)
aggregate(nri_tox$pov, by = list(nri_tox$river_tox), FUN = mean)

aggregate(nri_tox$total, by = list(nri_tox$drought), FUN = sum)
aggregate(nri_tox$total, by = list(nri_tox$river), FUN = sum)




