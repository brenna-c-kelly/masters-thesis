
library(dplyr)

# prep rsei to merge with pop
rsei <- read.csv("water by county.csv")

# contains all years (2011-2020)
# contains all media (transfer, air, incineration, water)
# note: not all counties are included; not all had pollution to model
#   left join population to rsei score; impute 0 lter for non-polluted counties

names(rsei) <- tolower(names(rsei))

rsei$rsei.score <- as.numeric(gsub(",", "", rsei$rsei.score))
rsei$rsei.score.cancer <- as.numeric(gsub(",", "", rsei$rsei.score.cancer))
rsei$rsei.score.noncancer <- as.numeric(gsub(",", "", rsei$rsei.score.noncancer))

rsei$fips <- str_pad(rsei$fips, width = 5, pad = "0")

rsei_2020 <- rsei %>%
  filter(submission.year == 2020) %>%
  filter(rsei.media == "Direct Water Releases")

# some counties are present more than once (2 in 2020, could be more for other years)
nonunique <- data.frame(rsei_2020[duplicated(rsei_2020$fips),])
nonunique_fips <- nonunique$fips

#by_county_2020$nonunique_fips_flag[by_county_2020$fips %in% nonunique_fips] <- "not unique"
#by_county_2020$nonunique_fips_flag[!by_county_2020$fips %in% nonunique_fips] <- "unique"

# get only nonunique, to fix
fix <- rsei_2020 %>%
  filter(fips %in% nonunique_fips)# %>%
  #aggregate(df, list(rsei.score, rsei.score.cancer, rsei.score.noncancer), sum)

added <- data.frame("", "", "", "")
names(added) <- c("rsei.score", "rsei.score.cancer",
                  "rsei.score.noncancer", "fips")

odd <- seq(1, by=2, len=nrow(fix)/2)

for(i in odd) {
  to_add <- fix[i, c('rsei.score', 'rsei.score.cancer', 'rsei.score.noncancer')] +
    fix[i + 1, c('rsei.score', 'rsei.score.cancer', 'rsei.score.noncancer')]
  to_add$fips <- fix[i, "fips"]
  added <- rbind(to_add, added)
  i = i + 1
}

impute <- rsei_2020[rsei_2020$fips %in% nonunique_fips, ]
impute <- impute[!duplicated(impute$fips), ]
impute <- select(impute, -c("rsei.score",
                            "rsei.score.cancer",
                            "rsei.score.noncancer"))

# get only unique fips, to merge with corrected data
rsei_2020 <- rsei_2020 %>%
  filter(!fips %in% nonunique_fips)

added <- added %>%
  filter(fips != "")
added <- added[with(added, order(fips)), ]
impute <- impute[with(impute, order(fips)), ]

fixed <- cbind(impute, added)
fixed <- select(fixed, -c(9))

rsei_2020 <- rbind(rsei_2020, fixed)

# make numeric, again
rsei_2020$rsei.score <- as.numeric(rsei_2020$rsei.score)
rsei_2020$rsei.score.cancer <- as.numeric(rsei_2020$rsei.score.cancer)
rsei_2020$rsei.score.noncancer <- as.numeric(rsei_2020$rsei.score.noncancer)

# remove nonconterminous
rsei_2020 <- rsei_2020 %>%
  filter(!state %in% c("Alaska", "Hawaii",
                       "Guam", "Puerto Rico",
                       "Virgin Islands"))

# merging population data with toxicity data
pop <- st_read("pop.shp")

# left join: keep all population data
pop_tox <- merge(pop, rsei_2020, by.x = "geoid", by.y = "fips", all = TRUE)



pop_tox_2$rsei.score <- ifelse(is.na(pop_tox_2$rsei.score), 1, pop_tox_2$rsei.score + 1)
pop_tox_2$rsei.score.cancer <- ifelse(is.na(pop_tox_2$rsei.score.cancer), 1, pop_tox_2$rsei.score.cancer + 1)
pop_tox_2$rsei.score.noncancer <- ifelse(is.na(pop_tox_2$rsei.score.noncancer), 1, pop_tox_2$rsei.score.noncancer + 1)

pop_tox_2$rsei.score_log <- log(pop_tox_2$rsei.score)
pop_tox_2$rsei.score.cancer_log <- log(pop_tox_2$rsei.score.cancer)
pop_tox_2$rsei.score.noncancer_log <- log(pop_tox_2$rsei.score.noncancer)

tm_shape(pop_tox_2) +
  tm_polygons(col = "black_p")

rsei <- tm_shape(pop_tox_2) +
  tm_polygons(col = "rsei.score_log", style = "cont", lwd = 0, palette = "plasma")
rsei.cancer <- tm_shape(pop_tox_2) +
  tm_polygons(col = "rsei.score.cancer_log", style = "cont", lwd = 0, palette = "plasma")
rsei.noncancer <- tm_shape(pop_tox_2) +
  tm_polygons(col = "rsei.score.noncancer_log", style = "cont", lwd = 0, palette = "plasma")
poc <- tm_shape(pop_tox_2) +
  tm_polygons(col = "nonwhite_p", style = "cont", lwd = 0, palette = "viridis")

hist(pop_tox_2$black_c_log)
current.mode <- tmap_mode("plot")
tmap_arrange(rsei, rsei.cancer, rsei.noncancer, poc,
             ncol = 2, nrow = 2)

tmap_arrange(rsei.cancer, poc,
             ncol = 2, nrow = 1)

#### test model
head(pop_tox_2)

summary(pop_tox_2$pov_p)

(pop_tox_2$pov_p)
pop_tox_2$pov_c_log <- log(pop_tox_2$pov_p + 0.01) - 
  log(mean(pop_tox_2$pov_p + 0.01))
pop_tox_2$nonwhite_c_log <- log(pop_tox_2$nonwhite_p + 0.01) - 
  log(mean(pop_tox_2$nonwhite_p + 0.01))

# centering, logging
pop_tox_2 <- pop_tox_2 %>%
  mutate(black_c_log = log(pop_tox_2$black_p + 0.01) - 
           log(mean(pop_tox_2$black_p + 0.01))) %>%
  mutate(aian_p_log = log(pop_tox_2$aian_p + 0.01) - 
           log(mean(pop_tox_2$aian_p + 0.01))) %>%
  mutate(asian_p_log = log(pop_tox_2$asian_p + 0.01) - 
           log(mean(pop_tox_2$asian_p + 0.01))) %>%
  mutate(nhpi_p_log = log(pop_tox_2$nhpi_p + 0.01) - 
           log(mean(pop_tox_2$nhpi_p + 0.01))) %>%
  mutate(tom_p_log = log(pop_tox_2$tom_p + 0.01) - 
           log(mean(pop_tox_2$tom_p + 0.01))) %>%
  mutate(other_p_log = log(pop_tox_2$other_p + 0.01) - 
           log(mean(pop_tox_2$other_p + 0.01))) %>%
  mutate(black_c = black_p - mean(black_p)) %>%
  mutate
(aian_c = aian_p - mean(aian_p)) %>%
  mutate(asian_c = asian_p - mean(asian_p)) %>%
  mutate(nhpi_c = nhpi_p - mean(nhpi_p)) %>%
  mutate(tom_c = tom_p - mean(tom_p)) %>%
  mutate(other_c = other_p - mean(other_p)) %>%
  mutate(nonwhite_c = nonwhite_p - mean(nonwhite_p))

  

names(pop_tox_2)





xy <- inla(rsei.score.noncancer_bin ~ 1 + f(idarea, model = "bym", graph = g) +
             nonwhite_c_log*pov_c_log + population_10k_c,
           #control.inla=list(cmin=0),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(compute = TRUE),
           family = "binomial",
           #Ntrials = population_10k,
           data = pop_tox_n)
summary(xy)






pop_tox_2$rsei.score.noncancer_bin <- as.factor(pop_tox_2$rsei.score.noncancer_bin)


formula.hurdle <- y ~ -1 + Intercept + f(bath.ber, model = "rw1") +
  f(bath.con, copy="bath.ber",fixed = F) + f(i.ber, model = spde) +
  f(i.con, model = spde)

formula.joint <- rsei.score.cancer_log ~ population_10k_c +
  nonwhite_c_log*pov_c_log + 
  f(idarea, model = "bym", graph = g)

res.jo <- inla(formula.joint, family = c("binomial", "gamma"), 
               data = inla.stack.data(stk.all), control.family = cff, 
               control.predictor = list(A = inla.stack.A(stk.all),
                                        link = link), 
               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE,
                                      config = TRUE),
               control.results = cres, control.inla = cinla, 
               control.mode = list(theta = ini.jo, restart = TRUE))


