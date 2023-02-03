
library(dplyr)

rsei <- read.csv("rsei modeled county.csv")
names(rsei) <- tolower(names(rsei))

length(unique(rsei$fips)) # 1573 counties were modeled
rsei$rsei.score <- as.numeric(gsub(",", "", rsei$rsei.score))

table(rsei$submission.year) # contains all years

pop_tox <- read.csv("pop_tox.csv")


#pop <- select(pop_tox, -c(3:24))
#pop # ready to merge

head(new)
table(new$state)
new_2020 <- new %>%
  filter(submission.year == 2020) %>%
  filter(!state %in% c("Hawaii", "Puerto Rico",
                       "Alaska", "American Samoa",
                       "Northern Mariana Islands",
                       "Virgin Islands"))
new_2020$fips <- str_pad(new_2020$fips, width = 5, pad = "0")
#getting n trials
facilities <- data.frame(table(new_2020$tri.facility.id))
names(facilities) <- c("tri.facility.id", "n")
facilities_county <- merge(facilities, new_2020, by = "tri.facility.id")
facilities_county <- facilities_county %>%
  filter(duplicated(fips) == FALSE)


# better data
by_county <- read.csv("/Users/brenna/Documents/School/Thesis/water by county.csv")
names(by_county) <- tolower(names(by_county))

summary(by_county)
by_county$rsei.score <- as.numeric(gsub(",", "", by_county$rsei.score))
by_county$rsei.score.cancer <- as.numeric(gsub(",", "", by_county$rsei.score.cancer))
by_county$rsei.score.noncancer <- as.numeric(gsub(",", "", by_county$rsei.score.noncancer))

by_county$fips <- str_pad(by_county$fips, width = 5, pad = "0")

by_county_2020 <- by_county %>%
  filter(submission.year == 2020) %>%
  filter(rsei.media == "Direct Water Releases")

nonunique <- data.frame(by_county_2020[duplicated(by_county_2020$fips),])
nonunique_fips <- nonunique$fips

#by_county_2020$nonunique_fips_flag[by_county_2020$fips %in% nonunique_fips] <- "not unique"
#by_county_2020$nonunique_fips_flag[!by_county_2020$fips %in% nonunique_fips] <- "unique"

fix <- by_county_2020 %>%
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

impute <- by_county_2020[by_county_2020$fips %in% nonunique_fips, ]
impute <- impute[!duplicated(impute$fips), ]
impute <- select(impute, -c("rsei.score",
                            "rsei.score.cancer",
                            "rsei.score.noncancer"))

by_county_2020 <- by_county_2020 %>%
  filter(!fips %in% nonunique_fips)

added <- added %>%
  filter(fips != "")

added <- added[with(added, order(fips)), ]
impute <- impute[with(impute, order(fips)), ]

fixed <- cbind(impute, added)
fixed <- select(fixed, -c(9))

by_county_2020 <- rbind(by_county_2020, fixed)

by_county_2020$rsei.score <- as.numeric(by_county_2020$rsei.score)
by_county_2020$rsei.score.cancer <- as.numeric(by_county_2020$rsei.score.cancer)
by_county_2020$rsei.score.noncancer <- as.numeric(by_county_2020$rsei.score.noncancer)

####
#merging old pop data with new tox data
names(by_county_2020)
by_county_2020 <- by_county_2020 %>%
  filter(!state %in% c("Alaska", "Hawaii",
         "Guam", "Puerto Rico",
         "Virgin Islands"))

pop_tox <- st_as_sf(pop_tox)
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
pop_tox <- st_transform(pop_tox, crs = st_crs(aea))

pop_tox_2 <- merge(pop_tox, by_county_2020, by.x = "geoid", by.y = "fips", all = TRUE)

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

#getting n trials
pop_tox_n <- merge(pop_tox_2, facilities_county, by.x = "geoid", by.y = "fips", all = TRUE)
pop_tox_n <- pop_tox_n %>%
  filter(!is.na(white))




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


