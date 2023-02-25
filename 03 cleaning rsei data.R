
library(INLA)
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

rsei <- rsei %>% #_2020 <- rsei %>%
  #filter(submission.year == 2020) %>%
  filter(rsei.media == "Direct Water Releases")

# remove nonconterminous
rsei <- rsei %>%
  filter(!state %in% c("Alaska", "Hawaii",
                       "Guam", "Puerto Rico",
                       "Virgin Islands",
                       "Northern Mariana Islands",
                       "American Samoa"))

split_rsei <- split(rsei, rsei$submission.year)
years <- as.numeric(names(split_rsei))

head(split_rsei[i])

dfList <- list() 
system.time(for(i in years) {
  #rsei_year <- split_rsei[i]
  
  rsei_year <- rsei %>%
    filter(submission.year == i)
  print(table(rsei_year$submission.year))
  # some counties are present more than once (2 in 2020, could be more for other years)
  nonunique <- data.frame(rsei_year[duplicated(rsei_year$fips),])
  nonunique_fips <- nonunique$fips
  
  #by_county_2020$nonunique_fips_flag[by_county_2020$fips %in% nonunique_fips] <- "not unique"
  #by_county_2020$nonunique_fips_flag[!by_county_2020$fips %in% nonunique_fips] <- "unique"
  
  # get only nonunique, to fix
  fix <- rsei_year %>%
    filter(fips %in% nonunique_fips)
  
  
  
  # get only nonunique, to fix
  fix <- rsei_year %>%
    filter(fips %in% nonunique_fips)# %>%
  #aggregate(df, list(rsei.score, rsei.score.cancer, rsei.score.noncancer), sum)
  
  added <- data.frame("", "", "", "")
  names(added) <- c("rsei.score", "rsei.score.cancer",
                    "rsei.score.noncancer", "fips")
  
  odd <- seq(1, by=2, len=nrow(fix)/2)
  
  for(j in odd) {
    to_add <- fix[j, c('rsei.score', 'rsei.score.cancer', 'rsei.score.noncancer')] +
      fix[j + 1, c('rsei.score', 'rsei.score.cancer', 'rsei.score.noncancer')]
    to_add$fips <- fix[j, "fips"]
    added <- rbind(to_add, added)
    j = j + 1
  }
  
  impute <- rsei_year[rsei_year$fips %in% nonunique_fips, ]
  impute <- impute[!duplicated(impute$fips), ]
  impute <- select(impute, -c("rsei.score",
                              "rsei.score.cancer",
                              "rsei.score.noncancer"))
  
  # get only unique fips, to merge with corrected data
  rsei_unique <- rsei_year %>%
    filter(!fips %in% nonunique_fips)
  
  added <- added %>%
    filter(fips != "")
  added <- added[with(added, order(fips)), ]
  impute <- impute[with(impute, order(fips)), ]
  str(impute)
  str(added)
  
  fixed <- cbind(impute, added)
  fixed <- select(fixed, -c(9))
  fixed$rsei.score <- as.numeric(fixed$rsei.score)
  fixed$rsei.score.cancer <- as.numeric(fixed$rsei.score.cancer)
  fixed$rsei.score.noncancer <- as.numeric(fixed$rsei.score.noncancer)
  
  rsei_clean <- rbind(rsei_unique, fixed)
  print(nrow(rsei_clean))
  
  assign(paste0("rsei_", i) , rsei_clean)
  
  #write.csv(rsei_clean, paste0("rsei_", i, ".csv"))

  #dfname <- paste0("rsei", i)
})

rsei_clean <- rbind(rsei_2011, rsei_2012,
                    rsei_2013, rsei_2014,
                    rsei_2015, rsei_2016,
                    rsei_2017, rsei_2018,
                    rsei_2019, rsei_2020)

rm(rsei_2011, rsei_2012,
   rsei_2013, rsei_2014,
   rsei_2015, rsei_2016,
   rsei_2017, rsei_2018,
   rsei_2019, rsei_2020)


#st_write(pop_temp, "pop_temp.shp")
# write error

#pop_tox <- pop_tox %>%
#  filter(!is.na(asian_c))
#pop_tox$rsei_score_log <- log(pop_tox$rsei.score)
#tm_shape(pop_tox) +
#  tm_polygons(col = "rsei_score_log", lwd = 0, palette = "viridis", style = "cont", main = "2019")
# in 2019, SLCounty had the highest RSEI score (and highest for noncancer) ... by far


# merging population data with toxicity data
#pop <- st_read("pop.shp")

# left join: keep all population data
pop_tox <- merge(pop_temp, rsei_clean, by.x = c("geoid", "year"), 
                 by.y = c("fips", "submission.year"), all = TRUE)

# make numeric, impute 0 for missing
pop_tox$rsei.score <- as.numeric(ifelse(is.na(pop_tox$rsei.score), 0, 
                                        pop_tox$rsei.score))
pop_tox$rsei.score.cancer <- as.numeric(ifelse(is.na(pop_tox$rsei.score.cancer), 0, 
                                               pop_tox$rsei.score.cancer))
pop_tox$rsei.score.noncancer <- as.numeric(ifelse(is.na(pop_tox$rsei.score.noncancer), 0, 
                                                  pop_tox$rsei.score.noncancer))

# add rsei binary variables for hg model
pop_tox$rsei_score_bin <- ifelse(pop_tox$rsei.score == 0, 0, 1)
pop_tox$rsei_cancer_bin <- ifelse(pop_tox$rsei.score.cancer == 0, 0, 1)
pop_tox$rsei_noncancer_bin <- ifelse(pop_tox$rsei.score.noncancer == 0, 0, 1)

write.csv(pop_tox, "data/pop_tox_merged.csv")

summary(pop_tox$rsei_noncancer_bin)

# reshaping for visualization

summary(pop_tox)

par(mfrow = c(2, 1))

plot(aggregate(pop_tox$rsei.score, by = list(pop_tox$year), FUN = mean))
plot(aggregate(pop_tox$rsei.score, by = list(pop_tox$year), FUN = max))


# rehaping for visualization only
dw <- reshape(pop_tox,
              timevar = "year",
              idvar = "geoid",
              direction = "wide")

pop_temp <- merge(geometry, dw, by = "geoid")


g <- ggplot(pop_tox, aes(x = year, y = rsei.score, 
                   group = geoid, color = state.x)) +
  geom_line(alpha = 0.1) + geom_point(size = 2) + theme_bw()

