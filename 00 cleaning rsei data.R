
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

rsei_2020 <- rsei

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
str(impute)
str(added)

fixed <- cbind(impute, added)
fixed <- select(fixed, -c(9))
fixed$rsei.score <- as.numeric(fixed$rsei.score)
fixed$rsei.score.cancer <- as.numeric(fixed$rsei.score.cancer)
fixed$rsei.score.noncancer <- as.numeric(fixed$rsei.score.noncancer)

rsei_2020 <- rbind(rsei_2020, fixed)

# remove nonconterminous
rsei_2020 <- rsei_2020 %>%
  filter(!state %in% c("Alaska", "Hawaii",
                       "Guam", "Puerto Rico",
                       "Virgin Islands"))

rsei <- rsei %>%
  filter(!state %in% c("Alaska", "Hawaii",
                       "Guam", "Puerto Rico",
                       "Virgin Islands",
                       "Northern Mariana Islands",
                       "American Samoa"))

# merging population data with toxicity data
#pop <- st_read("pop.shp")
library(INLA)

# left join: keep all population data

pop_tox <- merge(pop_temp_nogeom, rsei, by.x = c("geoid", "year"), 
                 by.y = c("fips", "submission.year"), all = TRUE)

# make numeric, impute 0 for missing
pop_tox$rsei.score <- as.numeric(ifelse(is.na(pop_tox$rsei.score), 0, 
                                        pop_tox$rsei.score))
pop_tox$rsei.score.cancer <- as.numeric(ifelse(is.na(pop_tox$rsei.score.cancer), 0, 
                                               pop_tox$rsei.score.cancer))
pop_tox$rsei.score.noncancer <- as.numeric(ifelse(is.na(pop_tox$rsei.score.noncancer), 0, 
                                                  pop_tox$rsei.score.noncancer))


#     reshaping for INLA

test <- test_2015 %>%
  filter(!geoid %in% test_2011$geoid)

test_2011 <- pop_tox %>%
  filter(year == 2011)
test_2015 <- pop_tox %>%
  filter(year == 2015)


john_parish <- pop_tox %>%
  #filter(year %in% c(2016:2020)) %>%
  filter(geoid == "22095")

not_etc <- john_parish[1:4, 1:32]
etc_john_parish <- john_parish[5:16, ]
#john_parish_fix <- etc_john_parish[, 1:29]



#john_parish[7:16, 1]

#cbind(etc_john_parish[, 1:29], aggregate(etc_john_parish[30:32], by = list(etc_john_parish$year), FUN = sum))

rsei_john <- aggregate(etc_john_parish[30:32], by = list(etc_john_parish$year), FUN = sum)
john_parish_fixed <- merge(john_parish[, 1:29], rsei_john, by.x = c("year"), , by.y = c("Group.1"), all = FALSE)

rsei_john <- john_parish_fixed[duplicated(john_parish_fixed$year), ]

rsei_john_fixed <- rbind(not_etc, rsei_john)
names(rsei_john_fixed)
john_parish <- select(john_parish, -c("Group.1"))

pop_tox <- pop_tox %>%
  filter(!geoid == "22095")

pop_tox <- rbind(rsei_john_fixed, pop_tox)
table(pop_tox$year)


table(pop_tox$geoid == "22095")


# add rsei binary variable, for hg model
pop_tox$rsei_bin <- 

pop_tox$rsei_score_bin <- ifelse(pop_tox$rsei.score == 0, 0, 1)
pop_tox$rsei_cancer_bin
pop_tox$rsei_noncancer_bin



dw <- reshape(pop_tox,
              timevar = "year",
              idvar = "geoid",
              direction = "wide")
pop_temp <- merge(geometry, dw, by = "geoid")

#st_write(pop_temp, "pop_temp.shp")
# write error

#pop_tox <- pop_tox %>%
#  filter(!is.na(asian_c))
#pop_tox$rsei_score_log <- log(pop_tox$rsei.score)
#tm_shape(pop_tox) +
#  tm_polygons(col = "rsei_score_log", lwd = 0, palette = "viridis", style = "cont", main = "2019")
# in 2019, SLCounty had the highest RSEI score (and highest for noncancer) ... by far
