
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

# remove nonconterminous
rsei_2020 <- rsei_2020 %>%
  filter(!state %in% c("Alaska", "Hawaii",
                       "Guam", "Puerto Rico",
                       "Virgin Islands"))

# merging population data with toxicity data
pop <- st_read("pop.shp")

# left join: keep all population data
pop_tox <- merge(pop, rsei_2020, by.x = "geoid", by.y = "fips", all = TRUE)

# make numeric, impute 0 for missing
pop_tox$rsei.score <- as.numeric(ifelse(is.na(pop_tox$rsei.score), 0, 
                                        pop_tox$rsei.score))
pop_tox$rsei.score.cancer <- as.numeric(ifelse(is.na(pop_tox$rsei.score.cancer), 0, 
                                               pop_tox$rsei.score.cancer))
pop_tox$rsei.score.noncancer <- as.numeric(ifelse(is.na(pop_tox$rsei.score.noncancer), 0, 
                                                  pop_tox$rsei.score.noncancer))

