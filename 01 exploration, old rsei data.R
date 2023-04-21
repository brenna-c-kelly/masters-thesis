
# old data
library(dplyr)

rsei <- read.csv("rsei modeled county.csv")
names(rsei) <- tolower(names(rsei))

length(unique(rsei$fips)) # 1573 counties were modeled
rsei$rsei.score <- as.numeric(gsub(",", "", rsei$rsei.score))

table(rsei$submission.year) # contains all years

pop <- st_read("pop.shp")

# prep rsei to merge with pop
rsei_2020 <- rsei %>%
  filter(submission.year == 2020) %>%
  filter(!state %in% c("Hawaii", "Puerto Rico",
                       "Alaska", "American Samoa",
                       "Northern Mariana Islands",
                       "Virgin Islands"))
head(rsei_2020)
rsei_2020$fips <- str_pad(new_2020$fips, width = 5, pad = "0")
#getting n trials
facilities <- data.frame(table(new_2020$tri.facility.id))
names(facilities) <- c("tri.facility.id", "n")
facilities_county <- merge(facilities, new_2020, by = "tri.facility.id")
facilities_county <- facilities_county %>%
  filter(duplicated(fips) == FALSE)