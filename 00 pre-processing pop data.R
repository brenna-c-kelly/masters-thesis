
library(sf)
library(dplyr)
library(tidycensus)

# list of variables in acs 5-year file for 2020
vars_acs_20 <- load_variables(2020, "acs5", cache = TRUE)

# pulling 2020 ACS estimates and geometry from tidycensus
county_vars <- get_acs(geography = "county",
                       variables = c('B01001_001', 'B02001_002',
                                     'B02001_003', 'B02001_004',
                                     'B02001_005', 'B02001_006',
                                     'B02001_007', 'B02001_008',
                                     'B05010_002'),
                       geometry = TRUE,
                       year = 2020) %>%
  group_by(GEOID) %>%
  spread(variable, estimate) %>%
  select(-moe) %>%
  fill(B01001_001, B02001_002,
       B02001_003, B02001_004,
       B02001_005, B02001_006,
       B02001_007, B02001_008,
       B05010_002, .direction = "up") %>%
  drop_na()

# tidying
names(county_vars) <- tolower(names(county_vars))
county_vars$state <- substr(county_vars$geoid, start = 0, stop = 2)
noncontiguous <- c("02", "15", "72") # excluding AK, HI, PR
county_vars <- county_vars %>%
  filter(!state %in% noncontiguous)

pop <- county_vars %>%
  mutate(total = b01001_001) %>% #renaming, making percentages
  mutate(white = (b02001_002/total)*100) %>%
  mutate(black = (b02001_003/total)*100) %>%
  mutate(aian = (b02001_004/total)*100) %>%
  mutate(asian = (b02001_005/total)*100) %>%
  mutate(nhpi = (b02001_006/total)*100) %>%
  mutate(other = (b02001_007/total)*100) %>%
  mutate(tom = (b02001_008/total)*100) %>%
  mutate(pov = (b05010_002/total)*100) %>%
  mutate(population_10k = total/10000) %>%
  mutate(nonwhite = ((total - b02001_002)/total)*100) %>%
  mutate(population_10k_c = population_10k - mean(county_vars$population_10k)) %>% #centering
  mutate(nonwhite_c = nonwhite - mean(nonwhite)) %>%
  mutate(white_c = white - mean(white)) %>%
  mutate(black_c = black - mean(black)) %>%
  mutate(aian_c = aian - mean(aian)) %>%
  mutate(asian_c = asian - mean(asian)) %>%
  mutate(nhpi_c = nhpi - mean(nhpi)) %>%
  mutate(other_c = other - mean(other)) %>%
  mutate(tom_c = tom - mean(tom)) %>%
  mutate(pov_c = pov - mean(pov))

  
pop$nonwhite_c <- pop$nonwhite - mean(pop$nonwhite)
summary(pop)
rm(county_vars)

# projecting
pop <- st_as_sf(pop)
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
pop <- st_transform(pop, crs = st_crs(aea))

st_write(pop, "pop.shp")

pop_shp <- st_read("pop.shp")
