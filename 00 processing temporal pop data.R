
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyverse)
library(tidycensus)

# list of variables in acs 5-year file for 2020
vars_acs_20 <- load_variables(20, "acs5", cache = TRUE)

# pulling 2011-2020 ACS estimates and geometry from tidycensus

for(i in 2011:2020) {
  county_vars <- get_acs(geography = "county",
                         variables = c('B01001_001', 'B03002_003',
                                       'B03002_004', 'B03002_005',
                                       'B03002_006', 'B03002_007',
                                       'B03002_008', 'B03002_009', 
                                       'B03002_012', 'B05010_002'),
                         geometry = FALSE,
                         year = i) %>%
    group_by(GEOID) %>%
    spread(variable, estimate) %>%
    select(-moe) %>%
    fill(B01001_001, B03002_003,
         B03002_004, B03002_005,
         B03002_006, B03002_007,
         B03002_008, B03002_009, 
         B03002_012, B05010_002, .direction = "up") %>%
    drop_na()
  
  # tidying
  names(county_vars) <- tolower(names(county_vars))
  county_vars$state <- substr(county_vars$geoid, start = 0, stop = 2)
  noncontiguous <- c("02", "15", "72") # excluding AK, HI, PR
  county_vars <- county_vars %>%
    filter(!state %in% noncontiguous)
  
  pop <- county_vars %>%
    # renaming, making percentages
    mutate(total = b01001_001) %>% 
    mutate(white = (b03002_003/total)*100) %>%
    mutate(black = (b03002_004/total)*100) %>%
    mutate(aian = (b03002_005/total)*100) %>%
    mutate(asian = (b03002_006/total)*100) %>%
    mutate(nhpi = (b03002_007/total)*100) %>%
    mutate(other = (b03002_008/total)*100) %>%
    mutate(tom = (b03002_009/total)*100) %>%
    mutate(hisp = (b03002_012/total)*100) %>%
    mutate(pov = (b05010_002/total)*100) %>%
    mutate(population_10k = total/10000) %>%
    mutate(nonwhite = ((total - b03002_003)/total)*100) %>%
    select(-c(b01001_001, b03002_003, b03002_004, 
              b03002_005, b03002_006, b03002_007,
              b03002_008, b03002_009, b03002_012, 
              b05010_002))
  # centering
  pop$population_10k_c <- pop$population_10k - mean(pop$population_10k)  
  pop$nonwhite_c <- pop$nonwhite - mean(pop$nonwhite)
  pop$white_c <- pop$white - mean(pop$white)
  pop$black_c <- pop$black - mean(pop$black)
  pop$aian_c <- pop$aian - mean(pop$aian)
  pop$asian_c <- pop$asian - mean(pop$asian)
  pop$nhpi_c <- pop$nhpi - mean(pop$nhpi)
  pop$other_c <- pop$other - mean(pop$other)
  pop$tom_c <- pop$tom - mean(pop$tom)
  pop$hisp_c <- pop$hisp - mean(pop$hisp)
  pop$pov_c <- pop$pov - mean(pop$pov)
  pop$year <- i
  if(i %in% c(2011, 2012, 2013, 2014)) {
    pop$geoid = ifelse(pop$geoid == 46113, 46102, pop$geoid)
  }
  write.csv(pop, paste("/Users/brenna/Documents/School/Thesis/masters-thesis/data/temporal data/pop_data", i, ".csv", sep = ""))
}
getwd()

list.files(path = "data/temporal data", pattern = "\\csv$")

pop_temp <- map(list.files("data/temporal data", full.names = T, pattern = "\\csv$"), read.csv) %>%
  bind_rows()

pop_temp$geoid <- str_pad(pop_temp$geoid, width = 5, pad = "0")

table(pop_temp$year)


# 2011-2013 have unique matches

# in 2015, shannon county (46113) was renamed oglala lakota county (46102)
#     recode 46113 to 46102
# ^ implement this in the loop above

# for 2018, Rio Arriba County will be averaged between 2017/2019
rio_arriba <- pop_temp %>%
  filter(geoid == "35039")
names(rio_arriba)
rio_arriba_num <- rio_arriba[7:8, 5:28] # only average numeric, only for 2017 & 2019
rio_arriba_2018 <- data.frame(rio_arriba[1, 1], 
                              rio_arriba[1, 2],
                              rio_arriba[1, 3],
                              rio_arriba[1, 4],
                              (rio_arriba_num[1, ] + rio_arriba_num[2, ])/2)
names(rio_arriba_2018) <- names(rio_arriba)
# combine with pop_temp data:
pop_temp <- rbind(pop_temp, rio_arriba_2018)
rm(rio_arriba_2018, rio_arriba)

# in 2014, bedford city (51515) was absorbed into bedford county (51019)
#     in years with 51515, sum with 51019
ftable(pop_temp$year, pop_temp$geoid == 51515) # only in 2011-2013

bedford <- pop_temp %>%
  filter(year %in% c(2011, 2012, 2013)) %>%
  filter(geoid %in% c(51515, 51019))
names(bedford)
bedford_num <- bedford[, 5:28]
names(bedford_num)
bedford_num[, 2:10] <- (bedford_num[, 2:10]/100) * bedford_num[, 1]
bedford_num
bedford_sum <- data.frame(bedford[1, 1],
                          bedford[1, 2],
                          bedford[1, 3],
                          bedford[1, 4],
                          (aggregate(bedford_num[1:11], by = list(group = bedford_num$year), FUN = sum))) %>%
  relocate(group, .after = population_10k)
names(bedford_sum) <- c(names(pop_temp[1:15]), "year")

bedford_sum <- bedford_sum %>%
  mutate(nonwhite = ((total - white)/total)*100) %>%
  mutate(white = (white/total)*100) %>%
  mutate(black = (black/total)*100) %>%
  mutate(aian = (aian/total)*100) %>%
  mutate(asian = (asian/total)*100) %>%
  mutate(nhpi = (nhpi/total)*100) %>%
  mutate(other = (other/total)*100) %>%
  mutate(tom = (tom/total)*100) %>%
  mutate(hisp = (hisp/total)*100) %>%
  mutate(pov = (pov/total)*100) %>%
  mutate(population_10k_c = population_10k - mean(pop$population_10k)) %>%
  mutate(nonwhite_c = nonwhite - mean(pop$nonwhite)) %>%
  mutate(white_c = white - mean(pop$white)) %>%
  mutate(black_c = black - mean(pop$black)) %>%
  mutate(aian_c = aian - mean(pop$aian)) %>%
  mutate(asian_c = asian - mean(pop$asian)) %>%
  mutate(nhpi_c = nhpi - mean(pop$nhpi)) %>%
  mutate(other_c = other - mean(pop$other)) %>%
  mutate(tom_c = tom - mean(pop$tom)) %>%
  mutate(hisp_c = hisp - mean(pop$hisp)) %>%
  mutate(pov_c = pov - mean(pop$pov)) %>%
  relocate(year, .after = pov_c)

names(bedford_sum) <- names(pop_temp)

bedford_clean <- pop_temp %>%
  filter(geoid %in% c(51515, 51019)) %>%
  filter(!year %in% c(2011, 2012, 2013))

bedford_all <- rbind(bedford_sum, bedford_clean)
bedford_all

pop_temp <- pop_temp %>%
  filter(!geoid %in% c(51515, 51019))

pop_temp <- rbind(pop_temp, bedford_all)


geometry <- get_acs(geography = "county",
                       variables = c('B01001_001'),
                       geometry = TRUE,
                       year = 2020)
names(geometry) <- tolower(names(geometry))
geometry <- select(geometry, -c(name, variable, estimate, moe))

pop_temp_geom <- merge(geometry, pop_temp, by = "geoid")
# keep in mind, the dataframe will be reshaped for the model
# geometry will only be used once, to make the graph
# inla will use the id associated with the geometry

# projecting
pop_temp_geom <- st_as_sf(pop_temp_geom)
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
pop_temp_geom <- st_transform(pop_temp_geom, crs = st_crs(aea))


pop_temp <- st_as_sf(pop_temp)
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
pop_temp <- st_transform(pop_temp, crs = st_crs(aea))

# figure
ggplot(pop_temp_geom) + 
  geom_sf(aes(fill = nonwhite_c)) +
  facet_wrap(~year, dir = "h", ncol = 7) +
  ggtitle("nonwhite population") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  )

names(pop_temp_geom)



#st_write(pop, "pop.shp", append = FALSE)

#pop_shp <- st_read("pop.shp")

