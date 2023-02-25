
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyverse)
library(tidycensus)

# list of variables in acs 5-year file for 2020
vars_acs_20 <- load_variables(2020, "acs5", cache = TRUE)

# pulling 2011-2020 ACS estimates and geometry from tidycensus

n = 10
datalist = vector("list", length = n)

disability_vars <- get_acs(geography = "county",
                           variables = c("B19083_001"),
                           geometry = FALSE,
                           year = 2020) %>%
  group_by(GEOID) %>%
  spread(variable, estimate) %>%
  drop_na() #%>%

ginilist = vector("list", length = 10)
for(i in 2011:2020) {
  gini <- get_acs(geography = "county",
                  variables = c("B19083_001"),
                  geometry = FALSE,
                  year = i) %>%
    group_by(GEOID) %>%
    spread(variable, estimate) %>%
    select(-moe) %>%
    drop_na()
  # tidying
  names(gini) <- tolower(names(gini))
  gini$state <- substr(gini$geoid, start = 0, stop = 2)
  noncontiguous <- c("02", "15", "72") # excluding AK, HI, PR
  gini <- gini %>%
    filter(!state %in% noncontiguous)
  gini$year <- i
  
  if(i %in% c(2011, 2012, 2013, 2014)) {
    gini$geoid = ifelse(gini$geoid == 46113, 46102, gini$geoid)
  }
  ginilist[[i]] <- gini
}

ginilist = do.call(rbind, ginilist)
ginilist$gini <- ginilist$b19083_001

geoids <- data.frame(table(ginilist$geoid))
subset(!pop_tox$geoid %in% geoids$Var1)
subset(pop_tox, !(geoid %in% geoids$Var1))
subset(geoids, !(Var1 %in% pop_tox$geoid))

head(ginilist)

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
  pop$year <- i
  
  if(i %in% c(2011, 2012, 2013, 2014)) {
    pop$geoid = ifelse(pop$geoid == 46113, 46102, pop$geoid)
  }
  datalist[[i]] <- pop
}

pop_temp = do.call(rbind, datalist)

pop_temp$geoid <- str_pad(pop_temp$geoid, width = 5, pad = "0")

table(pop_temp$year)

pop_temp <- cbind(pop_temp, ginilist$gini)
names(pop_temp)[17] <- "gini"

# 2011-2013 have unique matches

# in 2015, shannon county (46113) was renamed oglala lakota county (46102)
#     recode 46113 to 46102
# ^ implement this in the loop above

# for 2018, Rio Arriba County will be averaged between 2017/2019
rio_arriba <- pop_temp %>%
  filter(geoid == "35039")

rio_arriba_num <- rio_arriba[7:8, 4:17] # only average numeric, only for 2017 & 2019
rio_arriba_2018 <- data.frame(rio_arriba[1, 1], 
                              rio_arriba[1, 2],
                              rio_arriba[1, 3],
                              #rio_arriba[1, 4],
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
bedford_num <- bedford[, 4:17] # only numeric
names(bedford_num)
bedford_num[, c(2:10, 12)] <- (bedford_num[, c(2:10, 12)]/100) * bedford_num$total


bedford_sum <- data.frame(bedford[1, 1],
                          bedford[1, 2],
                          bedford[1, 3],
                          #bedford[1, 4],
                          (aggregate(bedford_num[1:11], by = list(group = bedford_num$year), FUN = sum)),
                          (aggregate(bedford_num[, "gini"], by = list(group = bedford_num$year), FUN = mean))) %>%
  relocate(group, .after = population_10k) %>%
  select(-group.1)
names(bedford_sum) <- c(names(pop_temp[1:14]), "year", "gini")

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
  #mutate(population_10k_c = population_10k - mean(pop$population_10k)) %>%
  #mutate(nonwhite_c = nonwhite - mean(pop$nonwhite)) %>%
  #mutate(white_c = white - mean(pop$white)) %>%
  #mutate(black_c = black - mean(pop$black)) %>%
  #mutate(aian_c = aian - mean(pop$aian)) %>%
  #mutate(asian_c = asian - mean(pop$asian)) %>%
  #mutate(nhpi_c = nhpi - mean(pop$nhpi)) %>%
  #mutate(other_c = other - mean(pop$other)) %>%
  #mutate(tom_c = tom - mean(pop$tom)) %>%
  #mutate(hisp_c = hisp - mean(pop$hisp)) %>%
  #mutate(pov_c = pov - mean(pop$pov)) %>%
  relocate(year, .after = nonwhite) %>%
  relocate(gini, .after = year)

names(bedford_sum) <- names(pop_temp)

bedford_clean <- pop_temp %>%
  filter(geoid %in% c(51515, 51019)) %>%
  filter(!year %in% c(2011, 2012, 2013))

bedford_sum

bedford_all <- rbind(bedford_sum, bedford_clean)
bedford_all

pop_temp <- pop_temp %>%
  filter(!geoid %in% c(51515, 51019))

pop_temp <- rbind(pop_temp, bedford_all)

# imputing small value if 0, to prep for log transformation
#     get the nonzero minimum, for imputation
pop_temp <- pop_temp %>%
  mutate(black_z = ifelse(black == 0, min(pop_temp$black[pop_temp$black > 0]),
                          black + min(pop_temp$black[pop_temp$black > 0]))) %>%
  mutate(aian_z = ifelse(aian == 0, min(pop_temp$aian[pop_temp$aian > 0]),
                         aian + min(pop_temp$aian[pop_temp$aian > 0]))) %>%
  mutate(asian_z = ifelse(asian == 0, min(pop_temp$asian[pop_temp$asian > 0]), 
                          asian + min(pop_temp$asian[pop_temp$asian > 0]))) %>%
  mutate(nhpi_z = ifelse(nhpi == 0, min(pop_temp$nhpi[pop_temp$nhpi > 0]), 
                         nhpi + min(pop_temp$nhpi[pop_temp$nhpi > 0]))) %>%
  mutate(other_z = ifelse(other == 0, min(pop_temp$other[pop_temp$other > 0]), 
                          other + min(pop_temp$other[pop_temp$other > 0]))) %>%
  mutate(tom_z = ifelse(tom == 0, min(pop_temp$tom[pop_temp$tom > 0]), 
                        tom + min(pop_temp$tom[pop_temp$tom > 0]))) %>%
  mutate(hisp_z = ifelse(hisp == 0, min(pop_temp$hisp[pop_temp$hisp > 0]), 
                         hisp + min(pop_temp$hisp[pop_temp$hisp > 0]))) %>%
  mutate(pov_z = ifelse(pov == 0, min(pop_temp$pov[pop_temp$pov > 0]), 
                        pov + min(pop_temp$pov[pop_temp$pov > 0]))) %>%
  mutate(nonwhite_z = ifelse(nonwhite == 0, min(pop_temp$nonwhite[pop_temp$nonwhite > 0]),
                             nonwhite + min(pop_temp$nonwhite[pop_temp$nonwhite > 0]))) %>%
  mutate(white_z = ifelse(white == 0, min(pop_temp$white[pop_temp$white > 0]),
                          white + min(pop_temp$white[pop_temp$white > 0])))

# log transformed, centered on mean
pop_temp$population_10k_lc <- log(pop_temp$population_10k) - 
  log(mean(pop_temp$population_10k))  
pop_temp$nonwhite_lc <- log(pop_temp$nonwhite_z) - 
  log(mean(pop_temp$nonwhite_z))
pop_temp$white_lc <- log(pop_temp$white_z) - 
  log(mean(pop_temp$white_z))
pop_temp$black_lc <- log(pop_temp$black_z) - 
  log(mean(pop_temp$black_z))
pop_temp$aian_lc <- log(pop_temp$aian_z) - 
  log(mean(pop_temp$aian_z))
pop_temp$asian_lc <- log(pop_temp$asian_z) - 
  log(mean(pop_temp$asian_z))
pop_temp$nhpi_lc <- log(pop_temp$nhpi_z) - 
  log(mean(pop_temp$nhpi_z))
pop_temp$other_lc <- log(pop_temp$other_z) - 
  log(mean(pop_temp$other_z))
pop_temp$tom_lc <- log(pop_temp$tom_z) - 
  log(mean(pop_temp$tom_z))
pop_temp$hisp_lc <- log(pop_temp$hisp_z) - 
  log(mean(pop_temp$hisp_z))
pop_temp$pov_lc <- log(pop_temp$pov_z) - 
  log(mean(pop_temp$pov_z))



# centering
pop_temp$population_10k_c <- pop_temp$population_10k - mean(pop_temp$population_10k)  
pop_temp$nonwhite_c <- pop_temp$nonwhite - mean(pop_temp$nonwhite)
pop_temp$white_c <- pop_temp$white - mean(pop_temp$white)
pop_temp$black_c <- pop_temp$black - mean(pop_temp$black)
pop_temp$aian_c <- pop_temp$aian - mean(pop_temp$aian)
pop_temp$asian_c <- pop_temp$asian - mean(pop_temp$asian)
pop_temp$nhpi_c <- pop_temp$nhpi - mean(pop_temp$nhpi)
pop_temp$other_c <- pop_temp$other - mean(pop_temp$other)
pop_temp$tom_c <- pop_temp$tom - mean(pop_temp$tom)
pop_temp$hisp_c <- pop_temp$hisp - mean(pop_temp$hisp)
pop_temp$pov_c <- pop_temp$pov - mean(pop_temp$pov)
pop_temp$gini_c <- pop_temp$gini - mean(pop_temp$gini)


write.csv(pop_temp, "data/pop_temp.csv")

geometry <- get_acs(geography = "county",
                       variables = c('B01001_001'),
                       geometry = TRUE,
                       year = 2020)

aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
geometry <- st_as_sf(geometry, crs = st_crs(aea))
geometry <- st_transform(geometry, aea)

names(geometry) <- tolower(names(geometry))
geometry <- select(geometry, -c(variable, estimate, moe))
geometry$state <- substr(geometry$geoid, 1, 2)
geometry <- geometry %>%
  filter(!state %in% noncontiguous)

st_write(geometry, "data/geometry.shp")

# keep in mind, the dataframe will be reshaped for the model
# geometry will only be used once, to make the graph
# inla will use the id associated with the geometry

# projecting
pop_temp_geom <- st_as_sf(pop_temp_geom)
aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
pop_temp_geom <- st_transform(pop_temp_geom, crs = st_crs(aea))

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


state_geo <- get_acs(geography = "state",
                     variables = c('B01001_001'),
                     geometry = TRUE,
                     year = 2020)

aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
state_geo <- st_as_sf(state_geo, crs = st_crs(aea))
state_geo <- st_transform(state_geo, aea)

names(state_geo) <- tolower(names(state_geo))
state_geo <- select(state_geo, -c(variable, estimate, moe))
state_geo$state <- substr(state_geo$geoid, 1, 2)
state_geo <- state_geo %>%
  filter(!state %in% noncontiguous)
tm_shape(state_geo) + tm_polygons()

st_write(state_geo, "data/state_geometry.shp")

#st_write(pop, "pop.shp", append = FALSE)

#pop_shp <- st_read("pop.shp")

