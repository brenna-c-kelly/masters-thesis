

library(sf)
library(INLA)
library(spdep)
library(ggplot2)
library(regclass)
library(tidyverse)

## data
pop_tox <- read.csv("data/pop_tox_merged.csv")

pop_tox$state.x <- str_pad(pop_tox$state.x, 2, pad = "0")
pop_tox$geoid <- str_pad(pop_tox$geoid, 5, pad = 0)

pop_tox$idarea <- as.numeric(as.factor(pop_tox$geoid))
pop_tox$idarea1 <- pop_tox$idarea
pop_tox$idtime <- 1 + pop_tox$year - min(pop_tox$year)

# glms
m1 <- glm(rsei_score_bin ~ population_10k_lc +
            black_lc + aian_lc + pov_lc + epa_region +
            asian_lc + nhpi_lc + 
            other_lc + tom_lc +
            hisp_lc + pov_lc + 
            gini_c, 
          data = pop_tox, family = binomial(link = logit))
m2 <- glm(rsei.score ~ population_10k_lc +
            black_lc*pov_lc + aian_lc*pov_lc + 
            asian_lc*pov_lc + nhpi_lc*pov_lc + 
            other_lc*pov_lc + tom_lc*pov_lc +
            hisp_lc*pov_lc + pov_lc*pov_lc + gini, 
          data = subset(pop_tox, rsei_score_bin == 1),
          family = Gamma(link = log))
summary(m1)
summary(m2)

VIF(m1) # very low multicollinearity
VIF(m2)
# check
plot(m1, cex = 0.4)

# prep for inla hg
n = nrow(pop_tox)

z = as.vector(pop_tox$rsei_score_bin == 1)
y = ifelse(z == 1, pop_tox$rsei.score, NA)

z = as.vector(pop_tox$rsei_cancer_bin == 1)
y = ifelse(z == 1, pop_tox$rsei.score.cancer, NA)
mean(y, na.rm = TRUE)
summary(pop_tox$rsei.score)

nothing1 <- rep(NA, n)
nothing2 <- rep(NA, n)

zNA = as.vector(c(z, nothing1))
yNA = as.vector(c(nothing2, y))

outcome.matrix <- matrix(c(zNA, yNA), ncol=2)

mu_z <- c(rep(1, n), nothing1) # Binomial 
mu_y <- c(nothing2, rep(1,  n)) # Gamma 

x_bla <- pop_tox$black_lc
x_aia <- pop_tox$aian_lc
x_asi <- pop_tox$asian_lc
x_nhp <- pop_tox$nhpi_lc
x_tom <- pop_tox$tom_lc
x_oth <- pop_tox$other_lc
x_his <- pop_tox$hisp_lc
x_pov <- pop_tox$pov_lc
x_pop <- pop_tox$population_10k_lc
x_gin <- pop_tox$gini_c
x_epa <- pop_tox$epa_region

## Create index vectors
id_space <- pop_tox$idarea
id_time <- pop_tox$idtime

## Queens case
geometry <- st_read("data/geometry.shp")
dat_nb <- poly2nb(geometry)

## Write to file
nb2INLA("map.adj", dat_nb)
g <- inla.read.graph(filename = "map.adj")

# doubling
id_space_z <- c(id_space, nothing1) # Binomial
id_space_y <- c(nothing2, id_space) # Gamma
id_space_z2 <- c(id_space, nothing1) # Binomial
id_space_y2 <- c(nothing2, id_space) # Gamma

id_time_z <- c(id_time, nothing1) # Binomial
id_time_y <- c(nothing2, id_time) # Gamma

x_nw_z <- c(x_nonwhite, nothing1) # Binomial
x_nw_y <- c(nothing2, x_nonwhite) # Gamma

x_bla_z <- c(x_bla, nothing1) # Binomial 
x_bla_y <- c(nothing2, x_bla) # Gamma 

x_aia_z <- c(x_aia, nothing1) # Binomial 
x_aia_y <- c(nothing2, x_aia) # Gamma 

x_asi_z <- c(x_asi, nothing1) # Binomial 
x_asi_y <- c(nothing2, x_asi) # Gamma 

x_nhp_z <- c(x_nhp, nothing1) # Binomial 
x_nhp_y <- c(nothing2, x_nhp) # Gamma 

x_tom_z <- c(x_tom, nothing1) # Binomial 
x_tom_y <- c(nothing2, x_tom) # Gamma 

x_oth_z <- c(x_oth, nothing1) # Binomial 
x_oth_y <- c(nothing2, x_oth) # Gamma 

x_his_z <- c(x_his, nothing1) # Binomial 
x_his_y <- c(nothing2, x_his) # Gamma 

x_pov_z <- c(x_pov, nothing1) # Binomial
x_pov_y <- c(nothing2, x_pov) # Gamma

x_pop_z <- c(x_pop, nothing1) # Binomial
x_pop_y <- c(nothing2, x_pop) # Gamma

x_gin_z <- c(x_gin, nothing1) # Binomial
x_gin_y <- c(nothing2, x_gin) # Gamma

x_epa_z <- c(x_epa, nothing1) # Binomial
x_epa_y <- c(nothing2, x_epa) # Gamma

# list
data_hg <- list(outcome.matrix = outcome.matrix, 
                id_space_z = id_space_z, id_space_y = id_space_y,
                id_space_z2 = id_space_z2, id_space_y2 = id_space_y2,
                id_time_z = id_time_z, id_time_y = id_time_y,
                mu_z = mu_z, mu_y = mu_y,
                x_nw_z = x_nw_z, x_nw_y = x_nw_y,
                x_bla_z = x_bla_z, x_bla_y = x_bla_y,
                x_aia_z = x_aia_z, x_aia_y = x_aia_y,
                x_asi_z = x_asi_z, x_asi_y = x_asi_y,
                x_nhp_z = x_nhp_z, x_nhp_y = x_nhp_y,
                x_tom_z = x_tom_z, x_tom_y = x_tom_y,
                x_oth_z = x_oth_z, x_oth_y = x_oth_y,
                x_his_z = x_his_z, x_his_y = x_his_y,
                x_pov_z = x_pov_z, x_pov_y = x_pov_y,
                x_pop_z = x_pop_z, x_pop_y = x_pop_y,
                x_gin_z = x_gin_z, x_gin_y = x_gin_y,
                x_epa_z = x_epa_z, x_epa_y = x_epa_y)

# inla
f_hg <- outcome.matrix ~ 
  # space and time effects
  f(id_space_z, model = "bym", graph = g) +
  f(id_space_y, model = "bym", graph = g) + 
  #id_time_z + id_time_y +
  f(id_time_z, model = "iid") + 
  f(id_time_y, model = "iid") + 
  x_epa_z + x_epa_y +
  # intercepts
  mu_z + mu_y + 
  #f(x_epa_z, model = "iid") + f(x_epa_y, model = "iid") +
  # 10k population
  x_pop_z + x_pop_y +
  # inequality index
  x_pov_z + x_gin_z + x_pov_y + x_gin_y +
   #+  +
  # race covariates
  #x_bla_z*x_pov_z + x_bla_y*x_pov_y + x_aia_z*x_pov_z + x_aia_y*x_pov_y +
  #x_asi_z*x_pov_z + x_asi_y*x_pov_y + x_nhp_z*x_pov_z + x_nhp_y*x_pov_y + 
  #x_tom_z*x_pov_z + x_tom_y*x_pov_y + x_oth_z*x_pov_z + x_oth_y*x_pov_y + 
  #x_his_z*x_pov_z  + x_his_y*x_pov_y - 1 +
  x_bla_z + x_bla_y + x_aia_z + x_aia_y +
  x_asi_z + x_asi_y + x_nhp_z + x_nhp_y + 
  x_tom_z + x_tom_y + x_oth_z + x_oth_y + 
  x_his_z + x_his_y - 1
  #x_bla_z*x_pov_z*x_gin_z + x_bla_y*x_pov_y*x_gin_y + x_aia_z*x_pov_z*x_gin_z + x_aia_y*x_pov_y*x_gin_y +
  #x_asi_z*x_pov_z*x_gin_z + x_asi_y*x_pov_y*x_gin_y + x_nhp_z*x_pov_z*x_gin_z + x_nhp_y*x_pov_y*x_gin_y + 
  #x_tom_z*x_pov_z*x_gin_z + x_tom_y*x_pov_y*x_gin_y + x_oth_z*x_pov_z*x_gin_z + x_oth_y*x_pov_y*x_gin_y + 
  #x_his_z*x_pov_z*x_gin_z  + x_his_y*x_pov_y*x_gin_y - 1
#x_bla_z*x_pov_z + x_bla_y + x_aia_z*x_pov_z + x_aia_y +
#x_asi_z*x_pov_z + x_asi_y + x_nhp_z*x_pov_z + x_nhp_y + 
#x_tom_z*x_pov_z + x_tom_y + x_oth_z*x_pov_z + x_oth_y + 
#x_his_z*x_pov_z + x_his_y + x_pov_y - 1
# poverty
#x_pov_z + x_pov_y

res1 <- inla(f_hg, family = c("binomial", "gamma"), data = data_hg,
            control.compute = list(dic = TRUE, waic = TRUE),
            control.inla= list(int.strategy = "eb"),
            #control.family = c("logit", "inverse"),
            verbose=FALSE)
# s, t, intercept     150043.97
# s, t, int, pop      36381213.28
# all effects -pop    didn't run, max correction warnings
# all effects         12367088.76, nan produced
# all effects -pov    12445770.37, nan produced
# all fx, pov*his     12196906.61, nan produced
# all fx, race*pov    11771494.03, nan produced, max correction

#add redlining? epa region? gini index

summary(res)
summary(res1)

rowid <- which(res1$summary.random$country == "Japan")
res1$summary.random$country[rowid, ]


print(round(exp(res1$summary.fixed), 2))
round(res$summary.fixed, 2)

# st, all fx      12096439.43
# all fx            141713.54
# st, no intx     11826028.80
# pop*pov intx    12247180.65
# basic, gini      8923387.01
# intx, gini       9141729.74
# y intx, gini     8863133.09
# z intx, gini     9048832.47
# zintx, gini*pop  8783139.27
# yintx, gini      8894685.23
# no gini intx     8794990.91

# 10146104.81
# 10492125.86
# 10318401.91
#  9874002.64
# 10234039.15
# 10014701.88 pov*gini + race
# 10164861.91 no interactions < use
#  9382592.97 no interactions, epa <- use
#   125285.42 no interactions, epa (cleaned)

# plots
plot(res, plot.fixed.effects = TRUE,
     plot.random.effects = FALSE,
     plot.hyperparameters = FALSE,
     plot.predictor = FALSE, cex = 1.25)

#intercepts
res$marginals.fixed$mu_z %>%
  as.data.frame() %>%
  mutate(x = plogis(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()
plogis(res$summary.fixed$mean[1])
round(res$summary.fixed, 2)

res$marginals.fixed$mu_y %>%
  as.data.frame() %>%
  mutate(x = exp(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()
exp(res$summary.fixed$mean[2])

#
plot(res, plot.fixed.effects = FALSE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.predictor = FALSE, cex = 1.25)


res$marginals.fixed$mu_y %>%
  as.data.frame() %>%
  mutate(x = exp(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()


res$marginals.fixed$x_gin_y %>%
  as.data.frame() %>%
  mutate(x = plogis(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()

save(res, file = "inla_results.RData")

