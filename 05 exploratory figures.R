
library(sf)
library(tmap)
library(dplyr)
library(viridis)
library(ggplot2)
library(stringr)
library(corrplot)
library(gghighlight)
library(wesanderson)
library(RColorBrewer)

# figures for exploration, possibly methods, descriptive results, discussion 

pop_tox <- read.csv("data/pop_tox_merged.csv")
pop_tox$geoid <- str_pad(pop_tox$geoid, 5, pad = 0)

geometry <- st_read("data/geometry.shp")

dat <- merge(geometry, pop_tox, by = "geoid", all = TRUE)

dat$rsei.score_log <- log(dat$rsei.score)

scores_time <- data.frame(aggregate(pop_tox$rsei.score, by = list(pop_tox$year), FUN = mean),
                     aggregate(pop_tox$rsei.score, by = list(pop_tox$year), FUN = sd),
                     aggregate(pop_tox$rsei.score.cancer, by = list(pop_tox$year), FUN = mean),
                     aggregate(pop_tox$rsei.score.cancer, by = list(pop_tox$year), FUN = sd),
                     aggregate(pop_tox$rsei.score.noncancer, by = list(pop_tox$year), FUN = mean),
                     aggregate(pop_tox$rsei.score.noncancer, by = list(pop_tox$year), FUN = sd)) %>%
  select(-c(Group.1, Group.1.2, Group.1.3, Group.1.4, Group.1.5)) %>%
  rename(x)



ggplot(data = pop_tox, 
       aes(x = year, y = rsei.score, 
           ymin = min(pop_tox$rsei.score), 
           ymax = max(pop_tox$rsei.score)) + #, fill = type, linetype=type)) + 
  geom_line() + 
  geom_ribbon(alpha=0.5) + 
  scale_x_log10() + 
  scale_y_log10() + 
  xlab(as.expression(expression( paste("Radius (", R[500], ")") ))) + 
  ylab("Scaled Temperature")



geom_errorbar(mapping = NULL, data = NULL, stat = “identity”, position = “identity”, …)

ggplot(gap_subset, aes(x = year, y = lifeExp,
                       colour = country)) +
  geom_line(size = 0.75) +
  theme_bw() +
  labs(x = "Year", y = "Life Expectancy (y)") +
  ggtitle("Life Expectancy Comparison Between \nMontenegro and Morocco") +
  scale_colour_brewer(palette = "Dark2")


cor.test(pop_tox$population_10k, pop_tox$asian)

M = cor(pop_tox[, c(3, 6:17, 42:47)])

corrplot(M, method = 'number')

dat$population_10k_log <- log(dat$population_10k)

ggplot(dat, aes(year, rsei.score_log, color=population_10k)) +
  geom_point(alpha = 0.1, cex = 2) +
  labs(y = "Life Expectancy (y)", x = "Year") +
  ggtitle("Life Expectancy for All Countries by Year") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_color_viridis(option = "D")
  #scale_fill_manual(values = c(RColorBrewer::brewer.pal(8, "YlGnBu")))

c(RColorBrewer::brewer.pal(8, "YlGnBu"))

g <- ggplot(pop_tox, aes(x = year, y = rsei.score, 
                         group = geoid, color = state.y)) +
  geom_line() + geom_point(size = 2) + theme_bw()
g <- g + theme(legend.position = "none")
library(gghighlight)
g + gghighlight(geoid == "49035")

library(plotly)
ggplotly(g)
#wow utah

texas <- pop_tox %>%
  filter(state.x == "48")
g <- ggplot(texas, aes(x = year, y = rsei.score, 
                         group = geoid, color = geoid)) +
  geom_line() + geom_point(size = 2) + theme_bw()
g <- g + theme(legend.position = "none")
library(gghighlight)
g + gghighlight(geoid == c("49035", "49049"))

library(plotly)
ggplotly(g)

texas <- dat %>%
  filter(state == "48")
utah <- dat %>%
  filter(state == "49")
alabama <- dat %>%
  filter(state == "01")
tn <- dat %>%
  filter(state == "47")
ny <- dat %>%
  filter(state %in% "36")

epa <- read.csv("data/epa regions.csv")
names(epa) <- tolower(names(epa))
epa_regions <- split(epa, epa$epa.region)

dat <- dat %>%
  mutate(epa_region = case_when(state %in% epa_regions$`1`$state.code ~ 1,
                                state %in% epa_regions$`2`$state.code ~ 2,
                                state %in% epa_regions$`3`$state.code ~ 3,
                                state %in% epa_regions$`4`$state.code ~ 4,
                                state %in% epa_regions$`5`$state.code ~ 5,
                                state %in% epa_regions$`6`$state.code ~ 6,
                                state %in% epa_regions$`7`$state.code ~ 7,
                                state %in% epa_regions$`8`$state.code ~ 8,
                                state %in% epa_regions$`9`$state.code ~ 9,
                                state %in% epa_regions$`10`$state.code ~ 10,
                                state %in% epa_regions$`25`$state.code ~ 25))



table(tn$county)

states <- unique(dat$state)
for(i in states) {
  state <- dat %>%
    filter(state == i)
  tm_shape(state) +
    tm_polgyons(col = "rsei.score", lwd = 0, palette)
}

current.mode <- tmap_mode("plot")

pa <- dat %>%
  filter(state %in% "42")

tm_shape(pa) +
  tm_polygons(col = "rsei.score", lwd = 0, palette = "viridis", style = "cont") + 
  #tm_shape(ne) +
  #tm_polygons(col = "rsei_score_bin", lwd = 0, palette = "viridis", style = "cat") +
  tm_facets(by = "year", ncol = 4, nrow = 3) +
  tm_layout(frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA)

tm_shape(dat) +
  tm_polygons(col = "rsei_score_bin", lwd = 0, palette = "Dark2", style = "cat") +
  tm_facets(by = "year", ncol = 5, nrow = 2) +
  tm_layout(frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA)


by_state <- data.frame(aggregate(dat$rsei.score, by = list(dat$state), FUN = mean), 
                       aggregate(dat$rsei.score, by = list(dat$state), FUN = sd))


rsei_by_state <- data.frame(table(dat$state, dat$rsei_score_bin, dat$year))
rsei_by_state <- rsei_by_state %>%
  spread(Var2, Freq) %>%
  group_by(Var1, Var3) %>%
  fill(.direction = "up") %>%
  distinct() %>%
  mutate(prop_polluted = `1`/(`1`+`0`))

state_geo <- st_read("data/state_geometry.shp")
head(state_geo)
head(rsei_by_state)
most_polluted <- merge(state_geo, rsei_by_state, by.x = "geoid", by.y = "Var1")

hist(rsei_by_state$prop_polluted)

g <- ggplot(rsei_by_state, aes(x = Var3, y = prop_polluted, 
                       group = Var1, color = Var1)) +
  geom_line() + geom_point(size = 2) + theme_bw()
g <- g + theme(legend.position = "none")
g + gghighlight(Var1 == c("01"))


# epa region
tm_shape(dat) +
  tm_polygons(col = "rsei_score_bin", lwd = 0, palette = "Dark2", style = "cat") +
  tm_facets(by = "year", ncol = 5, nrow = 2) +
  tm_layout(frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA)

year_epa <- aggregate(dat$rsei.score, by = list(dat$epa_region, dat$year), FUN = mean)
year_epa$Group.1 <- as.factor(year_epa$Group.1)
pal <- wes_palette("Zissou1", n = 10, type = "continuous")
head(year_epa)
names(year_epa) <- c("EPA Region", "Year", "Mean Pollution")

g <- ggplot(year_epa, aes(x = `Year`, y = `Mean Pollution`, group = `EPA Region`, colour = `EPA Region`))+#,
                     #group = `Group.1`, fill = as.factor(`Group.1`))) +
  geom_line(aes(linewidth = `Mean Pollution`/3000)) +
  scale_color_manual(values = pal) +
  geom_point( size = 2) + theme_bw() + 
  scale_fill_manual(values = pal_vals)
g <- g + theme(legend.position = "none")
g + gghighlight(Group.1 == c("8"))
summary(year_epa$x)
pal_vals <- pal[1:10]

library(wesanderson)
wes_palettes
wes_palette()



