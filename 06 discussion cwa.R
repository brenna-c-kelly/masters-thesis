

head(cwa$clustername)

cwa <- read.csv("../defense/CWA violations.csv")

cwa_state <- merge(st, cwa, by.x = "name", by.y = "clustername")

tm_shape(cwa_state) +
  tm_polygons(col = "cvrows", lwd = 0, style = "cont", palette = "YlOrRd")
tm_shape(cwa_state) +
  tm_polygons(col = "v3rows", lwd = 0, style = "cont", palette = "YlOrRd")
tm_shape(cwa_state) +
  tm_polygons(col = "totalpenalties", lwd = 0, style = "cont", palette = "YlOrRd")

tm_shape(cwa_state) +
  tm_polygons(col = "viol_per_facility", lwd = 0, style = "cont", palette = "YlOrRd")


names(cwa) <- tolower(names(cwa))
names(cwa)
sum(cwa$violast4qrows)

cwa_state$action_per_violation <- cwa_state$svrows / cwa_state$v3rows

cwa_state$viol_per_facility <- cwa_state$v3rows / cwa_state$clustercount

sum(cwa$v3rows) / sum(cwa$clustercount)


head(st)


table(cwa$clustername, cwa$v3rows)

sum(cwa$svrows)

sum(cwa$totalpenalties) / sum(cwa$v3rows)

tri$county_st <- paste(tri$county, tri$st)