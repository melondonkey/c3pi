library(tigris)
library(leaflet)
library(leaflet.extras)

all_counties <- counties(state="GA")

all_counties_df <- ggplot2::fortify(all_counties)


m <- leaflet(all_counties) %>%
  setView(-87.54, 33.19, 5) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons() %>%
  addSearchOSM(options=searchOptions(zoom=7))

m