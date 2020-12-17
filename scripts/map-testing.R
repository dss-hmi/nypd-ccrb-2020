nypd_sf <- rgdal::readOGR(
  dsn = paste0(getwd(),"/data-unshared/raw/Police_Precincts")
  ,layer = "geo_export_7265b328-312d-4c37-a8ec-ed1819dc35e5"
  ,verbose = FALSE
)



nypd_ready <- broom::tidy(nypd_sf, region = 'precinct')



library(ggplot2)
g <- ggplot() +
  geom_polygon(data = nypd_ready, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void()

g

# ---- ggmap ----

library(ggmap)

location <- "New York City"

nyc_map <- get_googlemap(center = location)

g2 <- ggmap(nyc_map) +
  geom_polygon(
    data = nypd_ready
    ,aes( x = long, y = lat, group = group)
    ,color="black"
    ,fill = "white"
    ,alpha = 0.2)



g2

# --- leaflet ----

library(leaflet)

map_leaf <- leaflet(data =  nypd_sf) %>%
  addTiles() %>%
  addPolygons(color = "Black")

map_leaf
