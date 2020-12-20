nypd_sf <- rgdal::readOGR(
  dsn = paste0(getwd(),"/data-unshared/raw/Police_Precincts")
  ,layer = "geo_export_7265b328-312d-4c37-a8ec-ed1819dc35e5"
  ,verbose = FALSE
)


nypd_json <- rgdal::readOGR(
  paste0(getwd(),"/data-unshared/raw/policeprecincts.geojson"))


nypd_ready <- broom::tidy(nypd_sf, region = 'precinct')


# allows for mergeing addational data

nypd_sf_package <- geojsonsf::geojson_sf("./data-unshared/raw/policeprecincts.geojson")


path_input <- "./data-unshared/raw/CCRB-Complaint-Data_202007271729/allegations_202007271729.csv"

library(tidyverse)

ds0 <- readr::read_csv(path_input)

ds1 <- ds0 %>% group_by(precinct) %>% count() %>% drop_na()


merge_data <- nypd_sf_package %>% mutate(across(precinct, as.numeric)) %>% left_join(ds1)


library(ggplot2)
g <- ggplot() +
  geom_polygon(data = nypd_ready, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void()

g

# ---- ggmap ----
#could use for publications

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
# use for shiny

library(leaflet)

map_leaf <- leaflet(data =  nypd_sf) %>%
  addTiles() %>%
  addPolygons(color = "Black")

map_leaf



map_sf_package <- leaflet(data =  nypd_sf_package) %>%
  addTiles() %>%
  addPolygons(color = "black", highlightOptions = highlightOptions(color = "white", weight = 2,
              bringToFront = TRUE),label = ~glue::glue("This is: {precinct}"))

map_sf_package
# see leaflet doc for explanation

pal <- colorNumeric(
  palette = "Blues"
  ,domain = ds1$n
  ,na.color = NA
)

map_sf_package_color <- leaflet(data =  merge_data) %>%
  addTiles() %>%
  addPolygons(
    color = "black"
    ,weight = 1
    ,fillOpacity = 0.8
    ,fillColor = ~pal(n)
    ,highlightOptions = highlightOptions(
      color = "white"
      ,weight = 2
      ,bringToFront = TRUE
      )
    ,label = ~glue::glue(
      "This is: {precinct}",
      "They had {n} complaints" ))

map_sf_package_color
