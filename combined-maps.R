library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(here)
library(osmdata)
library(mapview)
library(tidyverse)
library(rnaturalearth)
library(tidygeocoder)
library(here)


combined_nightlife <- read_csv("completed-dates.csv", na = "NULL")

combined_nightlife_geo <- combined_nightlife |> 
  filter(address != "") |> 
  filter(type != "") |> 
  geocode(address = address, method = "census", verbose = TRUE, lat = latitude, long = longitude) |> 
  filter(!is.na(longitude))

write_csv(combined_nightlife_geo, "new-geocode/combined_nightlife_geo.csv")

combined_nightlife_geo_sf <- st_as_sf(combined_nightlife_geo, coords = c("longitude", "latitude"),
                                    crs = 4326)
st_write(combined_nightlife_geo_sf,
         dsn = "new-geocode/combined_nightlife_geo_sf.geojson",
         delete_dsn = TRUE)

leaflet() %>%
  addTiles() %>%
  setView(-74.00, 40.71, zoom = 11) %>%
  addMarkers(data = combined_nightlife_geo_sf)
