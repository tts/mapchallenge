library(tidyverse)
library(sf)
library(rnaturalearth)

#https://stackoverflow.com/a/43250903
crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
        +datum=WGS84 +units=m +no_defs"

ctrys50m <- ne_countries(scale = 50, type = "countries", returnclass = "sf") %>%
  select(iso_a3, iso_n3, admin)

sphere <- st_graticule(st_transform(ctrys50m, crs = crs)) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))

# Ericsson Globe in Stockholm https://en.wikipedia.org/wiki/Ericsson_Globe
globe <- tribble(
  ~place, ~latitude, ~longitude, 
  "place", 59.29350, 18.083137
)

globe_sf <- globe %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

ggplot()  +
  geom_sf(data = sphere, fill = "#D8F4FF", alpha = 0.7) +
  geom_sf(data = ctrys50m, fill = "grey") +
  geom_sf(data = globe_sf, aes(geometry=geometry), color = "magenta")
