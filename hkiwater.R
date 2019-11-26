library(tidyverse)
library(sf)
library(ggspatial)

# Vantaa river watershed selected & saved from http://paikkatieto.ymparisto.fi/value/
vantaa <- st_read("valu1006020300626_388368_6678465/valu1006020300626_388368_6678465.shp")

crs <- "urn:ogc:def:crs:EPSG::4326"

# Waters
baseurl <- "https://kartta.hsy.fi/geoserver/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "asuminen_ja_maankaytto:maanpeite_vesi_2016"
request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json","&srsName=", crs)
waters <- st_read(request, stringsAsFactors = FALSE)

# Sea 
baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "avoindata:Seutukartta_maankaytto_merialue"
request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json", "&srsName=", crs)
sea <- st_read(request, stringsAsFactors = FALSE)

ggplot() +
  annotation_map_tile(zoom = 10, type = "cartodark") +
  geom_sf(data = sea, fill = "#85C1E9", color = "grey96", alpha = 0.5) +
  geom_sf(data = waters, color = "#AED6F1", alpha = 0.8) +
  geom_sf(data = vantaa, fill = "#D0ECE7", color = "grey96", alpha = 0.3) +
  theme_void() +
  theme(plot.title = element_text(size = 12*3, color = "grey50")) +
  labs(x = NULL, y = NULL, 
       title = "Vantaa river watershed & Helsinki region waters",
       caption = "Helsinki Region Map | Metropolitan area land cover | SYKE VALUE") 

ggsave(
  "waters.png",
  width = 35, 
  height = 35, 
  dpi = 72, 
  units = "cm", 
  device = 'png'
)
