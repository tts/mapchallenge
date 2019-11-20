library(tidyverse)
library(sf)
library(tmap)
library(mapview)

# Request data set from https://avaa.tdata.fi/web/paituli/latauspalvelu
# Metadata https://etsin.fairdata.fi/dataset/5f008b3b-d693-48d8-bf0d-7e28de08e351

data <- st_read("mavi/peltolohkot/2016/plohko_cd_2016B_ESRI.shp", stringsAsFactors = FALSE)

# Municipality number of Helsinki is 091, from Wikipedia
hki <- data %>% 
  filter(KNRO == "091") %>% 
  select(Farm = TILTU,
         Area = PINTA_ALA,
         Circuit = YMPARYS)

rm(data)
gc()

tmap_mode("view")

palette <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')

tm_basemap("OpenStreetMap.Mapnik") +
  tm_shape(hki) + 
  tm_fill("Farm",
          legend.show = T, 
          colorNA = "grey",
          title = "Farms in Helsinki",
          textNA = "-",
          popup.vars = c("Farm", "Area", "Circuit"),
          palette = palette,
          alpha = 0.6) +
  tm_borders("white", alpha = .1, lwd = 1.5) +
  tm_layout(outer.margins = 0) +
  tm_view(view.legend.position = c("left","bottom"))

# lf <- tmap_leaflet(map)
# mapshot(lf, file = "hkifields.png")
