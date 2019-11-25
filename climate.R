library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)

url <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Tietoyhteistyoyksikko/Aurinkopaneeleille_sopivat_sijainnit/paneeleille_sopivat_sijainnit_Hki.zip"

temp <- tempfile()
download.file(url, temp)
unzip(zipfile = temp)
unlink(temp)

rawdata <- st_read("useful_area_size.shp", stringsAsFactors = FALSE)

rawdata_4326 <- rawdata %>% 
  st_transform(crs = 4326) 

kulosaari_bbox <- c(ymin = 60.176500, ymax = 60.1918, xmin = 24.9899, xmax = 25.01842)

kulosaari <- st_crop(rawdata_4326, kulosaari_bbox) %>% 
  filter(areaSUM > 0)

# http://www.theanalyticslab.nl/polygon-plotting-in-r/
cuts <- c(0, 5, 10, 100, 500, 1000, 2000, 2500)
colorbins <- colorBin("YlOrRd", domain = kulosaari$areaSUM, bins = cuts)

map <-  leaflet(kulosaari) %>%
  addTiles(attribution = "HSY Creative Commons Attribution 4.0.") %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, 
              fillOpacity = 0.7, fillColor = ~colorbins(kulosaari$areaSUM))  

map_with_legend <- map %>% addLegend(pal = colorbins, 
                                     values = kulosaari$areaSUM,
                                     labFormat = labelFormat(suffix = " m2"),
                                     opacity = 0.7, title = "Roof area suitable for solar panels", position = "topright")

tooltip <- sprintf("<strong>%s</strong> m2",kulosaari$areaSUM) %>% lapply(htmltools::HTML)

map_with_tooltip <- map_with_legend %>% addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7, 
                                                    fillColor = ~colorbins(kulosaari$areaSUM), 
                                                    highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
                                                    label = tooltip)
map_with_tooltip

saveWidget(map_with_tooltip, file="Roofs on Kulosaari suitable for solar panels.html")
