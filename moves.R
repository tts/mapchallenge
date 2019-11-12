library(tidyverse)
library(raster)
library(sf)

# DEM sheets via https://tiedostopalvelu.maanmittauslaitos.fi
ras <- raster("V5211.tif")
ras_df <- as.data.frame(ras, xy = TRUE)
ras2 <- raster("V5212.tif")
ras2_df <- as.data.frame(ras2, xy = TRUE)

# Legacy Endomondo 
ski1 <- st_read("hiihto1213.geojson", stringsAsFactors = F)
ski2 <- st_read("hiihto1214.geojson", stringsAsFactors = F)
ski3 <- st_read("hiihto1215.geojson", stringsAsFactors = F)
ski4 <- st_read("hiihto1216.geojson", stringsAsFactors = F)

# Reproject vector layer to that of the raster EPSG:3067
ski1_EPSG3067 <- st_transform(ski1, crs = 3067)
ski2_EPSG3067 <- st_transform(ski2, crs = 3067)
ski3_EPSG3067 <- st_transform(ski3, crs = 3067)
ski4_EPSG3067 <- st_transform(ski4, crs = 3067)

ggplot() +
  geom_raster(data = ras_df,
              aes(x = x, y = y, fill= V5211)) +
  geom_raster(data = ras2_df,
              aes(x = x, y = y, fill = V5212)) +
  geom_sf(data = ski1_EPSG3067, color = "#EEF4FB") +
  geom_sf(data = ski2_EPSG3067, color = "#EEF4FB") +
  geom_sf(data = ski3_EPSG3067, color = "#EEF4FB") +
  geom_sf(data = ski4_EPSG3067, color = "#EEF4FB") +
  scale_fill_viridis_c() +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  labs(x = NULL, y = NULL, 
       title = "Skiing about in Saariselkä, December 2014",
       subtitle = "Elevation scale in meters",
       caption = "10 m DEM by National Land Survey of Finland CC 4.0\nEndomondo GPX") 



