library(tidyverse)
library(sf)

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=Puurekisteri_piste&outputFormat=json"
hki_trees_wfs <- paste0(baseurl,wfs_request)
trees <- st_read(hki_trees_wfs)

data <- trees %>% 
  filter(suku == "Tilia")

rm(trees)
gc()

data_4326 <- data %>%
  st_transform(crs = 4326)

data_4326_coord <- cbind(data_4326, st_coordinates(data_4326))
tilia <- st_drop_geometry(data_4326_coord)


p <- ggplot(tilia, aes(x = X, y = Y)) + 
  geom_hex() +
  geom_point(aes(y=60.1704507, x=24.9386955), colour="orange", size = 1) +
  theme_minimal() +
  theme(panel.grid = element_line(colour = "transparent")) +
  labs(x = NULL, y = NULL, 
       title = "Trees of the genus Tilia in Helsinki",
       subtitle = "The orange dot is Helsinki Central Station",
       caption = "Data source: Urban tree database of the City of Helsinki\nhttps://hri.fi/data/dataset/helsingin-kaupungin-puurekisteri")
p

ggsave(
  "tilia.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device='png'
)

