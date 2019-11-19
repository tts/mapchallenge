library(tidyverse)
library(sf)
library(ggrepel)

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"
noise <- "avoindata:Meluselvitys_2017_alue_17_metro_L_den"
crs <- "urn:ogc:def:crs:EPSG::4326"
request <- paste0(baseurl, "&typeName=", noise, "&outputFormat=json", "&srsName=", crs)

data <- st_read(request, quiet = TRUE, stringsAsFactors = FALSE)

stations <- "avoindata:Seutukartta_liikenne_metroasemat"
st_request <- paste0(baseurl, "&typeName=", stations, "&outputFormat=json", "&srsName=", crs)
metro_st <- st_read(st_request, quiet = TRUE, stringsAsFactors = FALSE)

metro_st_xy <- as.data.frame(metro_st) %>% 
  mutate(
    lon = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry,  ~st_centroid(.x)[[2]])
  ) %>% 
  filter(metroasema %in% c("Kalasatama","Kulosaari", "Herttoniemi",
                           "Siilitie", "Itäkeskus", "Puotila", "Rastila", 
                           "Vuosaari", "Myllypuro", "Kontula", "Mellunmäki"))

ggplot () +  
  ggtitle("Highest noise levels of Helsinki metro 2017") +
  geom_sf(data = data, aes(fill = db_hi)) +
  scale_fill_viridis_c(alpha = 0.8, begin = 0.4, direction = -1, option = "inferno") +
  geom_text_repel(data = metro_st_xy,
                  aes(x = lon, y = lat, label = metroasema),
                  point.padding = 1,
                  color = "#FDEBD0") +
  theme(plot.title = element_text(size = 16, color = "#EAE9CD"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "black", fill = "black"),  
        plot.subtitle = element_text(size = 12*0.8, color = "#EAE9CD"),
        plot.caption = element_text(size = 12*0.5, color = "#EAE9CD"),
        panel.background = element_rect(fill = "black", color  =NA),
        legend.background = element_rect(fill = "black", color = NA),
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white")) +
  labs(x = NULL, y = NULL, 
       subtitle = "Day-evening-night level (Lden) at a height of 4 metres",
       caption = "Source: Traffic noise zones in Helsinki 2017.\nhttps://hri.fi/data/en_GB/dataset/helsingin-kaupungin-meluselvitys-2017\nCreative Commons Attribution 4.0.\n") 

ggsave(
  "metronoise.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device = 'png'
)


