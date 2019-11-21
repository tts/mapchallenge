library(tidyverse)
library(sf)

# Bedrock data
baseurl <- "https://kartta.hsy.fi/geoserver/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "asuminen_ja_maankaytto:maanpeite_avokalliot_2016"
crs <- "urn:ogc:def:crs:EPSG::4326"
request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json","&srsName=", crs)
data <- st_read(request, stringsAsFactors = FALSE)
write.csv(data, "rocks.csv", row.names = F)

hkirocks <- data %>% 
  filter(kunta == "Helsinki")

rm(data)
gc()

# Shoreline data
baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "avoindata:Seutukartta_meren_rantaviiva"
request <- paste0(seaurl, "&typeName=", type, "&outputFormat=json", "&srsName=", crs)
shore <- st_read(request, stringsAsFactors = FALSE)

ggplot(shore) +
  geom_sf(colour = "#b8e0f7") +
  geom_sf(data = hkirocks, colour = "#BDBDBD") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "black", fill = "black"),
        plot.title = element_text(size = 12*5, color = "#BDBDBD"),
        plot.subtitle = element_text(size = 12*0.8, color = "#b8e0f7"),
        plot.caption = element_text(size = 12*0.5, color = "#EAE9CD"),
        panel.background = element_rect(fill = "black", color = "black")) +
  labs(x = NULL, y = NULL, 
       title = "Helsinki bedrock",
       subtitle = "with the shoreline",
       caption = "Sources: Metropolitan area land cover\nhttps://hri.fi/data/en_GB/dataset/paakaupunkiseudun-maanpeiteaineisto\nHelsinki Region Map\nhttps://hri.fi/data/en_GB/dataset/seutukartta\nCreative Commons Attribution 4.0.\n") 

ggsave(
  "bedrock.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device = 'png'
)

