library(tidyverse)
library(sf)
library(RColorBrewer)

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?"
wfs_request <- "request=GetFeature&service=WFS&version=2.0.0&typeName=avoindata:RaideJokeri_ratalinja&outputFormat=json"
raidejokeri_wfs <- paste0(baseurl,wfs_request)

class(st_geometry(st_read(raidejokeri_wfs, quiet = TRUE)))
# "sfc_POLYGON" "sfc" 

jokeri <- st_read(raidejokeri_wfs, quiet = TRUE, type = 3)

jokeri_4326 <- jokeri %>%
  st_transform(crs = 4326)

myColors <- brewer.pal(3,"Oranges")
names(myColors) <- levels(jokeri_4326$tyyppi)
colScale <- scale_colour_manual(name = "grp",values = myColors)

p <- ggplot(jokeri_4326) +
  geom_sf(aes(color = tyyppi), size = 2) +
  geom_point(aes(y=60.1704507, x=24.9386955), colour="orange", size = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs(x = NULL, y = NULL, 
       title = "Jokeri light rail line (plan)",
       subtitle = "The darker area is a tunnel. The lone dot is Helsinki Central Station",
       caption = "Data source: Helsingin Raide-Jokerin linjaus ja pysäkit\nhttps://hri.fi/data/dataset/helsingin-raide-jokerin-linjaus-ja-pysakit")
p

p1 <- p + colScale
p1

ggsave(
  "jokeri.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device='png'
)
