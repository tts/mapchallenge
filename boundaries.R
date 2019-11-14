library(tidyverse)
library(sf)
library(ggrepel)

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"

typenames <- c("avoindata:Seutukartta_maankaytto_viheralueet", "avoindata:Seutukartta_maankaytto_rakennetut_alueet",
               "avoindata:Seutukartta_maankaytto_jarvet")

get_data <- function(type,format) {
  wfs_request <- paste0(baseurl, "&typeName=", type, "&outputFormat=", format)
  res <- st_read(wfs_request, quiet = TRUE, stringsAsFactors = FALSE)
  res_4326 <- res %>%
    st_transform(crs = 4326)
  return(res_4326)
}

data <- map2(typenames, "json", ~get_data(.x, .y))

green <- as.data.frame(data[1])
grey <- as.data.frame(data[2])
lakes <- as.data.frame(data[3]) %>% 
  mutate(
    lon = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry,  ~st_centroid(.x)[[2]])
  ) 

sample <- lakes %>% 
  sample_n(30)

ggplot(green) +
  geom_sf(fill = "#52be80", color = "black") +
  geom_sf(data = grey, fill = "#aeb6bf", color = "black") +
  geom_sf(data = lakes, fill = "#3498db", color = "black") +
  geom_label_repel(data = sample, 
                  aes(x = lon, y = lat, label = vesisto_nimi_s),
                  arrow = arrow(length = unit(0.03,"npc"), type = "closed", 
                                ends = "last", angle = 15)) +
  theme_minimal() +
  theme(plot.title = element_text(size=22),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "transparent")) +
  labs(x = NULL, y = NULL, 
       title = "Green, grey (built) and blue (lakes) boundaries in the Helsinki region",
       caption = "Data: Helsinki region map\nhttps://hri.fi/data/dataset/seutukartta")

ggsave(
  "hkiboundaries.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device = 'png'
)


