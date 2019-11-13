library(tidyverse)
library(sf)
library(ggrepel)

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"
typenames <- c("avoindata:Seutukartta_liikenne_metro_rata", "avoindata:Seutukartta_liikenne_metroasemat", "avoindata:Seutukartta_meren_rantaviiva")

get_data <- function(type,format) {
  wfs_request <- paste0(baseurl, "&typeName=", type, "&outputFormat=", format)
  res <- st_read(wfs_request, quiet = TRUE, stringsAsFactors = FALSE)
  res_4326 <- res %>%
    st_transform(crs = 4326)
  return(res_4326)
}

data <- map2(typenames, "json", ~get_data(.x, .y))

metro <- as.data.frame(data[1])
st <- as.data.frame(data[2]) %>% 
  mutate(
    lon = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry,  ~st_centroid(.x)[[2]])
  ) 
coast <- as.data.frame(data[3])

ggplot(coast) +
  geom_sf(aes(geometry = geometry), size = 0.4, color = "lightblue") +
  geom_sf(data = metro, aes(geometry = geometry), size = 1, color = "#e67e22") +
  geom_sf(data = st, aes(geometry = geometry), size = 2, color = "black") +
  geom_text_repel(data = st, 
                  aes(x = lon, y = lat, label = metroasema),
                  point.padding = 1,
                  color = "#666658") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "transparent")) +
  labs(x = NULL, y = NULL, 
       title = "Helsinki metropolitan metro tracks follow the coastline",
       caption = "Data: Helsinki region map\nhttps://hri.fi/data/dataset/seutukartta")

ggsave(
  "tracks.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device='png'
)


