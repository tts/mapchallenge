library(tidyverse)
library(sf)
library(ggrepel)
library(RColorBrewer)

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "avoindata:Piirijako_pienalue"

# Get districts
wfs_request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json")
res <- st_read(wfs_request, quiet = TRUE, stringsAsFactors = F)
areas <- st_transform(res, crs = 4326)

# Crop
area_cropped <- st_crop(areas, xmin = 24.78281, xmax = 25.25450,
                          ymin = 60.1, ymax = 60.29784)

# Get names
type <- c("avoindata:Nimisto_piste_rekisteritiedot")
wfs_request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json")
res <- st_read(wfs_request, quiet = TRUE, stringsAsFactors = F)
names <- st_transform(res, crs = 4326)

# Count most common name groups
topgroups <- names %>% 
  filter(str_detect(nimikategoria, "ryhmänimi")) %>% 
  mutate(namegroup = sub("^.*,([^,]+),.*", "\\1", nimikategoria)) %>% 
  group_by(namegroup) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(!str_detect(namegroup, "ryhmänimi"),
         !str_detect(namegroup, "liitynnäinen")) %>% 
  head(10) %>% 
  mutate(namegroup = case_when(
    namegroup == " muistonimi" ~ "person",
    namegroup == " myllyt" ~ "mill",
    namegroup == " muinaislinnat ja -löytöpaikat" ~ "historic",
    namegroup == " pitäjien nimet" ~ "municipality",
    namegroup == " maanviljelys" ~ "farming",
    namegroup == " kalevalaiset nimet" ~ "kalevala",
    namegroup == " musiikki ja soittimet" ~ "music",
    namegroup == " tähdistö" ~ "stars",
    namegroup == " Seitsemän veljestä" ~ "7 brothers",
    namegroup == " veneet" ~ "boats"),
    lon = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry,  ~st_centroid(.x)[[2]])
    )

sample <- area_cropped %>% 
  sample_n(30) %>% 
  mutate(lon = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry,  ~st_centroid(.x)[[2]]))

ggplot(area_cropped, 
       aes(geometry=geometry)) +
  geom_sf(fill = "white", color = "wheat") +
  geom_sf(data = topgroups, aes(color=namegroup)) +
  scale_color_brewer(palette="Paired") +
  geom_text_repel(data = sample, size = 3, 
                  aes(x = lon, y = lat, label = osaalue_nimi_fi)) +
  geom_label_repel(data = topgroups, size = 5, nudge_x = -0.2, nudge_y = -0.1, alpha=0.9, 
                   aes(x = lon, y = lat, label = namegroup, color = namegroup)) +
  theme_minimal() +
  theme(plot.title = element_text(size=22),
        legend.position="none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "transparent")) +
  labs(x = NULL, y = NULL, 
       title = "Top street name categories in Helsinki",
       caption = "Data: Helsinki region map https://hri.fi/data/dataset/seutukartta\nStreet names https://hri.fi/data/en_GB/dataset/helsingin-nimisto\nDistrict map https://hri.fi/data/en_GB/dataset/helsingin-piirijako")

ggsave(
  "names.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device = 'png'
)
