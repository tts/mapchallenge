library(tidyverse)
library(sf)

url <-"https://kartta.hel.fi/avoindata/Helsinki_liikennevaylat_avoin_data.zip"
temp <- tempfile()
download.file(url, temp)
unzip(zipfile = temp)
unlink(temp)

data <- st_read("Helsinki_liikennevaylat_avoin_data/Shape/Hki_liikennevaylat.shp", stringsAsFactors = FALSE)

bridges <- data %>% 
  filter(Vaylatyypp == 'Silta tai ylikulku (katuverkolla)')

rm(data)
gc()

ggplot(bridges) +
  geom_sf(size = 1.2, color = "#f4d03f") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "black", fill = "black"),
        plot.title = element_text(size = 12*3, color = "#b7950b"),
        plot.caption = element_text(size = 12*0.5, color = "#f4d03f"),
        panel.background = element_rect(fill = "black", color = "black")) +
  labs(x = NULL, y = NULL, 
       title = "Bridges and overpasses in Helsinki",
       caption = "Source: City of Helsinki road map https://hri.fi/data/en_GB/dataset/helsingin-liikennevaylat\nCreative Commons Attribution 4.0.\n") 

ggsave(
  "bridges.png",
  width = 31, 
  height = 24.90, 
  dpi = 72, 
  units = "cm", 
  device = 'png'
)
