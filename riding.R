library(sf)
library(tidyverse)

roads <-"https://kartta.hel.fi/avoindata/Helsinki_liikennevaylat_avoin_data.zip"

temp <- tempfile()
download.file(roads, temp)
unzip(zipfile = temp)
unlink(temp)

areas <- "http://www.hel2.fi/tietokeskus/data/kartta_aineistot/PKS_Kartta_Rajat_KML2011.zip"
temp <- tempfile()
download.file(areas, temp)
unzip(zipfile = temp)
unlink(temp)

class(st_geometry(st_read("Helsinki_liikennevaylat_avoin_data/KML/Hki_liikennevaylat.kml", quiet = TRUE)))
# "sfc_LINESTRING" "sfc"

class(st_geometry(st_read("PKS_suuralue.kml", quiet = TRUE)))
# "sfc_POLYGON" "sfc" 

roads_data <- st_read("Helsinki_liikennevaylat_avoin_data/KML/Hki_liikennevaylat.kml")

areas_data <- st_read("PKS_suuralue.kml")

hki <- areas_data %>%
  filter(Name %in% c("Läntinen", "Keskinen","Pohjoinen")) %>%
  mutate(Name_en = case_when(
    Name == "Läntinen" ~ "West",
    Name == 'Keskinen' ~ "Central",
    Name == 'Pohjoinen' ~ "North",
  ))

riding_tracks <- roads_data %>% 
  filter(str_detect(Description, "Ratsastus.*"))

lengths <- riding_tracks %>% 
  mutate(l = st_length(st_cast(st_sfc(geometry),"LINESTRING"))) %>% 
  summarise(total_length = sum(l))

track_length <- as.numeric(lengths$total_length)
# 8442.004 m

# Had troubles in pasting the track_length in the title
chart_title <- "There are 8,4 km horse riding tracks in Helsinki"
                        
p <- ggplot(hki) + geom_sf(alpha = 0.3, color = "slategray2") + 
  geom_sf_text(aes(label = Name_en), colour = "slategray3") +
  geom_point(aes(y=60.1704507, x=24.9386955), colour="red", size = 1) 

p <- p + geom_sf(data = riding_tracks, color = "brown", linetype = "dotdash") + 
  ylab("Latitude") + 
  xlab("Longitude") +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5), 
        panel.background = element_rect(fill = "aliceblue")) +
  labs(x = NULL, y = NULL, 
       title = chart_title,
       subtitle = "The lonely dot is Helsinki Central Station",
       caption = "Data sources: Helsingin liikenneväylät, Helsingin aluejakokartat\nhttps://hri.fi/data/dataset/helsingin-liikennevaylat https://hri.fi/data/fi/dataset/paakaupunkiseudun-aluejakokartat")
p

ggsave(
  "riding.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device='png'
)


