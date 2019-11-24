library(tidyverse)
library(sf)
library(cartography)

url <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Tietoyhteistyoyksikko/Yollinen_valaistus/Yollinen_valaistus.zip"
temp <- tempfile()
download.file(url, temp)
unzip(zipfile = temp)
unlink(temp)

rawdata <- st_read("Yollinen_valaistus/light_emissions_helsinki.shp", quiet = TRUE, stringsAsFactors = FALSE)

data <- rawdata %>% 
  filter(range_cont != "<NoData>") %>% 
  mutate(range_cont = ifelse(substr(range_cont, 1, 3) %in% c("130", "140", "150", "160", "170", "180", 
                                                             "190", "200", "210", "220"),  "130 -",
                             ifelse(substr(range_cont, 1, 1) %in% c("-","0"), "- 10", range_cont)),
         range_cont = factor(range_cont,
                             levels=c("- 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",
                                      "50 - 60", "60 - 70", "70 - 80", "80 - 90", "90 - 100",
                                      "100 - 110", "110 - 120", "120 - 130", "130 -")))

palette <- carto.pal(
  "sand.pal",
  20,
  pal2 = NULL,
  n2 = NULL,
  middle = FALSE,
  transparency = TRUE
)

ggplot() +
  geom_sf(data = data, aes(fill = range_cont)) +
  theme_void() +
  theme(plot.title = element_text(size = 12*2),
        plot.subtitle = element_text(size = 12)) +
  scale_fill_manual(values = rev(palette)) +
  labs(fill = "nW/cm2/sr",
       title = "Night time light emissions in Helsinki region, May 2014",
       subtitle = "Decumanus project | Eurosense | NOAA satellite",
       caption = "Source: Helsingin seudun ympäristöpalvelut HSY\nCreative Commons Attribution 4.0.\n")

ggsave(
  "lightemissions.png",
  width = 31, 
  height = 24.90, 
  dpi = 72, 
  units = "cm", 
  device = 'png'
)
