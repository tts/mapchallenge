library(tidyverse)
library(sf)
library(ggspatial)

url2018 <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Ilmanlaatu/Typpidioksidin%20vuosikeskiarvot/Shape/typpidioksidin_vuosikeskiarvot_2018.zip"
temp <- tempfile()
download.file(url2018, temp)
unzip(zipfile = temp)
unlink(temp)

url2017 <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Ilmanlaatu/Typpidioksidin%20vuosikeskiarvot/Shape/typpidioksidin_vuosikeskiarvot_2017_SHP.zip"
temp <- tempfile()
download.file(url2017, temp)
unzip(zipfile = temp)
unlink(temp)

url2016 <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Ilmanlaatu/Typpidioksidin%20vuosikeskiarvot/Shape/typpidioksidin_vuosikeskiarvot_2016.zip"
temp <- tempfile()
download.file(url2016, temp)
unzip(zipfile = temp)
unlink(temp)

url2015 <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Ilmanlaatu/Typpidioksidin%20vuosikeskiarvot/Shape/typpidioksidin_vuosikeskiarvot_2015.zip"
temp <- tempfile()
download.file(url2015, temp)
unzip(zipfile = temp)
unlink(temp)

url2014 <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Ilmanlaatu/Typpidioksidin%20vuosikeskiarvot/Shape/typpidioksidin_vuosikeskiarvot_2014.zip"
temp <- tempfile()
download.file(url2014, temp)
unzip(zipfile = temp)
unlink(temp)

url2013 <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Ilmanlaatu/Typpidioksidin%20vuosikeskiarvot/Shape/typpidioksidin_vuosikeskiarvot_2013.zip"
temp <- tempfile()
download.file(url2013, temp)
unzip(zipfile = temp)
unlink(temp)

url2012 <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Ilmanlaatu/Typpidioksidin%20vuosikeskiarvot/Shape/typpidioksidin_vuosikeskiarvot_2012.zip"
temp <- tempfile()
download.file(url2012, temp)
unzip(zipfile = temp)
unlink(temp)

url2011 <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Ilmanlaatu/Typpidioksidin%20vuosikeskiarvot/Shape/typpidioksidin_vuosikeskiarvot_2011.zip"
temp <- tempfile()
download.file(url2011, temp)
unzip(zipfile = temp)
unlink(temp)

url2010 <- "https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Ilmanlaatu/Typpidioksidin%20vuosikeskiarvot/Shape/typpidioksidin_vuosikeskiarvot_2010.zip"
temp <- tempfile()
download.file(url2010, temp)
unzip(zipfile = temp)
unlink(temp)

data2018 <- st_read("typpidioksidin_vuosikeskiarvot_2018.shp") %>%
  select(-starts_with("ETRS"))
data2017 <- st_read("typpidioksidin_vuosikeskiarvot_2017.shp") %>%
  select(-starts_with("ETRS"))
data2016 <- st_read("typpidioksidin_vuosikeskiarvot_2016.shp") %>%
  select(-starts_with("ETRS"))
data2015 <- st_read("typpidioksidin_vuosikeskiarvot_2015.shp")
data2014 <- st_read("typpidioksidin_vuosikeskiarvot_2014.shp")
data2013 <- st_read("typpidioksidin_vuosikeskiarvot_2013.shp")
data2012 <- st_read("typpidioksidin_vuosikeskiarvot_2012.shp") %>%
  rename(Linkki = linkki)
data2011 <- st_read("typpidioksidin_vuosikeskiarvot_2011.shp") %>%
  rename(Linkki = linkki)
data2010 <- st_read("typpidioksidin_vuosikeskiarvot_2010.shp") %>%
  rename(Linkki = linkki)

data2010_2018 <- rbind(data2010, data2011, data2012, data2013, data2014, data2015, data2016, data2017, data2018)

# https://www.hsy.fi/fi/asiantuntijalle/ilmansuojelu/ilmanlaadunparantaminen/Sivut/Normit.aspx
data <- data2010_2018 %>%
  mutate(limit = ifelse(as.numeric(NO2) >= 40, 1, 0))

p <- ggplot(data, aes(size = 2, color = as.factor(limit), alpha = 0.7)) +
  #annotation_map_tile(zoom = 10, type = "stamenwatercolor") +
  geom_sf() +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12*3, color = "#BDBDBD"),
        plot.subtitle = element_text(size = 12*1.5, color = "#BDBDBD"),
        plot.caption = element_text(size = 12*0.5, color = "#BDBDBD")) +
  scale_color_manual(values = c("bisque4", "red")) +
  labs(x = NULL, y = NULL,
       title = "Nitrogen dioxide levels under (grey) and over (red) the norm",
       subtitle = "The norm is max 40 µg/m3/year",
       caption = "Data: HSY CC 4.0 BY https://www.hsy.fi/fi/asiantuntijalle/avoindata/Sivut/AvoinData.aspx?dataID=59")

p + facet_wrap(~Vuosi) +
  theme(strip.text.x = element_text(size=12*2, color = "#BDBDBD"))
        
ggsave(
  "no2.png",
  width = 40,
  height = 40,
  dpi = 72,
  units = "cm",
  device = 'png'
)
