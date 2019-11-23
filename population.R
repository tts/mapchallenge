library(tidyverse)
library(sf)
library(tmap)

url <-"https://www.hsy.fi/sites/AvoinData/AvoinData/SYT/Tietoyhteistyoyksikko/Shape%20(Esri)/V%C3%A4est%C3%B6tietoruudukko/Vaestotietoruudukko_2018_shp.zip"
temp <- tempfile()
download.file(url, temp)
unzip(zipfile = temp)
unlink(temp)

data <- st_read("Vaestoruudukko_2018.shp", stringsAsFactors = FALSE) 

data_long <- data %>% 
  select(starts_with("IKA"), -geometry) %>% 
  gather(Age, Count, starts_with("IKA"))

cuts <- c(0,100,200,300,Inf)

tm_shape(data_long) +
  tm_polygons("Count",
              breaks = cuts,
              border.col = "white", 
              border.alpha = 0.4) +
  tm_facets(by = "Age") +
  tm_layout(main.title = "Where do people live in Helsinki?",
            panel.labels = c('Age over 80','0-9','10-19','20-29','30-39',
                             '40-49','50-59','60-69','70-79'),
            panel.label.size = 1.5, panel.label.color = 'khaki4',
            panel.label.bg.color = 'wheat', panel.label.height = 1.1,
            bg.color = "grey85", legend.title.size = 0.5)

tm_shape(data) +
  tm_polygons("IKA0_9",
              border.col = "white",
              border.alpha = 0.4) +
  tm_layout(main.title = "Children under age of 10",
            bg.color = "grey85", legend.show = FALSE)

tm_shape(data) +
  tm_polygons("IKA60_69",
              border.col = "white",
              border.alpha = 0.4) +
  tm_layout(main.title = "Age 60 to 69",
            bg.color = "grey85", legend.show = FALSE)



