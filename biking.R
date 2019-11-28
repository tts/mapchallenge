library(tidyverse)
library(sf)
library(gganimate)
library(lubridate)

aprildata <- "http://dev.hsl.fi/citybikes/od-trips-2019/2019-04.csv"
temp <- tempfile()
download.file(maydata, temp)
data <- read_csv(temp)
unlink(temp)

bikestations <- "https://opendata.arcgis.com/datasets/1b492e1a580e4563907a6c7214698720_0.csv"
temp <- tempfile()
download.file(bikestations, temp)
stations <- read_csv(temp)
unlink(temp)

stations_cleaned <- stations %>% 
  select(X, Y, name, id)

aalto <- data %>% 
  filter(`Departure station name` == "Aalto-yliopisto (M), Tietotie")

kulosaari <- data %>% 
  filter(`Departure station name` == "Kulosaaren metroasema")

# Join departure station coords
kulo_dep <- left_join(kulosaari, stations_cleaned, by = c(`Departure station id`="id"))
aalto_dep <- left_join(aalto, stations_cleaned, by = c(`Departure station id`="id"))

# and arrival coords
kulo_dep_arr <- left_join(kulo_dep, stations_cleaned, by = c(`Return station id`="id")) %>% 
  mutate(h = hour(Departure),
         min = minute(Departure)) %>%
  filter(X.x != X.y)

aalto_dep_arr <- left_join(aalto_dep, stations_cleaned, by = c(`Return station id`="id")) %>% 
  mutate(h = hour(Departure),
         min = minute(Departure)) %>% 
  filter(X.x != X.y)

p <- ggplot() +
  geom_curve(data = kulo_dep_arr,
             aes(x = X.x, y = Y.x, 
                 xend = X.y, yend = Y.y), 
             curvature = 0.3, 
             color = "orange",
             arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  geom_curve(data = aalto_dep_arr,
             aes(x = X.x, y = Y.x, 
                 xend = X.y, yend = Y.y), 
             curvature = 0.3, 
             color = "wheat",
             arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "black", fill = "black"),
        plot.title = element_text(size = 12*1.2, color = "white"),
        plot.subtitle = element_text(size = 12*1.2, color = "white"),
        plot.caption = element_text(size = 12*0.5, color = "white"),
        panel.background = element_rect(fill = "black", color  =  NA))

anim <- p +
  transition_time(h) +
  labs(title = "Departures from Aalto University and Kulosaari bike stations in April 2019",
       subtitle = paste0("Hour: ","{round(frame_time)}"),
       caption = "Sources: Helsinki Region Transport’s (HSL) city bike stations’ Origin-Destination (OD) data and stations.\nCreative Commons Attribution 4.0.\n") 
       

anim

anim_save("kulo_aalto_bike_depts.gif", anim)

