# VOI scooter data

library(httr)
library(tidyverse)
library(jsonlite)

# Helsinki
resp <- fromJSON("https://api.voiapp.io/v1/vehicle/status/ready?lat=60.16952&lng=24.93545")

data <- resp %>% 
  mutate(r = map(location, ~ data.frame(t(.)))) %>%
  unnest(r) %>%
  rename(lat = X1,
         lon = X2) %>%
  select(-location) %>% 
  filter(battery >= 80 & locked == TRUE)

p <- ggplot() + theme_minimal()
p <- p + geom_point(data=data, 
                    aes(x=lon, y=lat), 
                    color= alpha("darkgreen", 0.3)) +
  geom_point(aes(y=60.1704507, x=24.9386955), colour="red", size = 5) +
  labs(title = 'VOI electric scooters in the Helsinki metropolitan area',
       subtitle = "Status: locked, and battery level at least 80%") +
  theme(axis.title = element_blank())
p

ggsave(
  "voi.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device='png'
)
