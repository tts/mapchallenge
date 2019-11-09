library(tidyverse)
library(ggrepel)

bikestations <- "https://opendata.arcgis.com/datasets/1b492e1a580e4563907a6c7214698720_0.csv"

temp <- tempfile()
download.file(bikestations, temp)
stations <- read.csv(temp, encoding = "UTF-8")
unlink(temp)

stations_cleaned <- stations %>% 
  rename(x = X.U.FEFF.X,
         y = Y) %>%
  select(x, y, name, id)

st_sample <- stations_cleaned %>% 
  sample_n(30)

p <- ggplot(data = stations_cleaned, 
            aes(x = x, y = y)) +
  geom_point(color = "#F5F008") +
  geom_point(aes(y=60.1704507, x=24.9386955), colour="magenta", size = 2) 

p + 
  geom_text_repel(data = st_sample,
                  aes(label = name),
                  point.padding = 1,
                  color = "#666658") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "black", fill = "black"),  
        plot.title = element_text(size = 12*1.2, color = "#EAE9CD"),  
        plot.subtitle = element_text(size = 12*0.8, color = "#EAE9CD"),
        plot.caption = element_text(size = 12*0.5, color = "#EAE9CD"),
        panel.background = element_rect(fill = "black", color  =  NA)) +
  labs(x = NULL, y = NULL, 
       title = "Helsinki city bike stations",
       subtitle = "The magenta dot is Helsinki Central Station",
       caption = "Source: Helsinki Region Transport’s (HSL) city bicycle stations.\nhttps://hri.fi/data/fi/dataset/hsl-n-kaupunkipyoraasemat\nCreative Commons Attribution 4.0.\n") 

ggsave(
  "st.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device='png'
)

