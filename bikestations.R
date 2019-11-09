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

baana <- stations_cleaned %>% 
  filter(name == "Baana")

therest <- stations_cleaned %>% 
  filter(name != "Baana")

sample <- therest %>% 
  sample_n(30)

p <- ggplot(data = therest, 
            aes(x = x, y = y)) +
  geom_point(color = "#F5F008") +
  geom_point(data = baana, aes(x = x, y = y), colour = "red")

p + 
  geom_text_repel(data = sample,
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
       subtitle = "The red one is station Baana",
       caption = "Source: Helsinki Region Transport’s (HSL) city bicycle stations.\nhttps://hri.fi/data/fi/dataset/hsl-n-kaupunkipyoraasemat\nCreative Commons Attribution 4.0.\n") 

ggsave(
  "st.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device='png'
)

