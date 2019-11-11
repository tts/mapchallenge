library(rayshader)
library(raster)
library(tidyverse)

ras <- raster("V5211.tif")

ras_df <- as.data.frame(ras, xy = TRUE)

p <- ggplot() +
  geom_raster(data = ras_df, 
              aes(x = x, y = y, fill = V5211)) +
  scale_fill_viridis_c() 

plot_gg(p, width = 7, height = 4, multicore = FALSE, windowsize = c(1000, 800), 
        zoom = 0.85, phi = 35, theta = -45)

rgl::snapshot3d("saariselka.png")
