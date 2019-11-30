library(tidyverse)
library(raster)
library(rayshader)

ras <- raster("L4133.tif")

ras_df <- as.data.frame(ras, xy = TRUE)

p <- ggplot() +
  geom_raster(data = ras_df, 
              aes(x = x, y = y, fill = L4133)) +
  scale_fill_viridis_c() 

plot_gg(p, width = 7, height = 4, 
        shadow_intensity = 0.3, 
        multicore = TRUE, 
        sunangle = 206, # Helsinki, 2pm
        windowsize = c(1000, 800), 
        zoom = 0.85, phi = 35, theta = -45)

rgl::snapshot3d("home.png")

