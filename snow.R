library(tidyverse)
library(rvest)
library(geofi)
library(leaflet)
library(htmlwidgets)
library(htmltools)

stats <- "https://kilotavu.com/fmi-tilastot.php"
stations <- "https://ilmatieteenlaitos.fi/havaintoasemat"

rows <- stats %>%
  read_html() %>% 
  html_nodes("[class='taulukko']") %>% 
  html_nodes(xpath = "//tr")

# https://github.com/aaronschiff/30-day-map-challenge/blob/master/09-yellow.R
parse_rows <- function(x) {
  l_dat <- x %>% 
    html_children() %>% 
    html_text %>% 
    gsub("\n","",.) %>% 
    enframe() %>%
    select(value) %>%
    mutate(value = str_trim(value)) %>%
    bind_cols(tibble(name = c("station", 
                              "avgdegrees", 
                              "maxdegree", 
                              "mixdegree", 
                              "rain", 
                              "snow"))) %>%
    pivot_wider(names_from = name, values_from = value)
  
  return(l_dat)
}

parsed_stats <- map_dfr(.x = rows[2:length(rows)], 
                              .f = parse_rows)

st_rows <- stations %>%
  read_html() %>% 
  html_nodes("[class='table stations']") %>% 
  html_nodes(xpath = "//tr")

parse_st_rows <- function(x) {
  l_dat <- x %>% 
    html_children() %>% 
    html_text %>% 
    gsub("\n","",.) %>% 
    enframe() %>%
    select(value) %>%
    mutate(value = str_trim(value)) %>%
    bind_cols(tibble(name = c("name", 
                              "fmisid", 
                              "lpnn", 
                              "wmo", 
                              "lat",
                              "lon",
                              "height",
                              "groups",
                              "starting"))) %>%
    pivot_wider(names_from = name, values_from = value)
  
  return(l_dat)
}

parsed_st_data <- map_dfr(.x = st_rows[2:length(st_rows)], 
                       .f = parse_st_rows)

stats_stations <- inner_join(parsed_stats, parsed_st_data, by=c("station"="name"))

data <- stats_stations %>% 
  filter(groups != "Mareografi") %>% 
  mutate(lat = gsub(",",".",lat),
         lon = gsub(",", ".", lon),
         lat = as.numeric(lat),
         lon = as.numeric(lon),
         snow = as.numeric(snow)) %>% 
  select(station,snow,fmisid,lat,lon)

# https://ropengov.github.io/geofi/articles/geofi_tutorial.html
municipalities <- get_municipalities(year = 2019, scale = 4500)
municipalities_lonlat <- sf::st_transform(x = municipalities, crs = "+proj=longlat +datum=WGS84")

# https://stackoverflow.com/a/52226825
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 16px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Snow depth at weather stations in Finland 2019-11-09")
)  

leaflet(municipalities_lonlat) %>% 
  addTiles(attribution = "OpenStreetMap contributors|ilmatieteenlaitos.fi|kilotavu.com") %>% 
  addControl(title, position = "topleft", className="map-title") %>% 
  addPolygons(color = "black", 
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = FALSE)) %>% 
  addCircleMarkers(data = data,
                   ~lon, ~lat,
                   radius = ~snow/3,
                   color = "white",
                   popup = ~station, label = ~as.character(snow))

