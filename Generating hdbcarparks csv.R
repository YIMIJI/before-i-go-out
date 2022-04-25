
library(rvest)
library(dplyr)
library(ggmap)
library(xml2)
library(sf)
library(tidyverse)
library(stringr)
library(jsonlite)
library(geosphere)

library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(shinyTime)
library(mapsapi)

ggmap::register_google(key = 'AIzaSyC7ed3xrF-Uav_RE4_tgHU6yWCqmtg08Ew')
key <- 'AIzaSyC7ed3xrF-Uav_RE4_tgHU6yWCqmtg08Ew'


#writing csv
hdb_ori <- read.csv("hdb-carpark-information.csv") #taken from https://data.gov.sg/dataset/hdb-carpark-information

sf_df <- st_as_sf(hdb_ori, coords = c("x_coord", "y_coord"), crs = 3414) 

latlon_df <- st_transform(sf_df, crs = 4326)  #crs used by google earth
hdb2 <- latlon_df %>%
  mutate(lon = st_coordinates(latlon_df)[,1], lat = st_coordinates(latlon_df)[,2]) %>% as.data.frame() %>% select(1:10,12:13)


for(i in 1:nrow(hdb2)){
  hdb2$address[i] <- str_to_title(hdb2$address[i])
  hdb2$car_park_type[i] <- str_to_title(hdb2$car_park_type[i])
  hdb2$type_of_parking_system[i] <- str_to_title(hdb2$type_of_parking_system[i])
  hdb2$short_term_parking[i] <- str_to_title(hdb2$short_term_parking[i])
  hdb2$free_parking[i] <- str_to_title(hdb2$free_parking[i])
  hdb2$night_parking[i] <- str_to_title(hdb2$night_parking[i])
}


write.csv(hdb2, "hdb_carparks_final.csv", row.names = F)