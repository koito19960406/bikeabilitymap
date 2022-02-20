## code to prepare `my_dataset` dataset goes here
pacman::p_load(here,tidyverse,sf)
# load dataset ------------------------------------------------------------
tokyo_point<-read.csv(here::here("data-raw/bikeability_tokyo.csv")) %>% 
  st_as_sf(., coords = c("panoLon", "panoLat"), 
           crs = 4326)
singapore_point<-read.csv(here::here("data-raw/bikeability_singapore.csv")) %>% 
  st_as_sf(., coords = c("panoLon", "panoLat"), 
           crs = 4326)
point<-NULL
point$tokyo<-tokyo_point
point$singapore<-singapore_point
usethis::use_data(point, overwrite = TRUE)
