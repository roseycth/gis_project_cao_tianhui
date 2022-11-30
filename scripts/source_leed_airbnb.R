# set up ------------------------------------------------------------------

library(sf)
library(tmap)
library(tmaptools)
library(tidygeocoder)
library(tidyverse)

# read in shapefiles ------------------------------------------------------

nbhd_sf <- 
  st_read('data/raw/shapefiles/sf_neighbourhoods.geojson') %>% 
  set_names(
    names(.) %>%
      tolower()) %>% 
  st_transform(crs = 7131) %>% 
  select(-neighbourhood_group)

leed_sf <- 
  st_read('data/processed/leed_sf.geojson')

airbnb_sf <- 
  st_read('data/processed/airbnb_sf.geojson')

leed_closest5_id <- 
  read_rds('data/processed/leed_closest5_id.rds')


