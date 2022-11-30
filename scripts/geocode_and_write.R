# set up ------------------------------------------------------------------

library(sf)
library(tmap)
library(tmaptools)
library(tidygeocoder)
library(tidyverse)

# read in -----------------------------------------------------------------

leed_complete <- 
  read_csv('data/raw/PublicLEEDProjectsDirectory.csv')

nbhd_sf <- 
  st_read('data/raw/shapefiles/sf_neighbourhoods.geojson') %>% 
  set_names(
    names(.) %>% 
      tolower()) %>% 
  st_transform(crs = 7131)

airbnb_sf <- 
  read_csv('data/raw/sf_listings.csv') %>% 
  st_as_sf(coords = c('longitude','latitude'),
           crs = 4326) %>% 
  st_transform(crs = 7131)

# Geocode San Francisco----------------------------------------------------

# Zipcode range to identify LEED projects in San Francisco

leed_sf_longlat <- 
  leed_complete %>% 
  filter(
    Zipcode >= '94103'
    & Zipcode <= '94172') %>% 
  mutate(
    full_address =
      paste(Street,
            City,
            State,
            sep = ',')) %>% 
  geocode(
    full_address,
    method = 'osm',
    lat = latitude,
    long = longitude)

## Transform LEED file to shapefile ---------------------------------------

leed_sf <- 
  leed_sf_longlat %>% 
  filter(
    is.na(longitude) == FALSE) %>% 
  st_as_sf(
    coords = c('longitude','latitude'),
    crs = 4326) %>% 
  st_transform(
    crs = 7131)


## save processed leed and airbnb shapefile -------------------------------

leed_sf %>% 
  st_write('data/processed/leed_sf.geojson')

airbnb_sf %>% 
  st_write('data/processed/airbnb_sf.geojson')

# spatial join leed and neighbourhood for future analysis

leed_nbhd_sf <-
  leed_sf %>% 
  st_join(
    nbhd_sf %>%
      select(neighbourhood))

# save the resultant object

leed_nbhd_sf %>% 
  st_write('data/processed/leed_nbhd.geojson')