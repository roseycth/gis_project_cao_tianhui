# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

source('scripts/source_leed_airbnb.R')


# find closest five airbnb ------------------------------------------------


# Grab the airbnb dataframe's neighbourhood

leed_closest5_updated <- 
  leed_nbhd_sf %>%
  pull(ID) %>% 
  map_dfr(
    ~{
      target_leed <- 
        leed_nbhd_sf %>% 
        filter(ID == .x)
      airbnb_sf %>% 
        filter(neighbourhood == target_leed$neighbourhood) %>% 
        mutate(
          leed_dist = st_distance(., target_leed)) %>% 
        as_tibble() %>% 
        arrange(leed_dist) %>% 
        slice(1:5) %>%
        transmute(
          leed_id = .x,
          airbnb_id = id)
    }) %>% 
  mutate(airbnb_id = as.character(airbnb_id))

# leed_closest5_updated %>% write_csv('data/processed/leed_closest5_id.csv')
leed_closest5_updated %>% write_rds('data/processed/leed_closest5_id.rds')
