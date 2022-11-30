
# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

source('scripts/source_leed_airbnb.R')

# First glance ----------------------------------------------------------

# Plot LEED projects (dots)

tmap_mode('plot')

m1 <- 
  tm_shape(nbhd_sf)+
  tm_polygons()+
  tmap_options(check.and.fix = TRUE)+
  
  tm_shape(leed_sf)+
  tm_dots()+
  tm_layout(
    main.title =
      'San Francisco: LEED projects distribution')

tmap_save(m1,
          'output/plots/sf_leed_dot.png')

# Airbnb listings (dots)

m2 <- 
  tm_shape(nbhd_sf)+
  tm_polygons()+
  tmap_options(check.and.fix = TRUE)+
  tm_shape(airbnb_sf)+
  tm_dots()+
  tm_layout(
    main.title = 'San Francisco: Airbnb Distribution')

tmap_save(m2,
          'output/plots/sf_airbnb_dot.png',
          height = 10)


# Choropleth maps ---------------------------------------------------------

## SF: LEED projects -------------------------------------------------------

# Number of LEED projects in each Airbnb neighbourhood

m3 <- 
  leed_sf %>% 
  st_join(nbhd_sf %>% select(neighbourhood)) %>% 
  as_tibble() %>% 
  group_by(neighbourhood) %>% 
  summarize(n = n()) %>% 
  left_join(
    nbhd_sf,
    .,
    by='neighbourhood') %>%
  tm_shape()+
  tm_polygons(
    col = 'n',
    style = 'fixed',
    breaks = 
      c(0,
        5,
        10,
        20,
        40,
        50, 
        500),
    palette = "YlOrRd",
    n = 6, 
    contrast = c(0.1, 1),
    title = 'Number of Projects')+
  tmap_options(check.and.fix = TRUE) +
  tm_layout(
    main.title = 'LEED projects in San Francisco',
    frame = '#999999')

tmap_save(m3,
          'output/plots/sf_leed_n_map.png')


## SF: Average Price of Airbnb Listings -----------------------------------

airbnb_sf$price = as.numeric(gsub("\\$", "", airbnb_sf$price))

m4 <- 
  airbnb_sf %>%
  filter(price < 481,
         minimum_nights < 365) %>% 
  st_join(nbhd_sf %>% select(neighbourhood)) %>% 
  as_tibble() %>%
  group_by(neighbourhood.x) %>% 
  summarize(mean_price = mean(price,na.rm = TRUE)) %>% 
  left_join(
    nbhd_sf,
    .,
    by = c('neighbourhood' = 'neighbourhood.x')) %>%
  
  tm_shape()+
  tm_polygons(
    col = 'mean_price',
    title = 'Average Price Per Night')+
  tmap_options(check.and.fix = TRUE) +
  tm_layout(
    main.title = 'Price of Airbnb Listings in San Francisco',
    frame = '#999999')

tmap_save(m4,
          'output/plots/sf_airbnb_price_map.png')