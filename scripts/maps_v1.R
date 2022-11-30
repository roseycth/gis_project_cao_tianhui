# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

source('scripts/source_leed_airbnb.R')


# data processing ---------------------------------------------------------

# filter to listinghs with price less than 1000
airbnb_data <- 
  leed_closest5_id %>% 
  left_join(
    airbnb_sf %>% 
      transmute(airbnb_id = as.character(id),
                name,
                host_name,
                neighbourhood,
                room_type,
                price,
                minimum_nights,
                number_of_reviews,
                availability_365)) %>% 
  filter(price < 1000) %>% 
  st_as_sf()


# Convert Certification Level to Factors

leed_data <- 
  leed_sf %>% 
  mutate(
    CertLevel = factor(CertLevel,
                       levels = 
                         c('Certified',
                           'Silver',
                           'Gold',
                           'Platinum')) %>%
      fct_explicit_na('No Info (Missing or Not Certified)'))

# Median price in each polygon
# Not very helpful in static map
# But may be interesting in an interactive map

airbnb_area_median_price <-
  leed_closest5_id %>%
  left_join(
    airbnb_sf %>%
      transmute(airbnb_id = as.character(id),
                price,
                neighbourhood,
                availability_365)) %>%
  filter(price < 1000) %>%
  group_by(neighbourhood) %>%
  summarize(median_price = median(price,na.rm = TRUE)) %>%
  left_join(
    nbhd_sf,
    .)


# Static Map --------------------------------------------------------------

# Plot

tmap_mode('plot')

map1 <- 
  tm_shape(nbhd_sf)+
  tm_polygons()+
  tmap_options(check.and.fix = TRUE) +
  tm_shape(
    airbnb_data) + 
  tm_dots(
    col = 'darkgrey',
    size = 'price',
    title.size = 'Airbnb Listing Price',
    alpha = 0.5) +
  tm_shape(
    leed_data) +
  tm_dots(
    title = 'LEED Certification Level',
    col = 'CertLevel',
    size = 0.6,
    alpha = 0.8,
    shape = 23,
    palette = '-YlGnBu',
    style = 'cat') +
  
  tm_credits('Subset to Airbnb listings with price less than $1000 per night\namong observations in Top 5 closest listings to each building in the LEED directory', fontface = "italic", position = c("left", "top")) +
  
  tm_layout(
    main.title = 'LEED Certification and Airbnb Listings Price in San Francisco',
    frame = '#999999',
    legend.outside = TRUE)

tmap_save(map1,
          'output/plots/sf_leed_airbnb_price.png',
          height = 10)

# Interactive Map ---------------------------------------------------------

# bounding box to the city of SF neighborhoods

nbhd_bbox <-
  nbhd_sf %>%
  st_bbox()

# Plot

tmap_mode('view')

tm_shape(
  airbnb_area_median_price,
  name = 'Airbnb Median Price') +
  tm_polygons(
    col = 'median_price',
    alpha = 0.4,
    palette = 'Purples',
    title = 'Median Price ($)',
    popup.vars = c('Median Price' = 'median_price')) +
  tm_shape(
    airbnb_data,
    name = 'Airbnb Listings Distribution') +
  tm_dots(
    id = 'name',
    col = 'price',
    palette = 'PuRd',
    contrast = c(0.4,0.8),
    popup.vars = c(
      'Host Name' = 'host_name',
      'Room Type' = 'room_type',
      'Price ($)' = 'price',
      'Minimum Nights to book' = 'minimum_nights',
      'Number of Reviews' = 'number_of_reviews',
      'Availability in future 365 days' = 'availability_365'),
    title = 'Price of Airbnb Listings ($, Per Night)')+
  tm_shape(
    leed_data,
    name = 'LEED distribution') +
  tm_dots(
    id = 'ProjectName',
    col = 'CertLevel',
    palette = '-YlGnBu',
    style = 'cat',
    size = 0.06,
    popup.vars = 
      c('Address' = 'full_address',
        'Certification Level'= 'CertLevel',
        'Points Achieved' = 'PointsAchieved',
        'Leed System Version' = 'LEEDSystemVersionDisplayName',
        'Gross floor Area' = 'GrossFloorArea',
        'Unit of Measuement' = 'UnitOfMeasurement',
        'Project Type' = 'ProjectTypes',
        'Owner Organization' = 'OwnerOrganization'),
    title = 'Leed Certification Level') +
  tm_view(bbox = nbhd_bbox)

