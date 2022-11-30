# set up ------------------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(tidygeocoder)
library(purrr)

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

# First glance ------------------------------------------------------------

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

# Closest five Airbnb listings to each LEED -------------------------------

# This is a rough trial section... Ugly coding alert!!!

# Grab the airbnb dataframe's neighbourhood

airbnb_sf$neighbourhood %>%
  unique()


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
    })



leed_closest5_updated %>% nest(airbnb_id = airbnb_id)
leed_closest5_updated %>% unnest(airbnb_id = airbnb_id)







leed_closest5 <- 
  airbnb_sf$neighbourhood %>%
  unique() %>%
  
  # Iterate through leed_nbhd dataframe,
  # if the neighbourhood has at least one project, then create a distance matrix
  # Without flow control, the distance matrix creation will break
  
  map(
    ~ if(
      dim(
        leed_nbhd_sf %>%
        filter(
          neighbourhood == .x))[1] == 0){
      print('no leed')
    } else {
      
      # distance matrix (within each neighbourhood)
      
      airbnb_sf %>%
        filter(
          neighbourhood == .x)%>%
        st_distance(
          leed_nbhd_sf %>%
            filter(
              neighbourhood == .x)) %>% 
        
        # closest five
        # return the index in the 
        # airbnb data set in the specific neighbourhood
        # and extract the top 5 with smallest distance
        # (column-wise sorting)
        
        apply(
          MARGIN = 2,
          FUN = function(x){
            order(x, decreasing = FALSE)[1:5]}) %>% 
        
        # use the index returned above to retrieve airbnb listings
        
        apply(
          MARGIN = 2,
          FUN = 
            function(x,
                     data){
              data[x, ]},
          data = 
            airbnb_sf %>%
            filter(neighbourhood == .x)) 
    }) %>%
  
  # set names for the resultant object
  
  set_names(
    airbnb_sf$neighbourhood %>%
      unique())


# Trials to join back to the leed data frame (Chinatown as an example) 

## Question 1 --------------------------------------------------------------
# How to set up multiple geometry set up in the data frame
# LEED geometry(point) and Airbnb geometry(point)
# Each Airbnb listing can have multiple LEED labels,
# which means it's among top 5 for multiple buildings
# how to address this & how to map it

Chinatown_combined <- 
  
  # Filter Chinatown
  
  leed_nbhd_sf %>%
  filter(neighbourhood == 'Chinatown') %>%
  as_tibble() %>% 
  
  # Create a label column for each leed project (by order)
  
  tibble::rowid_to_column('label') %>%
  
  # Trying to join
  
  left_join(
    
    # Flatten the list of named lists object, create label for each observation
    
    do.call(rbind, leed_closest5[['Chinatown']]) %>%
      as_tibble() %>%
      
      # 11111, 22222, 333333... pattern
      
      mutate(label = as.numeric(gl(length(leed_closest5[['Chinatown']]), 5))),
    by = 'label'
  ) %>%
  
  # rename the geometry columns
  
  rename(
    'geometry_leed' = 'geometry.x',
    'geometry_airbnb' = 'geometry.y') %>% 
  
  # drop duplicate column
  
  select(., -neighbourhood.y) %>% 
  
  # rename the neighbourhood column
  rename(
    'neighbourhood' = 'neighbourhood.x') %>% 
  
  # transform back to sf
  
  st_as_sf()

## Question 2 : How to iterate through all the neighbourhoods -------------

# Not working!... for iteration
# leed_closest5 %>% 
#   names()%>% 
#   map(
#     ~ leed_nbhd_sf %>%
#       filter(neighbourhood == .x) %>% 
#       tibble::rowid_to_column('label') %>% 
#       left_join(
#         do.call(rbind, leed_closest5[[.x]]) %>%
#           as_tibble() %>% 
#           mutate(label = as.numeric(gl(length(leed_closest5[[.x]]), 5))),
#         by = 'label'
#       ) %>% 
#       rename(
#         'geometry_leed' = 'geometry.x',
#         'geometry_airbnb' = 'geometry.y'))


# Separate Chinatown LEED and Airbnb dataframe

Chinatown_leed <-
  leed_nbhd_sf %>%
  filter(neighbourhood == 'Chinatown') %>%
  tibble::rowid_to_column('label')

Chinatown_airbnb <- 
  do.call(rbind, leed_closest5[['Chinatown']]) %>%
  as_tibble() %>%
  mutate(label = as.numeric(gl(length(leed_closest5[['Chinatown']]), 5))) %>% 
  st_as_sf(crs = st_crs(Chinatown_leed))

# Map trial for "Chinatown" neighbourhood ---------------------------------

tmap_mode('view')

tm_shape(nbhd_sf %>% filter(neighbourhood == 'Chinatown'))+
  tm_polygons()+
  tmap_options(check.and.fix = TRUE) +
  
  tm_shape(Chinatown_leed)+
  tm_dots(col = 'red') +
  
  tm_shape(Chinatown_airbnb)+
  tm_dots()

