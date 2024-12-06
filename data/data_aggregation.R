#### Data Aggregation - Tract, ZCTA, Neighborhood ####

##### Libraries & Configs #####
library(sf)
library(tigris)
library(tidyverse)
library(conflicted)
library(mapview)
library(scales)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

##### Point - no aggregation #####
# landmarks <- st_read("data/point/landmarks.geojson") %>%
#   st_transform(crs = "EPSG:4326") %>%
#   group_by(PARENT_NAME) %>%
#   slice_head(n = 1) %>%
#   select(NAME) %>%
#   st_centroid() %>%
#   # Get lng and lat
#   mutate(lng = st_coordinates(geometry)[, 1],
#          lat = st_coordinates(geometry)[, 2])
# 
# # write_sf(landmarks, "data/landmarks.geojson")

##### Point - to aggregate #####  
# bus <- st_read("data/point/septa_bus_2024.geojson") %>%
#   select(route = LineAbbr) %>%
#   st_crop(st_bbox(tract_bounds))
# 
# train <- st_read("data/point/septa_train_2024.geojson") %>%
#   select(route = Route) %>%
#   st_crop(st_bbox(tract_bounds))
# 
# transit <- bind_rows(bus, train) %>%
#   st_intersection(tract_bounds) %>%
#   st_drop_geometry() %>%
#   group_by(tract) %>%
#   unique() %>%
#   summarise(transit_lines = n(),
#             transit_line_names = paste0(route, collapse = ", "))
# 
# st_write(transit, "data/tract/transit_tract.geojson", driver = "GeoJSON")
transit <- st_read("data/tract/transit_tract.geojson") %>% st_drop_geometry()

##### Polygon #####  
hs_catchment <- st_read("data/polygon/Catchments_HS_2023/Catchments_HS_2023.shp") %>%
  st_transform(crs = "EPSG:4326") %>%
  select(HS_Name) %>%
  rename(hs_catchment = HS_Name) %>%
  st_make_valid()

ms_catchment <- st_read("data/polygon/Catchments_MS_2023/Catchments_MS_2023.shp") %>%
  st_transform(crs = "EPSG:4326") %>%
  select(MS_Name) %>%
  rename(ms_catchment = MS_Name) %>%
  st_make_valid()

es_catchment <- st_read("data/polygon/Catchments_ES_2023/Catchments_ES_2023.shp") %>%
  st_transform(crs = "EPSG:4326") %>%
  select(ES_Name) %>%
  rename(es_catchment = ES_Name) %>%
  st_make_valid()

##### Neighborhood level #####
### Geometry
nb_bounds <- st_read("phl_neighs_2024.geojson") %>%
  st_make_valid() %>%
  st_transform(crs = "EPSG:4326") %>%
  select(neighborhood = MAPNAME) 


### Final
dat_nb <- nb_bounds 

##### ZCTA level #####
### Geometry
zcta_bounds <- zctas(year = "2022") %>%
  filter(substr(GEOID20, 1, 3) == "191") %>%
  st_transform(crs = "EPSG:4326") %>%
  select(zip_code = GEOID20)

safmr <- st_read("data/zcta/safmr_groups_2024.csv")

dat_zcta <- zcta_bounds %>%
  left_join(safmr, by = "zip_code") %>%
  st_as_sf()

##### Tract level #####
### Geometry
tract_bounds <- tracts(state = "PA", county = "Philadelphia") %>%
  st_transform(crs = "EPSG:4326") %>%
  select(tract = GEOID)

### Features
acs <- st_read("data/tract/acs_2022.csv") %>%
  rename(tract = GEOID) %>%
  select(-c(medhhinc2022, median_age2022, child_male2022, child_female2022, child_pct2022, female_pct2022,
           white_pct2022, black_pct2022, hispanic_pct2022, same_county_move_pct2022, dif_county_same_state_move_pct2022,
           dif_state_move_pct2022, abroad_move_pct2022, owner_housing_condition_pct2022,
           renter_housing_condition_pct2022, unemployed_pct2022, race_ice2022, 
           female_hh_pct2022, public_asst_pct2022, renter_pct2022, vacancy_pct2022))

amenities <- st_read("data/tract/amenities_tract.geojson") %>%
  pivot_wider(id_cols = c("nb_name", "geometry"), names_from = type, values_from = count_per_mi2) %>%
  st_transform(crs = "EPSG:4326") %>%
  st_centroid()

vouchers <- st_read("data/tract/vouchers_tract_2023.csv") %>%
  mutate(count = 1) %>%
  group_by(GEOID) %>%
  summarise(vouchers = sum(count)) %>%
  select(tract = GEOID, vouchers) %>%
  filter(tract %in% tract_bounds$tract)
# 
# violent <- c('100', '200', '300', '400', '800', '1700', '2000', '1500')
# crime <- read_csv("data/point/crime_2024.csv") %>%
#   drop_na(lat, lng) %>%
#   filter(!is.na(point_x) & ucr_general %in% violent) %>%
#   st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
#   st_crop(st_bbox(tract_bounds)) %>%
#   select(geometry) %>%
#   st_intersection(tract_bounds) %>%
#   group_by(tract) %>%
#   summarise(crime_incidents = n()) %>%
#   st_drop_geometry() %>%
#   left_join(tract_bounds, by = "tract") %>%
#   st_as_sf()
# 
# st_write(crime, "data/tract/crime_tract.geojson", driver = "GeoJSON")
crime <- st_read("data/tract/crime_tract.geojson") %>% st_drop_geometry()

### Final
dat_tract <- st_intersection(amenities, tract_bounds) %>%
  st_drop_geometry() %>%
  left_join(., acs, by = "tract") %>%
  left_join(., crime, by = "tract") %>%
  left_join(., vouchers, by = "tract") %>%
  left_join(., transit, by = "tract") %>%
  right_join(., tract_bounds, by = "tract") %>%
  st_as_sf() %>%
  rename(tract_neigh = nb_name)
  
##### Merge #####
downtown <- st_sf(
  geometry = st_sfc(st_point(c(-75.1639, 39.9526))),
  crs = 4326  
)

bootleg_crosswalk <- tract_bounds %>%
  st_make_valid() %>%
  st_centroid() %>%
  st_intersection(zcta_bounds) %>%
  st_intersection(nb_bounds) %>%
  st_intersection(es_catchment) %>%
  st_intersection(ms_catchment) %>%
  st_intersection(hs_catchment) %>%
  mutate(downtown_dist = as.numeric(st_distance(geometry, downtown))) %>%
  st_drop_geometry()

dat_merge <- dat_tract %>%
  left_join(., bootleg_crosswalk, by = "tract") %>%
  left_join(., st_drop_geometry(dat_nb), by = "neighborhood") %>%
  left_join(., st_drop_geometry(dat_zcta), by = "zip_code")


##### Scale #####
to_scale <- c("entertainment", "kids", "nightlife", "restaurant", "beauty", "grocery", "shopping", "arts",                             
              "education", "historic", "parks", "healthcare", "population2022", "poverty_pct2022",                   
              "same_house_pct2022", "vouchers", "safety", "transit_density", "downtown_prox")

dat_scale <- dat_merge %>%
  rename(downtown_prox = downtown_dist) %>%
  mutate(transit_density = ifelse(!is.na(transit_lines), transit_lines, 0),
         vcrime = ifelse(as.numeric(crime_incidents) >= 0 & as.numeric(population2022) > 0 & !is.na(as.numeric(population2022)), crime_incidents/as.numeric(population2022), 0),
         safety = ifelse(tract %in% c(42101036901, 42101989300), 0, vcrime), # outliers - setting to zero to avoid skewing the scale. will change scaled value to 0 
         across(all_of(to_scale), ~ as.numeric(.))) %>%
  mutate(across(all_of(to_scale), ~ rescale(.x, to = c(0, 1)))) %>%
  mutate(across(c(poverty_pct2022, safety, downtown_prox), ~ 1 - .x),  #flip undesirable scores
         safety = ifelse(tract %in% c(42101036901, 42101989300), 0, safety)) # setting outliers to 0

dat_final <- dat_scale %>%
  # replace NAs with 0
  mutate(across(all_of(to_scale), ~ replace_na(.x, 0))) %>%
  st_make_valid()
  
##### Export #####
st_write(dat_final, "panel_2024.geojson", driver = "geojson")
