#### Setup ####
# Packages
library(tidyverse)
library(tigris)
library(yelpr)
library(sf)
library(mapview)
library(httr)
library(purrr)
library(waffle)

#### Philly boundaries via tigris package ####
options(tigris_use_cache = TRUE) # set cache = TRUE to save time for future calls

zcta <- zctas(year = 2021) %>% # get all ZIPs in US (we can't filter by state unfortunately)
  rename(zip_code = GEOID20) %>% 
  st_transform("EPSG:32129") # re-project

phl_zcta <- zcta %>%
  filter(substr(zip_code, start = 1, stop = 3) == 191) %>% # filter to those in phl county
  erase_water(area_threshold = 0.9)

zip_list <- as.list(phl_zcta$zip_code) # list of zip codes to include in API query

phl_bound <- st_union(phl_zcta) # phl boundary

bg_zctas <- zcta %>%
  st_crop(xmin= 808555.2, ymin= 59543.11, xmax= 839734.8, ymax= 94453.39) %>%
  erase_water(area_threshold = 0.9)

roads <- primary_roads() %>%
  st_transform("EPSG:32129") %>%
  st_crop(xmin= 808555.2, ymin= 59543.11, xmax= 839734.8, ymax= 94453.39) %>%
  erase_water()

#### Yelp api call function ####
url <- "https://api.yelp.com/v3/businesses/search"

get_yelp = function(category, zip_code, offset_num) { # args are category of business, zipcode, and number to offset by
  

  queryString <- list(
    location = zip_code,
    term = category,
    sort_by = "best_match",
    limit = 50,
    offset = offset_num
  )
  
  response <- VERB("GET", url, query = queryString, add_headers('Authorization' = 'Bearer u6vZZfFrAnI1IHH0p40WwmEB2W8yp8l-r1m8lmLmoYJCuq8obxrgyNYsgRJXwkqD1E4ceSmLuMEIuKUwF0yvv0Pb1GOO2TwtCaDfKl1DJ_L9xSElW4kCigyWG3tCZXYx'), content_type("application/octet-stream"), accept("application/json"))
  
  
  # turn the response into a json file
  yelp.json = content(response, "parsed", flatten = TRUE, simplify = TRUE)
  
  # retrieve columns from json structure
  biz.name = data.frame(yelp.json$businesses$name)
  biz.lat = data.frame(yelp.json$businesses$coordinates.latitude)
  biz.lon = data.frame(yelp.json$businesses$coordinates.longitude)
  biz.rating = data.frame(yelp.json$businesses$rating)
  biz.addr = data.frame(yelp.json$businesses$location.address1)
  
  # bind the columns into one dataframe
  yelp.df = cbind(biz.name, biz.rating, biz.addr, biz.lat, biz.lon)  %>%
    as.data.frame()
  
  colnames(yelp.df) <- c("name", "rating", "address", "lat", "lon")
  

  
  # Replace NA values with ""
  yelp.df <- yelp.df %>% 
    mutate(across(where(is.numeric), ~replace_na(.x, 0)), # Replace NA with 0 for numeric columns
           across(where(is.character), ~replace_na(.x, ""))) # Replace NA with "" for character columns
  
  # When creating an empty dataframe, use "" as default value
  if(nrow(yelp.df) == 0) {
    yelp.df <- data.frame(name="", rating=numeric(0), address="", lat=numeric(0), lon=numeric(0), stringsAsFactors=FALSE)
  }

  return(yelp.df)
}



#### Get businesses from yelp ####
# Initialize a named list of empty dataframes
init_df_list <- function(zips) { 
  empty_df <- data.frame(name=character(0), rating=numeric(0),  address=character(0), lat=numeric(0), lon=numeric(0))
  named_list <- lapply(zips, function(zip) empty_df)
  names(named_list) <- zips
  return(named_list)
}

biz_list_0 <- init_df_list(zip_list) # Initiate list of restaurant dataframes for each offset (since the query limit is 50, offset = 1 would return 51-100)
biz_list_1 <- init_df_list(zip_list) # 10 dfs x 50 restaurants each = a 500 restaurant sample per zip code
biz_list_2 <- init_df_list(zip_list)
biz_list_3 <- init_df_list(zip_list)

biz_list_4 <- init_df_list(zip_list)
biz_list_5 <- init_df_list(zip_list)
biz_list_6 <- init_df_list(zip_list)
# 
# biz_list_7 <- init_df_list(zip_list)
# biz_list_8 <- init_df_list(zip_list)



# master list to store the dataframes
offset_list <- list(biz_list_0, biz_list_1, biz_list_2, biz_list_3
                    # biz_list_4,
                    #  biz_list_5, biz_list_6
                    # biz_list_7,
                    #  biz_list_8
                    )

# Loop through each offset (think of each offset as a page of results)
for (i in 1:length(zip_list)) {
  
  # initialize zipnum (this is so we know where we're at in the list of zips)
  zipnum <- 1
  # initialize offset (so we can pull page 1, then page 2, then page 3, etc)
  offset <- i-1
  
  # Loop through each zip code
  for (zip_code in zip_list) {
    print(paste("batch ", offset + 1, ", ", "zip", zipnum, ": ", zip_code, sep = ""))
    
    # Fetch Yelp data for the zip code and store it in the list
    offset_list[[i]][[as.character(zip_code)]] <- get_yelp("kids", as.character(zip_code), as.character(offset))
    
    zipnum <- zipnum + 1 #iterate zipnum each loop
  }
  offset <- offset + 50
}


#### Bind dataframe ####

# Combine all dataframes into one dataframe and remove duplicates
health.sf <- map_dfr(offset_list, ~ bind_rows(.x)) %>%
  unique() %>%
  st_as_sf(crs = 4326, coords = c("lon", "lat")) %>% 
  st_transform("EPSG:32129") %>%
  st_intersection(phl_bound)
  
grocery.sf <- map_dfr(offset_list, ~ bind_rows(.x)) %>%
  unique() %>%
  st_as_sf(crs = 4326, coords = c("lon", "lat")) %>% 
  st_transform("EPSG:32129") %>%
  st_intersection(phl_bound)

restaurants.sf <- map_dfr(offset_list, ~ bind_rows(.x)) %>%
  unique() %>%
  st_as_sf(crs = 4326, coords = c("lon", "lat")) %>% 
  st_transform("EPSG:32129") %>%
  st_intersection(phl_bound)

retail.sf <- map_dfr(offset_list, ~ bind_rows(.x)) %>%
  unique() %>%
  st_as_sf(crs = 4326, coords = c("lon", "lat")) %>% 
  st_transform("EPSG:32129") %>%
  st_intersection(phl_bound)

kids_activities.sf <- map_dfr(offset_list, ~ bind_rows(.x)) %>%
  unique() %>%
  st_as_sf(crs = 4326, coords = c("lon", "lat")) %>% 
  st_transform("EPSG:32129") %>%
  st_intersection(phl_bound)

# write out data so I don't have to query again
st_write(restaurants.sf, "restaurants.geojson", driver = "geojson")
st_write(health.sf, "health.geojson", driver = "geojson")
st_write(grocery.sf, "grocery.geojson", driver = "geojson")
st_write(retail.sf, "retail.geojson", driver = "geojson")
st_write(kids_activities.sf, "kids_activities.geojson", driver = "geojson")

# maps
water_rect <- st_as_sfc(st_bbox(bg_zctas), crs = "EPSG:32129")


ggplot() +
  geom_sf(data = water_rect, fill = "darkslategrey") +
  geom_sf(data = bg_zctas, color = "gray15", fill = "gray10") +
  geom_sf(data = phl_zcta, color = "gray20", fill = "gray15") +
  geom_sf(data = retail.sf, shape = 23, color = "pink", fill = "pink", size = .5) +
  labs(title = "kids activities") +
  theme_void()


library(tigris)

road <- primary_secondary_roads(year= 2021, state = "PA") %>%
  st_transform("EPSG:32129") %>%
  st_intersection(st_union(phl_zcta)) 

ggplot() +
  geom_sf(data = water_rect, fill = "darkslategrey") +
  geom_sf(data = bg_zctas, color = "gray15", fill = "gray10") +
  geom_sf(data = phl_zcta, color = "gray20", fill = "gray15") +
  geom_sf(data = road %>% st_buffer(100), shape = 23, aes(color = ""), alpha = 0.5) +
  geom_sf(data = restaurants.sf, color = "cyan", size = 1, alpha = 0.3) +
  scale_color_manual(values = "pink", name = "roads") +
  theme_void() + theme(legend.position = c(0.8, 0.2))

shooting <- st_read("/Users/annaduan/Library/CloudStorage/Box-Box/SAFMR\ Study/SAFMR\ Study\ Administrative\ Data/Data/shootings.geojson")
green <- st_read("/Users/annaduan/Desktop/GitHub/week-5/data/parks.geojson")
ggplot() +
  geom_sf(data = water_rect, fill = "lightblue") +
  geom_sf(data = bg_zctas, color = "gray80", fill = "gray85") +
  geom_sf(data = phl_zcta, color = "gray85", fill = "gray90") + 
  geom_sf(data = shooting, aes(color = ""), alpha = 0.1, size = 0.1) +
  scale_color_manual(values = "black") +
  geom_sf(data = green %>% st_make_valid() %>% st_union(), aes(fill = ""), color = NA, alpha = 0.3) +
  scale_fill_manual(values = "chartreuse4") +
  labs(fill = "Parks", color = "1 shooting") +
  theme_void() +
  theme(legend.position = c(0.8, 0.2),
        legend.text = element_text(color = "beige")) 


