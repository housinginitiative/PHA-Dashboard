library(tidycensus)
library(tidyverse)

census_api_key("d9ebfd04caa0138647fbacd94c657cdecbf705e9", install = TRUE, overwrite=TRUE)

##### ACS #####
acs_call = function(year){
  dat <- get_acs(geography = "tract", variables = c(
    # DEMOGRAPHICS              
    "B01003_001", #TOTAL POPULATION
    "B01002_001", #MEDIAN AGE
    "B01001_026", #TOTAL FEMALE POP
    "B02001_002", #TOTAL WHITE ALONE
    "B02001_003", #TOTAL BLACK ALONE
    "B03003_003", #TOTAL HISPANIC/LATINO
    "C18120_006", #TOTAL UNEMPLOYED
    "B01001_003", #MALE UNDER 5
    "B01001_004", #MALE 5-9
    "B01001_005", #MALE 10-14
    "B01001_006", #MALE 15-17
    "B01001_027", #FEMALE UNDER 5
    "B01001_028", #FEMALE 5-9
    "B01001_029", #FEMALE 10-14
    "B01001_030", #FEMALE 15-17
    
    # ECONOMIC SITUATION
    "B11012_008", #TOTAL FEMALE-HEADED HOUSEHOLD
    "B17001_002", #TOTAL BELOW POVERTY
    "B19057_002", #TOTAL RECEIVING PUBLIC ASSISTANCE
    "B19013_001", #MEDIAN HOUSEHOLD INCOME
    
    # TENURE/MOBILITY/PROPERTY
    "B25003_003", #RENTER OCCUPIED
    "B11012_001", #TOTAL HOUSEHOLDS
    "B07013_001", #MOBILITY BY TENURE DENOM
    "B07013_004", #SAME HOUSE 1 YEAR AGO
    "B07013_007", #SAME COUNTY MOVE
    "B07013_010", #DIF COUNTY SAME STATE MOVE
    "B07013_013", #DIF STATE MOVE
    "B07013_016", #ABROAD MOVE
    "B25002_003", #VACANT PROPERTY
    "B25002_001", #HOUSING UNIT COUNT
    "B25002_002", #OWNER PROPERTY BY CONDITION DENOM
    "B25123_003", #OWNER OCC 1+ CONDITION
    "B25123_008", #RENTER PROPERTY BY CONDITION DENOM
    "B25123_009", #RENTER OCC 1+ CONDITION
    
    # HH INCOME
    "B19001_001", #HOUSEHOLDS W/ KNOWN INCOME
    "B19001H_014",#WHITE ALONE 100-124.99
    "B19001H_015",#WHITE ALONE 125-149.99
    "B19001H_016",#WHITE ALONE 150-199.99
    "B19001H_017",#WHITE ALONE 200+
    "B19001B_002",#BLACK ALONE UNDER 10	
    "B19001B_003",#BLACK ALONE 10-14.99	
    "B19001B_004",#BLACK ALONE 15-19.99	
    "B19001B_005" #BLACK ALONE 20-24.99
  ),
  year=year, state=42, county=101, geometry = FALSE, survey ="acs5")
  
  
  dat_processed <- 
    dat %>%
    dplyr::select( -NAME, -moe) %>%
    spread(variable, estimate) %>%
    rename(population = B01003_001, 
           median_age = B01002_001,
           female = B01001_026, 
           white = B02001_002,
           black = B02001_003, 
           hispanic = B03003_003,
           unemployed = C18120_006,
           male_under_5 = B01001_003,
           male_5_9 = B01001_004,
           male_10_14 = B01001_005,
           male_15_17 = B01001_006,
           female_under_5 = B01001_027,
           female_5_9 = B01001_028,
           female_10_14 = B01001_029,
           female_15_17 = B01001_030,
           female_hh = B11012_008,
           public_asst = B19057_002, 
           poverty = B17001_002,
           
           medhhinc = B19013_001,
           renter_hh = B25003_003,
           total_hh = B11012_001,
           mob_tenure_denom = B07013_001,
           same_house_1_year = B07013_004,
           same_county_move = B07013_007,
           dif_county_same_state_move = B07013_010,
           dif_state_move = B07013_013,
           abroad_move = B07013_016,
           vacant = B25002_003,
           housing_units = B25002_001,
           owner_condition_denom = B25002_002,
           owner_1condition = B25123_003,
           renter_condition_denom = B25123_008,
           renter_1condition = B25123_009,
           known_hh_incomes = B19001_001,
           white100_124k = B19001H_014,
           white125_149k = B19001H_015,
           white150_199k = B19001H_016,
           white200k_plus = B19001H_017,
           black_10k_below = B19001B_002,
           black_10_14k = B19001B_003,
           black_15_19k = B19001B_004,
           black_20_24k = B19001B_005) %>%
    
    mutate(
      child_male = male_under_5 + male_5_9 + male_10_14 + male_15_17,
      child_female = female_under_5 + female_5_9 + female_10_14 + female_15_17,
      child_pct = ifelse(population > 0, (child_male + child_female) / population, 0),
      female_pct = ifelse(population > 0, female / population, 0),
      white_pct = ifelse(population > 0, white / population, 0),
      black_pct = ifelse(population > 0, black / population, 0),
      hispanic_pct = ifelse(population > 0, hispanic / population, 0),
      female_hh_pct = ifelse(total_hh > 0, female_hh / total_hh, 0),
      public_asst_pct = ifelse(total_hh > 0, public_asst / total_hh, 0),
      poverty_pct = ifelse(population > 0, poverty / population, 0),
      renter_pct = ifelse(housing_units > 0, renter_hh / housing_units, 0),
      vacancy_pct = ifelse(housing_units > 0, vacant / housing_units, 0),
      same_house_pct = ifelse(mob_tenure_denom > 0, same_house_1_year / mob_tenure_denom, 0),
      same_county_move_pct = ifelse(mob_tenure_denom > 0, same_county_move / mob_tenure_denom, 0),
      dif_county_same_state_move_pct = ifelse(mob_tenure_denom > 0, dif_county_same_state_move / mob_tenure_denom, 0),
      dif_state_move_pct = ifelse(mob_tenure_denom > 0, dif_state_move / mob_tenure_denom, 0),
      abroad_move_pct = ifelse(mob_tenure_denom > 0, abroad_move / mob_tenure_denom, 0),
      owner_housing_condition_pct = ifelse(owner_condition_denom > 0, owner_1condition / owner_condition_denom, 0),
      renter_housing_condition_pct = ifelse(renter_condition_denom > 0, renter_1condition / renter_condition_denom, 0),
      unemployed_pct = ifelse(population > 0, unemployed / population, 0),
      race_ice = ifelse(known_hh_incomes > 0, ((white100_124k + white125_149k + white150_199k + white200k_plus) - (black_10k_below + black_10_14k + black_15_19k + black_20_24k)) / known_hh_incomes, 0)) %>%
    dplyr::select(population, median_age, child_male, child_female, child_pct, female_pct, white_pct, black_pct, hispanic_pct,
                  female_hh_pct, public_asst_pct, poverty_pct, medhhinc, renter_pct, vacancy_pct, same_house_pct,
                  same_county_move_pct, dif_county_same_state_move_pct, dif_state_move_pct, abroad_move_pct,
                  owner_housing_condition_pct, renter_housing_condition_pct, unemployed_pct, race_ice,
                  GEOID) %>%
    rename_at(vars(-GEOID), ~ paste0(., as.character(year)))
  
  return(dat_processed)
}

acs_2022 <- acs_call(2022)

st_write(acs_2022, "acs_2022.csv")
