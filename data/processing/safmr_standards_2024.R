# SAFMR payment Standards Oct, 2024
# 11/11/24

library(tidyverse)
library(tigris)

zips <- zctas(starts_with = "191") %>%
  select(zip_code = GEOID20) %>%
  mutate(safmr = case_when(
    zip_code %in% c(19120, 19124, 19126, 19132, 19133, 19134, 19136, 19139, 19140, 19141, 19142, 19143, 19151) ~ 1,
    zip_code %in% c(19101, 19104, 19105, 19109, 19110, 19111, 19112, 19114, 19115, 19116, 19119, 19121, 19122, 
                    19131, 19135, 19137, 19138, 19144, 19145, 19148, 19149, 19150, 19152, 19153, 19154) ~ 2,
    zip_code %in% c(19118, 19125, 19127, 19128, 19129, 19146) ~ 3,
    zip_code %in% c(19102, 19103, 19106, 19107, 19123, 19130, 19147) ~ 4
  ),
  safmr_label = case_when(safmr == 1 ~ "Traditional Rents",
                          safmr == 2 ~ "Mid Range Rents",
                          safmr == 3 ~ "Opportunity Rents",
                          safmr == 4 ~ "High Opportunity Rents"),
  cost_sro = case_when(safmr == 1 ~ 847,
                       safmr == 2 ~ 1042,
                       safmr == 3 ~ 1342,
                       safmr == 4 ~ 1522),
  cost_0br = case_when(safmr == 1 ~ 1130,
                       safmr == 2 ~ 1390,
                       safmr == 3 ~ 1790,
                       safmr == 4 ~ 2030),
  cost_1br = case_when(safmr == 1 ~ 1240,
                       safmr == 2 ~ 1540,
                       safmr == 3 ~ 1970,
                       safmr == 4 ~ 2270),
  cost_2br = case_when(safmr == 1 ~ 1480,
                       safmr == 2 ~ 1830,
                       safmr == 3 ~ 2350,
                       safmr == 4 ~ 2700),
  cost_3br = case_when(safmr == 1 ~ 1780,
                       safmr == 2 ~ 2200,
                       safmr == 3 ~ 2830,
                       safmr == 4 ~ 3250),
  cost_4br = case_when(safmr == 1 ~ 2030,
                       safmr == 2 ~ 2510,
                       safmr == 3 ~ 3220,
                       safmr == 4 ~ 3700),
  cost_5br = case_when(safmr == 1 ~ 2334,
                       safmr == 2 ~ 2886,
                       safmr == 3 ~ 3703,
                       safmr == 4 ~ 4255),
  cost_6br = case_when(safmr == 1 ~ 2639,
                       safmr == 2 ~ 3263,
                       safmr == 3 ~ 4186,
                       safmr == 4 ~ 4810),
  cost_7br = case_when(safmr == 1 ~ 2943,
                       safmr == 2 ~ 3639,
                       safmr == 3 ~ 4669,
                       safmr == 4 ~ 5365),
  cost_8br = case_when(safmr == 1 ~ 3248,
                       safmr == 2 ~ 4016,
                       safmr == 3 ~ 5152,
                       safmr == 4 ~ 5920)) %>%
  filter(!is.na(safmr)) %>%
  st_drop_geometry()

st_write(zips, "safmr_groups_2024.csv", driver = "csv")
