library(readr)
library(tidyr)
library(rnoaa)
library(rNOMADS)
library(dplyr)

# Import urls for WWIII models
model.urls <- GetDODSDates("wave") %>% 
  write_rds("model_url.rds")

# Import wave direction data 
sc_dir <- buoy(dataset = "swden", buoyid = 46042, year = 9999)

sc_swell_dir <- sc_dir$data %>%  tail(1000) %>% arrange(desc(time)) %>% 
  drop_na(principal_wave_dir) %>% 
  slice_head(n = 1) %>% 
  select(wave_direction = mean_wave_dir) %>% 
  write_rds("sc_swell_dir.rds")

# import data wave, period, wind and wind direction data from buoy 46042
sc_waves <- buoy(dataset = "stdmet", buoyid = 46042, year = 9999, limit=500, add_units = TRUE)

# Clean data
sc <- sc_waves$data %>% tail(n= 10000) %>% 
  mutate(time = as.Date(time)) %>% 
  rename(date = time) %>% 
  arrange(desc(date)) %>% 
  drop_na(wave_height) %>% 
  write_rds("sc_waves.rds")
