#combine main and retried routes
library(tidyverse)
library(mapboxapi)
library(leaflet)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)

#read in main shp file
tract_od_directions_main <- st_read("data/tract_od_total_shape/tract_od_total_shape.shp") %>% 
  rename(home_address = hm_ddrs,
         work_address = wrk_ddr,
         distance = distanc,
         duration = duratin,
         directions = dirctns,
         instructions = instrct,
         commuters = commtrs) %>% 
  mutate(source_file = "main")

tract_od_directions_retried <- st_read("data/tract_od_total_shape_retried/tract_od_total_shape_retried.shp") %>% 
  rename(home_address = hm_ddrs,
         work_address = wrk_ddr,
         distance = distanc,
         duration = duratin,
         instructions = instrct,
         commuters = commtrs) %>% 
  mutate(source_file = "retried")

glimpse(tract_od_directions_retried)

od_filter <- tract_od_directions_retried %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  distinct(h_tract, w_tract)

tract_od_directions_combined <- tract_od_directions_main %>% 
  anti_join(od_filter) %>% 
  bind_rows(tract_od_directions_retried)

glimpse(tract_od_directions_combined)

list.files("data/tract_od_total_shape_combined", full.names = TRUE)

list.files("data/tract_od_total_shape_combined", full.names = TRUE) %>% 
  set_names() %>% 
  map(file.remove)

tract_od_directions_combined %>% 
  st_write("data/tract_od_total_shape_combined/tract_od_total_shape_combined.shp")

tract_od_stats <- tract_od_directions_combined %>% 
  unnest(directions) %>%
  group_by(h_tract, home_address, w_tract, work_address) %>%
  summarize(duration = sum(duration),
            distance = sum(distance),
            steps = n(),
            commuters = unique(commuters)) %>% 
  ungroup()

tract_od_stats %>%
  write_csv("data/tract_od_stats.csv")

tract_od_stats %>% 
  ggplot(aes(distance, duration)) +
  geom_abline(linetype = 2) +
  geom_point(alpha = .5) +
  coord_equal()
