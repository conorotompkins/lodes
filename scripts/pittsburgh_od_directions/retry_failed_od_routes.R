#try od routes that failed first time
library(tidyverse)
library(mapboxapi)
library(leaflet)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

lodes_od_ac_main <- grab_lodes(state = "pa", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "tract") %>%
  select(state, w_tract, h_tract, S000, year) %>% 
  rename(commuters = S000) %>% 
  semi_join(allegheny_county_tracts, by = c("w_tract" = "GEOID")) %>% 
  semi_join(allegheny_county_tracts, by = c("h_tract" = "GEOID")) %>% 
  select(h_tract, w_tract, commuters)

tract_od_directions_main <- st_read("data/tract_od_total_shape/tract_od_total_shape.shp") %>% 
  rename(home_address = hm_ddrs,
         work_address = wrk_ddr,
         distance = distanc,
         duration = duratin,
         directions = dirctns,
         instruction = instrct,
         commuters = commtrs) %>% 
  mutate(source_file = "main") %>% 
  group_by(h_tract, w_tract) %>% 
  summarize(commuters = unique(commuters))

failed_od_routes <- read_csv("data/bad_directions.csv",
         col_types = cols(
           h_tract = col_character(),
           w_tract = col_character()
         ))

tract_od_directions_main %>% 
  #st_drop_geometry() %>% 
  semi_join(failed_od_routes)


allegheny_county_tracts <- tracts(state = "PA", county = "Allegheny", cb = TRUE) %>% 
  select(GEOID)

failed_od_routes <- failed_od_routes %>% 
  left_join(st_centroid(allegheny_county_tracts), by = c("h_tract" = "GEOID")) %>% 
  rename(h_tract_geo = geometry) %>% 
  left_join(st_centroid(allegheny_county_tracts), by = c("w_tract" = "GEOID")) %>% 
  rename(w_tract_geo = geometry) %>% 
  select(h_tract, h_tract_geo, w_tract, w_tract_geo) %>% 
  left_join(tract_od_directions_main)

glimpse(failed_od_routes)

failed_od_routes <- failed_od_routes %>% 
  mutate(home_address = map_chr(h_tract_geo, mb_reverse_geocode),
         work_address = map_chr(w_tract_geo, mb_reverse_geocode))

glimpse(failed_od_routes)

wexford_good_address <- "3321 Wexford Rd, Gibsonia, PA 15044"

failed_od_routes <- failed_od_routes %>% 
  mutate(home_address = case_when(h_tract == "42003409000" ~ wexford_good_address,
                                  h_tract != "42003409000" ~ home_address),
         work_address = case_when(w_tract == "42003409000" ~ wexford_good_address,
                                  w_tract != "42003409000" ~ work_address))

mb_directions_possibly <- possibly(mb_directions, otherwise = NA)

#geocode addresses, get directions
failed_od_routes <- failed_od_routes %>% 
  mutate(home_address_location_geocoded = map(home_address, mb_geocode),
         work_address_location_geocoded = map(work_address, mb_geocode)) %>% 
  mutate(directions = map2(home_address, work_address, ~ mb_directions_possibly(origin = .x,
                                                                                destination = .y,
                                                                                steps = TRUE,
                                                                                profile = "driving"))) %>% 
  select(h_tract, h_tract_geo, home_address, home_address_location_geocoded,
         w_tract, w_tract_geo, work_address, work_address_location_geocoded,
         directions, commuters)

glimpse(failed_od_routes)

failed_od_routes <- failed_od_routes %>% 
  select(h_tract, home_address, w_tract, work_address, directions, commuters) %>% 
  unnest(directions)

list.files("data/tract_od_total_shape_retried", full.names = TRUE)

list.files("data/tract_od_total_shape_retried", full.names = TRUE) %>% 
  set_names() %>% 
  map(file.remove)

failed_od_routes %>% 
  st_write("data/tract_od_total_shape_retried/tract_od_total_shape_retried.shp")
