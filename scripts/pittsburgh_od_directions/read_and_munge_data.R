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

#get tracts
allegheny_county_tracts <- tracts(state = "PA", county = "Allegheny", cb = TRUE) %>% 
  select(GEOID)

allegheny_county_tracts %>% 
  ggplot() +
  geom_sf()

#get lodes
##pa
lodes_od_ac_main <- grab_lodes(state = "pa", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "tract") %>%
  select(state, w_tract, h_tract, S000, year) %>% 
  rename(commuters = S000) %>% 
  semi_join(allegheny_county_tracts, by = c("w_tract" = "GEOID")) %>% 
  semi_join(allegheny_county_tracts, by = c("h_tract" = "GEOID"))

lodes_od_ac_main %>% 
  arrange(desc(commuters))

#munge od data
combined_tract_sf <- lodes_od_ac_main %>%
  arrange(desc(commuters)) %>% 
  filter(w_tract != h_tract) %>% 
  mutate(commuter_percentile = percent_rank(commuters))

combined_tract_sf %>% 
  select(commuters) %>% 
  arrange(desc(commuters)) %>% 
  mutate(id = row_number(),
         pct_commuters = commuters / sum(commuters),
         cumulative_pct_commuters = cumsum(pct_commuters)) %>% 
  ggplot(aes(id, cumulative_pct_commuters)) +
  geom_step()

combined_tract_sf %>% 
  ggplot(aes(commuters)) +
  geom_histogram()

combined_tract_sf_small <- combined_tract_sf %>% 
  select(h_tract, w_tract, commuters) %>% 
  arrange(desc(commuters)) %>% 
  mutate(id = row_number(),
         pct_commuters = commuters / sum(commuters),
         cumulative_pct_commuters = cumsum(pct_commuters)) %>% 
  filter(cumulative_pct_commuters <= .2) %>% 
  select(h_tract, w_tract, commuters)

combined_tract_sf_small <- combined_tract_sf_small %>% 
  left_join(st_centroid(allegheny_county_tracts), by = c("h_tract" = "GEOID")) %>% 
  rename(h_tract_geo = geometry) %>% 
  left_join(st_centroid(allegheny_county_tracts), by = c("w_tract" = "GEOID")) %>% 
  rename(w_tract_geo = geometry) %>% 
  select(h_tract, h_tract_geo, w_tract, w_tract_geo, commuters)

#get addresses for tract centroids
###need to try to get mb_directions to work with coordinate pairs
tract_od_directions <- combined_tract_sf_small %>%
  mutate(home_address = map_chr(h_tract_geo, mb_reverse_geocode),
         work_address = map_chr(w_tract_geo, mb_reverse_geocode))

#replace bad address with good address
wexford_good_address <- "3321 Wexford Rd, Gibsonia, PA 15044"

tract_od_directions <- tract_od_directions %>% 
  mutate(home_address = case_when(h_tract == "42003409000" ~ wexford_good_address,
                                  h_tract != "42003409000" ~ home_address),
         work_address = case_when(w_tract == "42003409000" ~ wexford_good_address,
                                  w_tract != "42003409000" ~ work_address))

mb_directions_possibly <- possibly(mb_directions, otherwise = NA)

#geocode addresses, get directions
tract_od_directions <- tract_od_directions %>% 
  mutate(home_address_location_geocoded = map(home_address, mb_geocode),
         work_address_location_geocoded = map(work_address, mb_geocode)) %>% 
  mutate(directions = map2(home_address, work_address, ~ mb_directions_possibly(origin = .x,
                                                                       destination = .y,
                                                                       steps = TRUE,
                                                                       profile = "driving"))) %>% 
  select(h_tract, h_tract_geo, home_address, home_address_location_geocoded,
         w_tract, w_tract_geo, work_address, work_address_location_geocoded,
         directions, commuters)

glimpse(tract_od_directions)

tract_od_directions %>% 
  filter(is.na(directions)) %>% 
  select(h_tract, w_tract, commuters) %>% 
  summarize(commuters = sum(commuters))

tract_od_directions %>% 
  filter(is.na(directions)) %>% 
  select(h_tract, w_tract) %>% 
  write_csv("data/bad_directions.csv")


#save as shp file
tract_od_directions_shp <- tract_od_directions %>%
  select(h_tract, home_address, w_tract, work_address, directions, commuters) %>% 
  unnest(directions)

glimpse(tract_od_directions_shp)

list.files("data/tract_od_total_shape", full.names = TRUE)

list.files("data/tract_od_total_shape", full.names = TRUE) %>% 
  set_names() %>% 
  map(file.remove)

st_write(tract_od_directions_shp, "data/tract_od_total_shape/tract_od_total_shape.shp")


#other possible geocoder and routefinder APIs
#hereR
##https://munterfinger.github.io/hereR/
##https://github.com/munterfinger/hereR/

#OSRM
##http://project-osrm.org/
##https://github.com/rCarto/osrm

#valhalla
##https://valhalla.readthedocs.io/en/latest/api/