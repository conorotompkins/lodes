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

# test_market_square <- h_tract_sf %>% 
#   mutate(home_address = map_chr(h_tract_geo, mb_reverse_geocode),
#          work_address = "Market Square, Pittsburgh, PA")

# mb_directions(origin = test_market_square$home_address[2], 
#               destination = test_market_square$work_address[1],
#               steps = TRUE)

# test_df <- test_market_square %>% 
#   mutate(directions = map2(home_address, work_address, ~ mb_directions(origin = .x,
#                                                                        destination = .y,
#                                                                        steps = TRUE,
#                                                                        profile = "driving")))
# glimpse(test_df)

# test_df %>% 
#   mutate(directions = map(directions, as_tibble)) %>% 
#   unnest(directions) %>% 
#   group_by(home_address, work_address) %>% 
#   summarize(duration = sum(duration),
#             distance = sum(distance)) %>% 
#   ggplot(aes(distance, duration)) +
#   geom_abline(linetype = 2) +
#   geom_point() +
#   coord_equal()
  
  
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
  filter(cumulative_pct_commuters <= .1) %>% 
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

#geocode addresses, get directions
tract_od_directions <- tract_od_directions %>% 
  mutate(home_address_location_geocoded = map(home_address, mb_geocode),
         work_address_location_geocoded = map(work_address, mb_geocode)) %>% 
  mutate(directions = map2(home_address, work_address, ~ mb_directions(origin = .x,
                                                                       destination = .y,
                                                                       steps = TRUE,
                                                                       profile = "driving"))) %>% 
  select(h_tract, h_tract_geo, home_address, home_address_location_geocoded,
         w_tract, w_tract_geo, work_address, work_address_location_geocoded,
         directions, commuters)

#save as shp file
tract_od_directions_shp <- tract_od_directions %>% 
  select(h_tract, home_address, w_tract, work_address, directions, commuters) %>% 
  unnest(directions)

st_write(tract_od_directions_shp, "data/tract_od_total_shape/tract_od_total_shape.shp")

#read in shp file
tract_od_directions_test <- st_read("data/tract_od_total_shape/tract_od_total_shape.shp") %>% 
  rename(home_address = hm_ddrs,
         work_address = wrk_ddr,
         distance = distanc,
         duration = duratin,
         instruction = instrct,
         commuters = commtrs)

glimpse(tract_od_directions_test)

#calculate and save od_stats
tract_od_stats <- tract_od_directions_test %>%
  # mutate(directions = map(directions, as_tibble)) %>%
  # unnest(directions) %>%
  group_by(h_tract, home_address, w_tract, work_address) %>%
  summarize(duration = sum(duration),
            distance = sum(distance))

tract_od_stats %>%
  write_csv("data/tract_od_stats.csv")

#read in od stats
tract_od_stats <- read_csv("data/tract_od_stats.csv")

#graph od stats
tract_od_stats %>% 
  ggplot(aes(distance, duration)) +
  geom_abline(linetype = 2) +
  geom_point(alpha = .5) +
  coord_equal()

tract_od_directions %>% 
  count(h_tract, w_tract, sort = TRUE) %>% 
  filter(n > 1)
  
#map routes
tract_od_directions_test %>%
  select(h_tract, w_tract, commuters, geometry) %>% 
  group_by(h_tract, w_tract, commuters) %>% 
  summarize() %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts, size = .1, fill = NA) +
  geom_sf(aes(alpha = commuters, size = commuters)) +
  guides(color = FALSE) +
  scale_size_continuous(range = c(.3, 2.5)) +
  scale_alpha_continuous(range = c(.01, .7)) +
  theme_void()
