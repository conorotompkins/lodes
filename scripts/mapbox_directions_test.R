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



my_route <- mb_directions(
  origin = "10 Avenue de Wagram, 75008 Paris France",
  destination = "59 Rue de Tocqueville, 75017 Paris France",
  profile = "cycling",
  steps = TRUE,
  language = "en"
)

leaflet(my_route) %>%
  addMapboxTiles(style_id = "light-v9",
                 username = "mapbox") %>%
  addPolylines()

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

lodes_od_ac_main %>% 
  ggplot(aes(commuters)) +
  geom_histogram() +
  scale_x_log10()

top_od <- lodes_od_ac_main %>% 
  arrange(desc(commuters)) %>% 
  #slice(1:20) %>% 
  select(-c(state, year, commuters))

w_tract_sf <- top_od %>% 
  select(w_tract) %>% 
  left_join(allegheny_county_tracts, by = c("w_tract" = "GEOID")) %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  rename(w_tract_geo = geometry)

w_tract_sf %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts) +
  geom_sf()
  
h_tract_sf <- top_od %>% 
  select(h_tract) %>% 
  left_join(allegheny_county_tracts, by = c("h_tract" = "GEOID")) %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  rename(h_tract_geo = geometry)

h_tract_sf %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  ggplot(aes(X, Y)) +
  geom_sf(data = allegheny_county_tracts, inherit.aes = FALSE) +
  geom_point()

h_tract_sf %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts) +
  geom_sf()

h_tract_sf %>% 
  slice(1) %>% 
  pull(h_tract_geo) %>%
  unlist()

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
  
  

combined_tract_sf <- lodes_od_ac_main %>%
  arrange(desc(commuters)) %>% 
  filter(w_tract != h_tract) %>% 
  mutate(commuter_percentile = percent_rank(commuters))

combined_tract_sf %>% 
  ggplot(aes(commuters)) +
  stat_ecdf(geom = "step") +
  coord_cartesian(xlim = c(0, 50))

combined_tract_sf <- combined_tract_sf %>% 
  slice(1:500) %>% 
  left_join(st_centroid(allegheny_county_tracts), by = c("h_tract" = "GEOID")) %>% 
  rename(h_tract_geo = geometry) %>% 
  left_join(st_centroid(allegheny_county_tracts), by = c("w_tract" = "GEOID")) %>% 
  rename(w_tract_geo = geometry) %>% 
  select(h_tract, h_tract_geo, w_tract, w_tract_geo, commuters)

#need to try to get mb_directions to work with coordinate pairs
# tract_od_directions <- combined_tract_sf %>% 
#   mutate(home_address = map_chr(h_tract_geo, mb_reverse_geocode),
#          work_address = map_chr(w_tract_geo, mb_reverse_geocode)) %>% 
#   mutate(directions = map2(home_address, work_address, ~ mb_directions(origin = .x,
#                                                                        destination = .y,
#                                                                        steps = TRUE,
#                                                                        profile = "driving")))


# tract_od_stats <- tract_od_directions %>% 
#   mutate(directions = map(directions, as_tibble)) %>% 
#   unnest(directions) %>% 
#   group_by(h_tract, home_address, w_tract, work_address) %>% 
#   summarize(duration = sum(duration),
#             distance = sum(distance))

# tract_od_stats %>% 
#   write_csv("data/tract_od_stats.csv")

tract_od_stats <- read_csv("data/tract_od_stats.csv")

tract_od_stats %>% 
  ggplot(aes(distance, duration)) +
  geom_abline(linetype = 2) +
  geom_point(alpha = .5) +
  coord_equal()

tract_od_directions %>% 
  mutate(directions = map(directions, as_tibble)) %>% 
  unnest(directions) %>% 
  select(instruction) %>% 
  View()

tract_od_directions %>% 
  select(w_tract_geo) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts) +
  geom_sf()

tract_od_directions %>% 
  count(h_tract, w_tract, sort = TRUE)

tract_od_directions %>% 
  mutate(id = row_number()) %>% 
  slice(1) %>% 
  select(h_tract_geo) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts) +
  geom_sf()
  

tract_od_directions %>% 
  mutate(id = row_number()) %>% 
  slice(1:5) %>% 
  select(id, h_tract, w_tract, directions) %>% 
  unnest(directions) %>% 
  st_as_sf() %>% 
  group_by(id, h_tract, w_tract) %>% 
  summarize() %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts) +
  geom_sf(aes(color = as.factor(id))) +
  facet_wrap(~id)
