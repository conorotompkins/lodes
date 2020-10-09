library(tidyverse)
library(mapboxapi)
library(leaflet)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(hrbrthemes)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

#get tracts
allegheny_county_tracts <- tracts(state = "PA", county = "Allegheny", cb = TRUE) %>% 
  select(GEOID)

#read in shp file
st_read("data/tract_od_total_shape_combined/tract_od_total_shape_combined.shp") %>% 
  names()

tract_od_directions <- st_read("data/tract_od_total_shape_combined/tract_od_total_shape_combined.shp") %>% 
  rename(home_address = hm_ddrs,
         work_address = wrk_ddr,
         distance = distanc,
         duration = duratin,
         directions = dirctns,
         instructions = instrct,
         commuters = commtrs,
         source_file = sorc_fl)

glimpse(tract_od_directions)

tract_od_directions %>% 
 filter(is.na(geometry))

#read in od stats
# tract_od_stats <- read_csv("data/tract_od_stats.csv",
#                            col_types = cols(
#                              h_tract = col_character(),
#                              home_address = col_character(),
#                              w_tract = col_character(),
#                              work_address = col_character(),
#                              duration = col_double(),
#                              distance = col_double(),
#                              steps = col_double(),
#                              commuters = col_double(),
#                              geometry = col_character()
#                            ))

tract_od_stats <- tract_od_directions %>% 
  unnest(directions) %>%
  group_by(h_tract, home_address, w_tract, work_address) %>%
  summarize(duration = sum(duration),
            distance = sum(distance),
            steps = n(),
            commuters = unique(commuters)) %>% 
  ungroup()

#graph od stats
tract_od_stats %>% 
  ggplot(aes(distance, duration)) +
  geom_abline(linetype = 2) +
  geom_point(alpha = .5) +
  coord_equal() +
  theme_ipsum()

tract_od_stats %>% 
  uncount(weights = commuters) %>% 
  ggplot(aes(duration)) +
  geom_density(fill = "grey") +
  theme_ipsum()

tract_od_directions %>% 
  group_by(h_tract, home_address, w_tract, work_address) %>% 
  summarize() %>% 
  ungroup() %>% 
  count(h_tract, w_tract, sort = TRUE) %>% 
  filter(n > 1)

#map routes
tract_od_directions %>%
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

tract_od_summarized <- tract_od_directions %>%
  st_drop_geometry() %>% 
  as_tibble() %>% 
  arrange(desc(commuters)) %>% 
  group_by(h_tract, w_tract) %>% 
  summarize(commuters = min(commuters, na.rm = TRUE),
            duration = sum(duration, na.rm = TRUE),
            distance = sum(distance, na.rm = TRUE)) %>% 
  arrange(desc(commuters))

tract_od_summarized %>% 
  select(h_tract, duration) %>% 
  arrange(h_tract) %>% 
  group_by(h_tract) %>% 
  summarize(mean_duration = mean(duration, na.rm = TRUE)) %>% 
  full_join(allegheny_county_tracts, by = c("h_tract" = "GEOID")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = mean_duration), size = .2) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Origin") +
  theme_void()

tract_od_summarized %>% 
  select(w_tract, duration) %>% 
  arrange(w_tract) %>% 
  group_by(w_tract) %>% 
  summarize(mean_duration = mean(duration, na.rm = TRUE)) %>% 
  full_join(allegheny_county_tracts, by = c("w_tract" = "GEOID")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = mean_duration), size = .2) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Destination") +
  theme_void()

tract_od_summarized %>% 
  select(h_tract, w_tract, duration) %>% 
  pivot_longer(contains("tract")) %>%
  group_by(name, value) %>% 
  summarize(avg_duration = mean(duration)) %>% 
  arrange(value) %>% 
  pivot_wider(names_from = name, values_from = avg_duration, values_fill = 0) %>% 
  ggplot(aes(h_tract, w_tract)) +
  geom_point(alpha = .7)


tract_od_summarized %>% 
  select(h_tract, w_tract, duration, commuters) %>%
  mutate(h_tract = fct_reorder(h_tract, commuters),
         w_tract = fct_reorder(w_tract, commuters)) %>% 
  complete(h_tract, w_tract, fill = list(duration = NA)) %>% 
  ggplot(aes(h_tract, w_tract, fill = duration)) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "grey90") +
  theme(panel.grid = element_blank())
  
