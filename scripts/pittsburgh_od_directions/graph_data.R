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
tract_od_directions <- st_read("data/tract_od_total_shape/tract_od_total_shape.shp") %>% 
  rename(home_address = hm_ddrs,
         work_address = wrk_ddr,
         distance = distanc,
         duration = duratin,
         instruction = instrct,
         commuters = commtrs)

glimpse(tract_od_directions)

#read in od stats
tract_od_stats <- read_csv("data/tract_od_stats.csv")

#graph od stats
tract_od_stats %>% 
  ggplot(aes(distance, duration)) +
  geom_abline(linetype = 2) +
  geom_point(alpha = .5) +
  coord_equal() +
  theme_ipsum()

tract_od_directions %>% 
  group_by(h_tract, home_address, w_tract, work_address) %>% 
  summarize() %>% 
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
  summarize(commuters = min(commuters),
            duration = sum(duration),
            distance = sum(distance)) %>% 
  arrange(desc(commuters))

tract_od_summarized %>% 
  select(h_tract, duration) %>% 
  arrange(h_tract) %>% 
  group_by(h_tract) %>% 
  summarize(mean_duration = mean(duration)) %>% 
  full_join(allegheny_county_tracts, by = c("h_tract" = "GEOID")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = mean_duration), size = .2) +
  scale_fill_viridis_c() +
  theme_void()

tract_od_summarized %>% 
  select(w_tract, duration) %>% 
  arrange(w_tract) %>% 
  group_by(w_tract) %>% 
  summarize(mean_duration = mean(duration)) %>% 
  full_join(allegheny_county_tracts, by = c("w_tract" = "GEOID")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = mean_duration), size = .2) +
  scale_fill_viridis_c() +
  theme_void()
