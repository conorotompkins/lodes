###test mb_matrix

library(tidyverse)
library(mapboxapi)
library(leaflet)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(mapdeck)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

allegheny_county_tracts <- tracts(state = "PA", county = "Allegheny", cb = TRUE) %>% 
  select(GEOID)

lodes_od_ac_main <- grab_lodes(state = "pa", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "tract") %>%
  select(state, w_tract, h_tract, S000, year) %>% 
  rename(commuters = S000) %>% 
  semi_join(allegheny_county_tracts, by = c("w_tract" = "GEOID")) %>% 
  semi_join(allegheny_county_tracts, by = c("h_tract" = "GEOID"))

top_od <- lodes_od_ac_main %>% 
  arrange(desc(commuters)) %>% 
  slice(1:10) %>% 
  select(-c(state, year, commuters)) %>% 
  pivot_longer(cols = everything()) %>% 
  distinct(value) %>% 
  rename(GEOID = value)

top_od_tracts <- top_od %>% 
  left_join(allegheny_county_tracts) %>% 
  st_as_sf() %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

  # #st_centroid() %>% 
  # select(geometry)

mkt_sq <- mb_geocode("Market Square, Pittsburgh, PA")

test_matrix <- mb_matrix(origins = top_od_tracts, destinations = top_od_tracts, profile = "driving")

test_matrix %>% 
  as_tibble() %>% 
  rownames_to_column(var = "origin_id") %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  rename(destination_id = name,
         travel_time = value) %>% 
  mutate(destination_id = str_remove(destination_id, "^V"),
         origin_id = parse_number(origin_id),
         destination_id = parse_number(destination_id)) %>% 
  left_join(st_drop_geometry(top_od_tracts), by = c("origin_id" = "id")) %>% 
  rename(origin_GEOID = GEOID) %>% 
  left_join(st_drop_geometry(top_od_tracts), by = c("destination_id" = "id")) %>% 
  rename(destination_GEOID = GEOID) %>%
  View()
  


times <- mb_matrix(origins = top_od_tracts, destinations = mkt_sq,
          profile = "driving")

str(times)

top_od_tracts %>% 
  mutate(travel_time = times) %>% 
  arrange(desc(travel_time))

top_od_tracts <- top_od_tracts %>% 
  mutate(travel_time = times) %>% 
  arrange(desc(travel_time))

top_od_tracts %>% 
  ggplot() +
  geom_sf(aes(fill = travel_time), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "Travel time to Market Square",
       subtitle = "By census tract",
       fill = "Minutes") +
  theme_void()
  
mapdeck(style = mapdeck_style("light"), zoom = 8,
        min_zoom = 4, max_zoom = 10) %>%
  add_polygon(data = top_od_tracts,
              fill_colour = "travel_time",
              fill_opacity = 0.6,
              legend = TRUE, 
              legend_format = list( fill_colour = as.integer ))