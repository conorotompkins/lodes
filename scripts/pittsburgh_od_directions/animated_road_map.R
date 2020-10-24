#create animated map of top roads used in commuter paths.
#start with 367 and 279.
#use str_detect on unsummarized directions table

library(tidyverse)
library(mapboxapi)
library(leaflet)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(hrbrthemes)
library(tidytext)
library(gganimate)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

#get tracts
allegheny_county_tracts <- tracts(state = "PA", county = "Allegheny", cb = TRUE) %>% 
  select(GEOID)

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

ac_water <- area_water("PA", "Allegheny", class = "sf")

allegheny_county_tracts <- st_erase(allegheny_county_tracts, ac_water)

main_rivers <- ac_water %>% 
  group_by(FULLNAME) %>% 
  summarize(AWATER = sum(AWATER)) %>% 
  arrange(desc(AWATER)) %>% 
  slice(1:4)

#load directions
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

#test filtering with str_detect
tract_od_directions %>% 
  filter(str_detect(instructions, "376")) %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts, size = .1, fill = "black") +
  geom_sf(aes(alpha = commuters, size = commuters), color = "#ffcc01")

#look at instructions
tract_od_directions %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  count(instructions, sort = T) %>% 
  View()

tract_od_directions %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(matches("tract"), instructions) %>% 
  mutate(intersects_i79 = str_detect(instructions, "I 79"),
         intersects_376 = str_detect(instructions, "I 376"),
         intersects_279 = str_detect(instructions, "I 279")) %>% 
  pivot_longer(cols = contains("intersects"), names_to = "intersects") %>%
  filter(value == TRUE) %>% 
  View()

highlight_paths <- tract_od_directions %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(matches("tract"), instructions) %>% 
  mutate(intersects_i79 = str_detect(instructions, "I 79"),
         intersects_376 = str_detect(instructions, "376"),
         intersects_279 = str_detect(instructions, "279")) %>% 
  pivot_longer(cols = contains("intersects"), names_to = "intersects") %>%
  mutate(intersects = str_remove(intersects, "^intersects_"),
         intersects = str_replace(intersects, "^i", "I ")) %>% 
  filter(value == TRUE) %>% 
  select(matches("tract"), intersects, value) %>% 
  distinct()

highlight_paths %>% 
  add_count(h_tract, w_tract, sort = T)

highlight_paths %>%
  count(h_tract, w_tract, sort = T)

tract_od_directions %>% 
  group_by(h_tract, w_tract) %>% 
  summarize(commuters = unique(commuters)) %>% 
  inner_join(highlight_paths, by = c("h_tract", "w_tract")) %>% 
  count(h_tract, w_tract, sort = T) %>% 
  View()

road_map <- tract_od_directions %>% 
  group_by(h_tract, w_tract) %>% 
  summarize(commuters = unique(commuters)) %>% 
  ungroup() %>% 
  inner_join(highlight_paths, by = c("h_tract", "w_tract")) %>% 
  arrange(h_tract, w_tract) %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts, size = .1, fill = "black") +
  geom_sf(data = main_rivers, color = "white") +
  geom_sf(aes(alpha = commuters, size = commuters), color = "#ffcc01") +
  scale_size_continuous(range = c(.3, 2.5)) +
  scale_alpha_continuous(range = c(.01, .7)) +
  theme_void() +
  transition_manual(intersects) +
  labs(title = "Commute path intersects: {current_frame}",
       size = "Commuters",
       alpha = "Commuters") +
  theme(plot.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"))

anim_save(filename = "output/animated_main_roads.gif", animation = road_map,
          duration = 15,
          width = 1000, height = 1000)

  


animated_route_steps <- tract_od_directions %>% 
  group_by(h_tract, w_tract) %>% 
  mutate(id = row_number()) %>% 
  # filter(h_tract == "42003409000",
  #        w_tract == "42003020100") %>% 
  select(id, h_tract, w_tract, commuters, geometry) %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts, size = .1, fill = "black") +
  geom_sf(data = main_rivers, color = "white") +
  geom_sf(aes(alpha = commuters, size = commuters), 
          color = "#ffcc01", alpha = .025) +
  scale_size_continuous(range = c(.3, 7)) +
  guides(size = guide_legend(override.aes= list(alpha = 1))) +
  labs(title = "Commuter routes between Allegheny County census tracts",
       subtitle = "Origin to destination",
       size = "Commuters") +
  theme_void(base_size = 30) +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.background = element_rect(fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(color = "white")) +
  transition_manual(id, cumulative = TRUE)

anim_save(filename = "output/animated_route_steps.gif", animation = animated_route_steps,
          duration = 10, fps = 50,
          width = 1000, height = 1000)
