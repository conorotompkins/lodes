library(tidyverse)
library(sf)
library(tigris)

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

#get water
ac_water <- tigris::area_water(state = "PA", county = "Allegheny")

main_rivers <- ac_water %>% 
  group_by(FULLNAME) %>% 
  summarize(AWATER = sum(AWATER)) %>% 
  arrange(desc(AWATER)) %>% 
  slice(1:4)

main_rivers %>% 
  ggplot() +
  geom_sf(aes(color = FULLNAME))

main_rivers %>% 
  distinct(FULLNAME) %>% 
  st_drop_geometry()

st_crs(main_rivers)


tract_od_directions <- st_read("data/tract_od_total_shape_combined/tract_od_total_shape_combined.shp") %>% 
  rename(home_address = hm_ddrs,
         work_address = wrk_ddr,
         distance = distanc,
         duration = duratin,
         directions = dirctns,
         instructions = instrct,
         commuters = commtrs,
         source_file = sorc_fl) %>% 
  st_transform(crs = "NAD83")

glimpse(tract_od_directions)  

st_crs(tract_od_directions)

tract_od_stats <- tract_od_directions %>% 
  unnest(directions) %>%
  group_by(h_tract, home_address, w_tract, work_address) %>%
  summarize(duration = sum(duration),
            distance = sum(distance),
            steps = n(),
            commuters = unique(commuters)) %>% 
  ungroup()

commute_intersect <- tract_od_stats %>% 
  mutate(intersects_ohio = st_intersects(., main_rivers %>% 
                                           filter(FULLNAME == "Ohio Riv")) %>% as.logical(),
         intersects_allegheny = st_intersects(., main_rivers %>% 
                                                filter(FULLNAME == "Allegheny Riv")) %>% as.logical(),
         intersects_monongahela = st_intersects(., main_rivers %>% 
                                                  filter(FULLNAME == "Monongahela Riv")) %>% as.logical(),
         intersects_youghiogheny = st_intersects(., main_rivers %>% 
                                                   filter(FULLNAME == "Youghiogheny Riv")) %>% as.logical()) %>% 
  replace_na(list(intersects_ohio = FALSE,
                  intersects_allegheny = FALSE,
                  intersects_monongahela = FALSE,
                  intersects_youghiogheny = FALSE))

commute_intersect %>% 
  pivot_longer(cols = contains("intersects")) %>%
  #replace_na(list(value = FALSE))
  filter(value == TRUE) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts, size = .1, fill = "black") +
  geom_sf(aes(alpha = commuters, size = commuters), color = "#ffcc01") +
  facet_wrap(~name, ncol = 1) +
  scale_size_continuous(range = c(.3, 1.5)) +
  scale_alpha_continuous(range = c(.1, .7)) +
  theme_void()
