library(tidyverse)
library(mapboxapi)
library(leaflet)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(hrbrthemes)
library(mapdeck)
library(mapview)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

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

tract_od_stats <- tract_od_directions %>% 
  unnest(directions) %>%
  group_by(h_tract, home_address, w_tract, work_address) %>%
  summarize(duration = sum(duration),
            distance = sum(distance),
            steps = n(),
            commuters = unique(commuters)) %>% 
  ungroup() %>% 
  mutate(path_tooltip = str_c(h_tract, w_tract, sep = " to "))

glimpse(tract_od_stats) %>% 
  ggplot() +
  geom_sf()


mapdeck(
  style = mapdeck_style("dark")
  , location = c(145, -37.8)
  , zoom = 10) %>%
  add_path(
    data = roads
    , stroke_colour = "RIGHT_LOC"
    , layer_id = "path_layer"
    , tooltip = "ROAD_NAME"
    , auto_highlight = TRUE
    , legend = T
  )

"mapbox://styles/conorotompkins/ckg2f7b150nss19s5vgyltusm"

path_tooltip <- sprintf("%s to %s",
                        tract_od_stats$h_tract,
                        tract_od_stats$w_tract) %>%
  lapply(htmltools::HTML)

mapdeck(style = mapdeck_style("light"),
        location = c(-80, 40.45),
        zoom = 9) %>% 
  add_polygon(data = allegheny_county_tracts,
              stroke_colour = "#000000",
              stroke_width = 50,
              fill_opacity = 0,
              #fill_colour = "#b0b0b0",
              tooltip = "GEOID",
              auto_highlight = T,
              update_view = FALSE) %>% 
  add_path(data = tract_od_stats,
           stroke_colour = "commuters",
           stroke_width = "commuters",
           stroke_opacity = "commuters",
           layer_id = "path_layer",
           tooltip = "path_tooltip",
           auto_highlight = TRUE,
           legend = T,
           update_view = FALSE)
