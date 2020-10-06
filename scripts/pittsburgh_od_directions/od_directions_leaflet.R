library(tidyverse)
library(mapboxapi)
library(leaflet)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(leaflet)
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

od_lines <- tract_od_directions %>% 
  group_by(h_tract, home_address, w_tract, work_address) %>% 
  summarize(duration = sum(duration, na.rm = TRUE) %>% round(1),
            distance = sum(distance, na.rm = TRUE) %>% round(1),
            commuters = unique(commuters)) %>% 
  ungroup() %>% 
  mutate(commuters_scaled = scale(commuters, center = FALSE) * 3)

od_lines %>% 
  ggplot(aes(commuters_scaled)) +
  geom_histogram()

direction_labels <- sprintf(#"%s to %s",
                            "Home tract: %s
                            <br/>Home address: %s
                            <br/>Work tract: %s
                            <br/>Work address: %s
                            <br/>Duration: %f minutes
                            <br/>Distance: %f km
                            <br/>Commuters: %f",
                            od_lines$h_tract,
                            od_lines$home_address,
                            od_lines$w_tract,
                            od_lines$work_address,
                            od_lines$duration,
                            od_lines$distance,
                            od_lines$commuters) %>% 
  lapply(htmltools::HTML)

leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addPolygons(data = allegheny_county_tracts,
              
              weight = .75,
              
              fillOpacity = .1
              
              
              ) %>% 
  addPolylines(data = od_lines,
              weight = ~commuters_scaled,
              color = "black",
              opacity = .3,
              #
              #group = "Directions",
              #
              highlightOptions = highlightOptions(#weight = 10,
                color = "red",
                opacity = 1,
                                                   bringToFront = TRUE),
              #
              label = direction_labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "6px",
                direction = "auto")
              )


tract_od_directions_small %>% 
  filter(str_detect(h_tract, "409000"))
