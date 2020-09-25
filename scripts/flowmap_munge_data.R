library(tidyverse)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(ggraph)
library(tidygraph)
library(flowmapblue)
library(googlesheets4)
library(googledrive)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

#get lodes
states <- c("pa", "oh", "wv", "va", "dc", "de",
            "md", "ny", "ri", "ct", "ma", "vt", "nh", "me")

lodes_od_main <- grab_lodes(state = states, year = 2017, 
                            lodes_type = "od", job_type = "JT00", 
                            segment = "S000", state_part = "main", 
                            agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_aux <- grab_lodes(state = states, year = 2017, 
                           lodes_type = "od", job_type = "JT00", 
                           segment = "S000", state_part = "aux", 
                           agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_combined <- bind_rows(lodes_od_main, lodes_od_aux)

lodes_combined %>% 
  count(h_county, w_county, sort = TRUE) %>% 
  filter(n > 1)

lodes_combined %>% 
  count(h_county, w_county, sort = TRUE) %>% 
  distinct(n)

#get geometry
counties_combined <- tigris::counties(state = c("PA", "NY", "NJ", "MD", 
                                                "OH", "WV", "DE", "VA", 
                                                "DC", "MA", "CT", "VT", 
                                                "RI", "NH", "ME"), 
                                      cb = TRUE) %>% 
  arrange(STATEFP) %>% 
  left_join(fips_codes %>% distinct(state_code, state_name), by = c("STATEFP" = "state_code"))



states <- counties_combined %>% 
  group_by(state_name) %>% 
  summarize()

counties_combined %>% 
  ggplot() +
  geom_sf(aes(fill = state_name))


#create node positions
node_pos <- counties_combined %>% 
  mutate(centroid = map(geometry, st_centroid),
         x = map_dbl(centroid, 1),
         y = map_dbl(centroid, 2)) %>% 
  select(GEOID, NAME, x, y) %>% 
  arrange(GEOID) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(-NAME) %>%
  rename(lon = x,
         lat = y) %>% 
  mutate(id = row_number()) %>% 
  select(id, GEOID, lat, lon)

node_pos <- node_pos %>% 
  left_join(st_drop_geometry(counties_combined), by = c("GEOID" = "GEOID")) %>% 
  mutate(county_name = str_c(NAME, "County", sep = " "),
         name = str_c(county_name, state_name, sep = ", "))

node_pos <- node_pos %>% 
  select(id, name, lat, lon, GEOID)

# counties_combined %>% 
#   ggplot() +
#   geom_sf() +
#   geom_point(data = node_pos, aes(lon, lat))

#create network graph
network_graph <- lodes_combined %>%
  semi_join(counties_combined, by = c("w_county" = "GEOID")) %>%
  semi_join(counties_combined, by = c("h_county" = "GEOID")) %>%
  select(h_county, w_county, commuters) %>% 
  as_tbl_graph(directed = TRUE) %>% 
  activate(edges) %>% 
  filter(commuters >= 500,
         #!edge_is_loop()
  ) %>%
  activate(nodes) %>%
  arrange(name)

nodes <- network_graph %>%
  activate(nodes) %>%
  as_tibble()

edges <- network_graph %>% 
  activate(edges) %>% 
  as_tibble()

#check that nodes match up
all(node_pos$GEOID == nodes$name)

identical(node_pos$GEOID, nodes$name)

length(node_pos$GEOID) == length(nodes$name)


#save files
nodes %>% 
  write_csv("data/nodes.csv")

edges %>% 
  write_csv("data/edges.csv")

node_pos %>% 
  write_csv("data/node_pos.csv")
