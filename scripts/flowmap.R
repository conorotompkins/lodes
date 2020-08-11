library(tidyverse)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(ggraph)
library(tidygraph)
library(flowmapblue)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

#get lodes
lodes_od_pa_main <- grab_lodes(state = "pa", year = 2010, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_pa_aux <- grab_lodes(state = "pa", year = 2010, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_ny_main <- grab_lodes(state = "ny", year = 2010, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_ny_aux <- grab_lodes(state = "ny", year = 2010, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_nj_main <- grab_lodes(state = "nj", year = 2010, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_nj_aux <- grab_lodes(state = "nj", year = 2010, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_combined <- bind_rows(lodes_od_pa_main, lodes_od_pa_aux,
                            lodes_od_ny_main, lodes_od_ny_aux,
                            lodes_od_nj_main, lodes_od_nj_aux)

lodes_combined %>% 
  count(h_county, w_county, sort = TRUE)

#get geometry
pa_counties <- tigris::counties(state = "PA") %>% 
  mutate(state = "PA") %>%
  select(state, NAME, GEOID) %>% 
  arrange(GEOID)

ny_counties <- tigris::counties(state = "NY") %>% 
  mutate(state = "NY") %>%
  select(state, NAME, GEOID) %>%  
  arrange(GEOID)

nj_counties <- tigris::counties(state = "NJ") %>% 
  mutate(state = "NJ") %>%
  select(state, NAME, GEOID) %>% 
  arrange(GEOID)

counties_combined <- bind_rows(pa_counties, ny_counties, nj_counties)

states <- counties_combined %>% 
  group_by(state) %>% 
  summarize()


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
  rename(name = NAME) %>% 
  mutate(name = str_c(name, "County", sep = " "))

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
  # activate(nodes) %>% 
  # mutate(community1 = as.factor(group_infomap()),
  #        community2 = as.factor(group_edge_betweenness(directed = TRUE)),
  #        community2 = fct_lump_min(community2, 2)) %>% 
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
  as_tibble() %>% 
  rename(origin = from,
         dest = to,
         count = commuters) %>% 
  arrange(desc(count))


#check that nodes match up
all(node_pos$GEOID == nodes$name)

identical(node_pos$GEOID, nodes$name)

length(node_pos$GEOID) == length(nodes$name)


#create flow map
flowmapblue(locations = node_pos, flows = edges, 
            mapboxAccessToken = "pk.eyJ1IjoiY29ub3JvdG9tcGtpbnMiLCJhIjoiY2p2b2MxeTNqMTg3ZjRhbnQwMTk0ZGt0ZyJ9.tfEDs2o7n8kKtHvcTneG_g", 
            clustering = TRUE, darkMode = TRUE, animation = TRUE)
