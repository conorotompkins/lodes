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
##pa
lodes_od_pa_main <- grab_lodes(state = "pa", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_pa_aux <- grab_lodes(state = "pa", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##oh
lodes_od_oh_main <- grab_lodes(state = "oh", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_oh_aux <- grab_lodes(state = "oh", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##ny
lodes_od_ny_main <- grab_lodes(state = "ny", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_ny_aux <- grab_lodes(state = "ny", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##md
lodes_od_md_main <- grab_lodes(state = "md", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_md_aux <- grab_lodes(state = "md", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##nj
lodes_od_nj_main <- grab_lodes(state = "nj", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_nj_aux <- grab_lodes(state = "nj", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##wv
lodes_od_wv_main <- grab_lodes(state = "wv", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_wv_aux <- grab_lodes(state = "wv", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##de
lodes_od_de_main <- grab_lodes(state = "de", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_de_aux <- grab_lodes(state = "de", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##va
lodes_od_va_main <- grab_lodes(state = "va", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_va_aux <- grab_lodes(state = "va", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##ct
lodes_od_ct_main <- grab_lodes(state = "ct", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_ct_aux <- grab_lodes(state = "ct", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##ri
lodes_od_ri_main <- grab_lodes(state = "ri", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_ri_aux <- grab_lodes(state = "ri", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##ma
lodes_od_ma_main <- grab_lodes(state = "ma", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_ma_aux <- grab_lodes(state = "ma", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##vt
lodes_od_vt_main <- grab_lodes(state = "vt", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_vt_aux <- grab_lodes(state = "vt", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##nh
lodes_od_nh_main <- grab_lodes(state = "nh", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_nh_aux <- grab_lodes(state = "nh", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##me
lodes_od_me_main <- grab_lodes(state = "me", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_me_aux <- grab_lodes(state = "me", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

##dc
lodes_od_dc_main <- grab_lodes(state = "dc", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_od_dc_aux <- grab_lodes(state = "dc", year = 2017, 
                              lodes_type = "od", job_type = "JT00", 
                              segment = "S000", state_part = "aux", 
                              agg_geo = "county") %>% 
  select(state, w_county, h_county, S000, year) %>% 
  rename(commuters = S000)

lodes_combined <- bind_rows(lodes_od_pa_main, lodes_od_pa_aux,
                            lodes_od_ny_main, lodes_od_ny_aux,
                            lodes_od_ct_main, lodes_od_ct_aux,
                            lodes_od_ri_main, lodes_od_ri_aux,
                            lodes_od_ma_main, lodes_od_ma_aux,
                            lodes_od_vt_main, lodes_od_vt_aux,
                            lodes_od_nh_main, lodes_od_nh_aux,
                            lodes_od_me_main, lodes_od_me_aux,
                            lodes_od_md_main, lodes_od_md_aux,
                            lodes_od_nj_main, lodes_od_nj_aux,
                            lodes_od_wv_main, lodes_od_wv_aux,
                            lodes_od_de_main, lodes_od_de_aux,
                            lodes_od_va_main, lodes_od_va_aux,
                            lodes_od_dc_main, lodes_od_dc_aux)

lodes_combined %>% 
  count(h_county, w_county, sort = TRUE) %>% 
  filter(n > 1)

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
