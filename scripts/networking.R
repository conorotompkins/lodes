library(tidyverse)
library(sf)
library(tidycensus)
library(tidygraph)
library(ggraph)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

theme_set(theme_bw())

allegheny <- get_decennial(geography = "county",
                           variables = c(total_pop = "P001001"),
                           state = "PA",
                           county = "Allegheny County",
                           geometry = TRUE,
                           output = "wide")

allegheny_tracts <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide")

df <- read_csv("data/pa_od_main_JT00_2015.csv.gz", col_types = cols(.default = "c"), n_max = 100000) %>% 
  mutate(S000 = as.numeric(S000),
         w_tract = str_sub(w_geocode, 1, 11),
         h_tract = str_sub(h_geocode, 1, 11)) %>% 
  select(h_tract, w_tract, S000)

df_tracts_summarized <- df %>% 
  group_by(h_tract, w_tract) %>% 
  summarize(jobs = sum(S000)) %>% 
  ungroup() %>% 
  arrange(desc(jobs))

#df_tracts_summarized <- df_tracts_summarized %>% 
#  group_by(h_tract, w_tract) %>% 
#  summarize(jobs = sum(jobs))

df_tracts_summarized <- df_tracts_summarized %>% 
  semi_join(allegheny_tracts, by = c("h_tract" = "GEOID")) %>% 
  semi_join(allegheny_tracts, by = c("w_tract" = "GEOID"))

df_intermediate <- df_tracts_summarized %>% 
  arrange(h_tract) %>% 
  na.omit() %>% 
  filter(!(h_tract == w_tract))
  
df_intermediate

df_intermediate %>% 
  as_tbl_graph() %>% 
  ggraph(layout = "kk") +
  geom_node_point(size = .3) +
  geom_edge_fan(aes(edge_width = jobs, edge_alpha = jobs),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) +
  scale_edge_width_continuous(range = c(.3, 2)) +
  scale_edge_alpha_continuous(range = c(.1, 1))

g <- df_intermediate %>% 
  as_tbl_graph(directed = TRUE) %>% 
  activate(edges) %>% 
  filter(jobs > 10)

#to filter on nodes
#tract_nodes <- g %>% 
#  activate(nodes) %>% 
#  as_tibble() %>% 
#  mutate(index = row_number())

#to_tract <- "42003020100"

#to_tract_index <- tract_nodes %>% 
#  filter(name == to_tract) %>% 
#  select(index) %>% 
#  pull()

#g <- g %>% 
#  activate(edges) %>% 
#  filter(to != to_tract_index)
#g
allegheny_tracts_centroids <- cbind(allegheny_tracts, st_coordinates(st_centroid(allegheny_tracts))) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  rename(x = X,
         y = Y) %>% 
  select(GEOID, x, y)

allegheny_tracts_centroids <- allegheny_tracts_centroids %>% 
  semi_join(df_tracts_summarized, by = c("GEOID" = "h_tract"))

node_pos <- allegheny_tracts_centroids

layout <- create_layout(g, 'manual',
                     node.positions = node_pos)

manual_layout <- create_layout(graph = g,
                               layout = "manual", node.positions = node_pos)

ggraph(manual_layout) +
  geom_sf(data = allegheny_tracts) +
  #geom_node_label(aes(label = name),repel = FALSE) +
  geom_node_point(alpha = .1, size = .1) +
  geom_edge_fan(aes(edge_width = jobs, edge_alpha = jobs),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm'),
                color = "blue") +
  scale_edge_width_continuous("Home to job", range = c(.3, 2)) +
  scale_edge_alpha_continuous("Home to job", range = c(.1, 1))


