library(tidyverse)
library(sf)
library(tidycensus)
library(tidygraph)
library(ggraph)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

df_intermediate <- read_csv("data/df_tracts_summarized.csv", col_types = cols(.default = "c")) %>% 
  mutate(jobs = as.numeric(jobs)) %>% 
  arrange(h_zcta) %>% 
  na.omit() %>% 
  filter(!(h_zcta == w_zcta))

allegheny <- get_decennial(geography = "county",
                           variables = c(total_pop = "P001001"),
                           state = "PA",
                           county = "Allegheny County",
                           geometry = TRUE,
                           output = "wide")

all_zips <- get_acs(geography = "zip code tabulation area",
                    variables = c(total_pop = "B01003_001"),
                    geometry = TRUE,
                    output = "wide")

allegheny_zcta <- st_intersection(allegheny, all_zips) %>% 
  select(-c(GEOID, NAME)) %>% 
  rename(GEOID = GEOID.1,
         NAME = NAME.1)

allegheny_zcta_centroids <- cbind(allegheny_zcta, st_coordinates(st_centroid(allegheny_zcta))) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  rename(x = X,
         y = Y) %>% 
  select(GEOID, x, y)

allegheny_zcta_centroids <- allegheny_zcta_centroids %>% 
  semi_join(df_intermediate, by = c("GEOID" = "h_zcta")) %>% 
  semi_join(df_intermediate, by = c("GEOID" = "w_zcta"))

df_intermediate <- df_intermediate %>% 
  semi_join(allegheny_zcta_centroids, by = c("h_zcta" = "GEOID")) %>% 
  semi_join(allegheny_zcta_centroids, by = c("w_zcta" = "GEOID"))

g <- df_intermediate %>% 
  as_tbl_graph(directed = TRUE)

g %>% 
  activate(edges) %>% 
  as_tibble() %>% 
  arrange(desc(jobs)) %>% 
  ggplot(aes(jobs)) +
  geom_density()

node_pos <- allegheny_zcta_centroids

from_node <- "15216"

selected_node <- node_pos %>% 
  mutate(row_id = row_number()) %>% 
  filter(GEOID == from_node) %>% 
  select(row_id) %>% 
  pull(row_id)

g <- g %>% 
  activate(edges) %>% 
  filter(from == selected_node)

layout <- create_layout(g, 'manual',
                         node.positions = node_pos)

manual_layout <- create_layout(graph = g,
                                layout = "manual", node.positions = node_pos)
allegheny_zcta %>% 
  ggplot() +
  geom_sf() +
  geom_label(data = allegheny_zcta_centroids, aes(x, y, label = GEOID))



ggraph(manual_layout) +
  geom_sf(data = allegheny_zcta) +
  #geom_node_label(aes(label = name),repel = FALSE) +
  geom_node_point(alpha = 0) +
  geom_edge_fan(aes(edge_width = jobs, edge_alpha = jobs),
                arrow = arrow(length = unit(.5, 'lines')), 
                start_cap = circle(.1, 'lines'),
                end_cap = circle(.5, 'lines'),
                color = "blue") +
  scale_edge_width_continuous("Commuters", range = c(.1, 1.5)) +
  scale_edge_alpha_continuous("Commuters", range = c(.1, 1), guide = "none") +
  labs(x = NULL,
       y = NULL)
