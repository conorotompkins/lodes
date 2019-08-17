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

#df_intermediate %>% 
#  as_tbl_graph() %>% 
#  activate(edges) %>% 
#  filter(jobs > 100) %>% 
#  ggraph(layout = "kk") +
#  geom_node_point(size = .3) +
#  geom_edge_fan(aes(edge_width = jobs, edge_alpha = jobs),
#                arrow = arrow(length = unit(4, 'mm')), 
#                start_cap = circle(3, 'mm'),
#                end_cap = circle(3, 'mm')) +
#  scale_edge_width_continuous(range = c(.3, 2)) +
#  scale_edge_alpha_continuous(range = c(.1, 1))

allegheny_zcta_centroids <- cbind(allegheny_zcta, st_coordinates(st_centroid(allegheny_zcta))) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  rename(x = X,
         y = Y) %>% 
  select(GEOID, x, y)

allegheny_zcta %>% 
  ggplot() +
  geom_sf() +
  geom_label(data = allegheny_zcta_centroids, aes(x, y, label = GEOID))

allegheny_zcta_centroids <- allegheny_zcta_centroids %>% 
  semi_join(df_intermediate, by = c("GEOID" = "h_zcta")) %>% 
  semi_join(df_intermediate, by = c("GEOID" = "w_zcta"))

df_intermediate <- df_intermediate %>% 
  semi_join(allegheny_zcta_centroids, by = c("h_zcta" = "GEOID")) %>% 
  semi_join(allegheny_zcta_centroids, by = c("w_zcta" = "GEOID"))

g <- df_intermediate %>% 
  as_tbl_graph(directed = TRUE)

#node_pos <- allegheny_zcta_centroids

#node_pos %>% 
#  count(GEOID, sort = TRUE)

minimum_jobs <- 200

g <- g %>% 
  activate(edges) %>% 
  filter(jobs > minimum_jobs)

graph_nodes <- g %>% 
  activate(nodes) %>% 
  as_tibble()

graph_nodes

g %>% 
  activate(edges) %>% 
  arrange(desc(jobs))

node_pos <- allegheny_zcta_centroids %>% 
  mutate(GEOID = as.numeric(GEOID)) %>% 
  arrange(GEOID) %>% 
  mutate(GEOID = as.character(GEOID))

layout <- create_layout(g, 'manual',
                        node.positions = node_pos)

manual_layout <- create_layout(graph = g,
                               layout = "manual", node.positions = node_pos)

legend_title <- str_c("Minimum: ", minimum_jobs, " commuters")
legend_title

ggraph(manual_layout) +
  geom_sf(data = allegheny_zcta) +
  #geom_node_label(aes(label = name),repel = FALSE) +
  geom_node_point(alpha = 0) +
  geom_edge_fan(aes(edge_width = jobs, edge_alpha = jobs),
                arrow = arrow(length = unit(.5, 'lines')), 
                start_cap = circle(.1, 'lines'),
                end_cap = circle(.5, 'lines'),
                color = "blue") +
  scale_edge_width_continuous(legend_title, range = c(.1, 1.5)) +
  scale_edge_alpha_continuous(legend_title, range = c(.1, 1), guide = "none") +
  labs(x = NULL,
       y = NULL,
       title = "Where do people commute from/to for work?",
       subtitle = "Based on 2015 US Census LODES dataset",
       caption = "@conor_tompkins") +
  theme_graph()


##################################
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

