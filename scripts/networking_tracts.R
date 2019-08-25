library(tidyverse)
library(sf)
library(tidycensus)
library(tidygraph)
library(ggraph)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

df_intermediate <- read_csv("data/df_tracts_summarized.csv", col_types = cols(.default = "c")) %>% 
  mutate(jobs = as.numeric(jobs)) %>% 
  arrange(h_tract) %>% 
  na.omit() %>% 
  filter(!(h_tract == w_tract))

allegheny_tracts <- get_decennial(geography = "tract",
                           variables = c(total_pop = "P001001"),
                           state = "PA",
                           county = "Allegheny County",
                           geometry = TRUE,
                           output = "wide")

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

allegheny_tracts_centroids <- cbind(allegheny_tracts, st_coordinates(st_centroid(allegheny_tracts))) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  rename(x = X,
         y = Y) %>% 
  select(GEOID, x, y)

allegheny_tracts %>% 
  ggplot() +
  geom_sf() +
  geom_point(data = allegheny_tracts_centroids, aes(x, y), size = .2)

#allegheny_tracts_centroids <- allegheny_tracts_centroids %>% 
#  semi_join(df_intermediate, by = c("GEOID" = "h_tract")) %>% 
#  semi_join(df_intermediate, by = c("GEOID" = "w_tract"))

#df_intermediate <- df_intermediate %>% 
#  semi_join(allegheny_tracts_centroids, by = c("h_tract" = "GEOID")) %>% 
#  semi_join(allegheny_tracts_centroids, by = c("w_tract" = "GEOID"))

#separating commutes to and from
df_home_jobs <- df_intermediate %>% 
  rename(tract = h_tract,
         commuters_out = jobs) %>% 
  select(-w_tract) %>% 
  group_by(tract) %>% 
  summarize(commuters_out = sum(commuters_out))

df_home_jobs %>% 
  count(tract, sort = TRUE)

df_work_jobs <- df_intermediate %>% 
  rename(tract = w_tract,
         commuters_in = jobs) %>% 
  select(-h_tract) %>% 
  group_by(tract) %>% 
  summarize(commuters_in = sum(commuters_in))

df_home_jobs %>% 
  count(tract, sort = TRUE)

allegheny_tracts <- allegheny_tracts %>% 
  left_join(df_home_jobs, by = c("GEOID" = "tract")) %>% 
  left_join(df_work_jobs, by = c("GEOID" = "tract")) %>% 
  replace_na(list(commuters_in = 0))

allegheny_tracts %>% 
  ggplot(aes(commuters_out, commuters_in, label = NAME)) +
  geom_point() +
  theme_bw()

allegheny_tracts %>% 
  ggplot() +
  geom_sf(aes(fill = commuters_in), size = .05) +
  scale_fill_viridis_c()

allegheny_tracts %>% 
  ggplot() +
  geom_sf(aes(fill = commuters_out), size = .05) +
  scale_fill_viridis_c()

allegheny_tracts %>% 
  mutate(diff = commuters_out - commuters_in) %>% 
  ggplot() +
  geom_sf(aes(fill = diff), size = .05) +
  scale_fill_viridis_c("Commuters in minus commuters out", direction = -1)

minimum_jobs <- 25

g <- df_intermediate %>% 
  as_tbl_graph(directed = TRUE)

#node_pos <- allegheny_zcta_centroids

#node_pos %>% 
#  count(GEOID, sort = TRUE)

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

node_pos <- allegheny_tracts_centroids

manual_layout <- create_layout(graph = g,
                               layout = "manual", node.positions = node_pos)

legend_title <- str_c("Minimum: ", minimum_jobs, " commuters")
legend_title

ggraph(manual_layout) +
  geom_sf(data = allegheny_tracts, aes(fill = commuters_out),  size = .05) +
  #geom_node_label(aes(label = name),repel = FALSE) +
  geom_node_point(alpha = 0) +
  geom_edge_fan(aes(edge_width = jobs, edge_alpha = jobs),
                arrow = arrow(length = unit(.5, 'lines')), 
                start_cap = circle(.1, 'lines'),
                end_cap = circle(.5, 'lines'),
                color = "white") +
  scale_edge_width_continuous(legend_title, range = c(.1, 1.5)) +
  scale_edge_alpha_continuous(legend_title, range = c(.1, 1), guide = "none") +
  scale_fill_viridis_c() +
  labs(x = NULL,
       y = NULL,
       title = "Where do people commute from/to for work?",
       subtitle = "Based on 2015 US Census LODES dataset",
       caption = "@conor_tompkins") +
  theme_graph()


##################################
#to filter on edges

filter_tract <- "42003409000"

selected_node <- manual_layout %>% 
  filter(name == filter_tract) %>% 
  pull(ggraph.orig_index)

g_filtered <- g %>% 
  activate(edges) %>% 
  filter(from == selected_node)

manual_layout_filtered <- create_layout(graph = g_filtered,
                               layout = "manual", node.positions = node_pos)

ggraph(manual_layout_filtered) +
  geom_sf(data = allegheny_tracts, aes(fill = commuters_out), size = .05) +
  geom_sf(data = allegheny_tracts %>%  filter(GEOID == filter_tract), color = "red", alpha = 0) +
  #geom_node_label(aes(label = name),repel = FALSE) +
  geom_node_point(alpha = 0) +
  geom_edge_fan(aes(edge_width = jobs, edge_alpha = jobs),
                arrow = arrow(length = unit(.5, 'lines')), 
                start_cap = circle(.1, 'lines'),
                end_cap = circle(.5, 'lines'),
                color = "white") +
  scale_edge_width_continuous(legend_title, range = c(.1, 1.5)) +
  scale_edge_alpha_continuous(legend_title, range = c(.1, 1), guide = "none") +
  scale_fill_viridis_c() +
  labs(x = NULL,
       y = NULL,
       title = "Where do people commute from/to for work?",
       subtitle = "Based on 2015 US Census LODES dataset",
       caption = "@conor_tompkins") +
  theme_graph()
