library(tidyverse)
library(sf)
library(tidycensus)
library(tidygraph)
library(ggraph)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

df_intermediate <- read_csv("data/df_tracts_summarized_allegheny.csv", col_types = cols(.default = "c")) %>% 
  mutate(commuters = as.numeric(commuters)) %>% 
  arrange(h_tract) %>% 
  na.omit() %>% 
  filter(!(h_tract == w_tract))

allegheny_tracts <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide")

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

#separating commutes to and from
df_home <- df_intermediate %>% 
  rename(tract = h_tract,
         commuters_out = commuters) %>% 
  select(-w_tract) %>% 
  group_by(tract) %>% 
  summarize(commuters_out = sum(commuters_out))

df_home %>% 
  count(tract, sort = TRUE) %>% 
  filter(n > 1)

df_work <- df_intermediate %>% 
  rename(tract = w_tract,
         commuters_in = commuters) %>% 
  select(-h_tract) %>% 
  group_by(tract) %>% 
  summarize(commuters_in = sum(commuters_in))

df_home %>% 
  count(tract, sort = TRUE) %>% 
  filter(n > 1)

allegheny_tracts <- allegheny_tracts %>% 
  left_join(df_home, by = c("GEOID" = "tract")) %>% 
  left_join(df_work, by = c("GEOID" = "tract")) %>% 
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
  ggplot(aes(commuters_out, total_pop)) +
  geom_point() +
  theme_bw()

allegheny_tracts %>% 
  mutate(diff = commuters_in - commuters_out) %>% 
  ggplot() +
  geom_sf(aes(fill = diff), size = .05) +
  scale_fill_viridis_c("Commuters in minus commuters out", direction = 1)

minimum_commuters <- 100

g <- df_intermediate %>% 
  as_tbl_graph(directed = TRUE)

g

g_main <- g %>% 
  activate(edges) %>% 
  filter(commuters > minimum_commuters)

g_main

graph_nodes <- g_main %>% 
  activate(nodes) %>% 
  as_tibble()

graph_nodes

g_main %>% 
  activate(edges) %>% 
  arrange(desc(commuters))

node_pos <- allegheny_tracts_centroids

manual_layout <- create_layout(graph = g_main,
                               layout = node_pos)

manual_layout %>% 
  as_tibble()

legend_title <- str_c("Minimum: ", minimum_commuters, " commuters")
legend_title

main_graph <- ggraph(manual_layout) +
  geom_sf(data = allegheny_tracts, size = .1, fill = NA, color = "grey") +
  geom_node_point(alpha = 0) +
  geom_edge_fan(aes(edge_width = commuters, 
                    edge_alpha = commuters),
                arrow = arrow(length = unit(.5, 'lines')), 
                start_cap = circle(.1, 'lines'),
                end_cap = circle(.2, 'lines'),
                color = "white",
                strength = .5) +
  scale_edge_width_continuous(legend_title, range = c(.1, 1.5)) +
  scale_edge_alpha_continuous(legend_title, range = c(.1, .8), 
                              #guide = "none"
                              ) +
  scale_fill_viridis_c() +
  labs(x = NULL,
       y = NULL,
       title = "Where do people commute from/to for work?",
       subtitle = "Not including with-tract commuters",
       caption = "Based on 2015 US Census LODES dataset | @conor_tompkins") +
  theme_graph() +
  theme(legend.background = element_rect(fill = "light grey"),
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        panel.background = element_rect(fill = "black"))
main_graph

ggsave("output/images/main_graph.png", main_graph, device = "png",  width = 12, height = 12)
###########
#facet on top 5 nodes
minimum_commuters <- 5

top_work_tracts <- df_home %>% 
  arrange(desc(commuters_out)) %>% 
  top_n(5, commuters_out) %>% 
  select(tract)

g_facet <- g %>% 
  activate(edges) %>% 
  left_join(manual_layout %>% select(.ggraph.index, name), by = c("from" = ".ggraph.index")) %>% 
  semi_join(top_work_tracts, by = c("name" = "tract")) %>% 
  filter(commuters > minimum_commuters)

g_facet %>% 
  arrange(desc(commuters)) %>% 
  activate(edges)

manual_layout_faceted <- create_layout(graph = g_facet,
                                        layout = node_pos)

legend_title <- str_c("Minimum: ", minimum_commuters, " commuters")
legend_title

manual_layout_faceted

ggraph(manual_layout_faceted) +
  geom_sf(data = allegheny_tracts, size = .1, fill = NA, color = "light grey") +
  geom_edge_fan(aes(edge_width = commuters, edge_alpha = commuters),
            arrow = arrow(length = unit(.5, 'lines')),
            start_cap = circle(.1, 'lines'),
            end_cap = circle(.2, 'lines'),
            color = "red",
            strength = .5) +
  facet_edges(~name) +
  scale_edge_width_continuous(legend_title, range = c(.1, 1.5)) +
  scale_edge_alpha_continuous(legend_title, range = c(.1, 1), 
                              #guide = "none"
  ) +
  scale_fill_viridis_c() +
  labs(x = NULL,
       y = NULL,
       title = "Where do people commute from/to for work?",
       #subtitle = str_c("From tract", filter_tract, sep = " "),
       caption = "Based on 2015 US Census LODES dataset | @conor_tompkins") +
  theme_graph() +
  theme(legend.background = element_rect(fill = "light grey"),
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        panel.background = element_rect(fill = "black"))

##################################
#exclude downtown tract
minimum_commuters <- 100

g_filtered <- g %>% 
  activate(edges) %>% 
  filter(commuters > minimum_commuters)

g_filtered

filter_tract <- "42003020100"

manual_layout %>% 
  as_tibble()

selected_node <- manual_layout %>% 
  filter(name != filter_tract) %>% 
  pull(.ggraph.orig_index)

g_filtered <- g_filtered %>% 
  activate(edges) %>% 
  filter(to %in% selected_node)

g_filtered

manual_layout_filtered <- create_layout(graph = g_filtered,
                                        layout = node_pos)

legend_title <- str_c("Minimum: ", minimum_commuters, " commuters")
legend_title

ggraph(manual_layout_filtered) +
  geom_sf(data = allegheny_tracts, size = .1, fill = NA, color = "light grey") +
  #geom_sf(data = allegheny_tracts %>%  filter(GEOID == filter_tract), 
  #        fill = "grey") +
  #geom_sf(data = allegheny_tracts_highlight, color = "red", alpha = 0) +
  geom_edge_fan(aes(edge_width = commuters, edge_alpha = commuters
            ),
            arrow = arrow(length = unit(.5, 'lines')),
            start_cap = circle(.1, 'lines'),
            end_cap = circle(.2, 'lines'),
            color = "white",
            strength = .5) +
  scale_edge_width_continuous(legend_title, range = c(.1, 1.5)) +
  scale_edge_alpha_continuous(legend_title, range = c(.1, .8), 
                              #guide = "none"
  ) +
  scale_fill_viridis_c() +
  labs(x = NULL,
       y = NULL,
       title = "Where do people commute from/to for work?",
       subtitle = "Exluding Downtown tract",
       caption = "Based on 2015 US Census LODES dataset | @conor_tompkins") +
  theme_graph() +
  theme(legend.background = element_rect(fill = "light grey"),
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        panel.background = element_rect(fill = "black"))

##################################
#to filter on edges

minimum_commuters <- 15

g_filtered <- g %>% 
  activate(edges) %>% 
  filter(commuters > minimum_commuters)

g_filtered

filter_tract <- "42003473500"

allegheny_tracts_highlight <- allegheny_tracts %>% 
  semi_join(df_intermediate %>% 
              filter(h_tract == filter_tract), by = c("GEOID" = "w_tract")) %>% 
  filter(commuters_in > minimum_commuters)

allegheny_tracts_highlight

manual_layout

selected_node <- manual_layout %>% 
  filter(name == filter_tract) %>% 
  pull(.ggraph.orig_index)

g_filtered <- g_filtered %>% 
  activate(edges) %>% 
  filter(from == selected_node)

g_filtered

manual_layout_filtered <- create_layout(graph = g_filtered,
                                        layout = node_pos)

zoom_x <- manual_layout_filtered %>% 
  filter(.ggraph.orig_index == selected_node) %>% 
  pull(x)

zoom_y <- manual_layout_filtered %>% 
  filter(.ggraph.orig_index == selected_node) %>% 
  pull(y)

zoom_magnitude <- .25

legend_title <- str_c("Minimum: ", minimum_commuters, " commuters")
legend_title

manual_layout

ggraph(manual_layout_filtered) +
  geom_sf(data = allegheny_tracts, size = .1, fill = NA) +
  geom_sf(data = allegheny_tracts %>%  filter(GEOID == filter_tract), 
          fill = "grey") +
  #geom_sf(data = allegheny_tracts_highlight, color = "red", alpha = 0) +
  geom_edge_fan(aes(edge_width = commuters, #edge_alpha = commuters
                    ),
                arrow = arrow(length = unit(.5, 'lines')),
                start_cap = circle(.1, 'lines'),
                end_cap = circle(.2, 'lines'),
                color = "red",
                strength = .5) +
  coord_sf(xlim = c(zoom_x + zoom_magnitude, zoom_x -zoom_magnitude), 
           ylim = c(zoom_y + zoom_magnitude, zoom_y - zoom_magnitude)) +
  scale_edge_width_continuous(legend_title, range = c(.1, 1.5)) +
  scale_edge_alpha_continuous(legend_title, range = c(.1, .8), 
                              #guide = "none"
  ) +
  scale_fill_viridis_c() +
  labs(x = NULL,
       y = NULL,
       title = "Where do people commute from/to for work?",
       subtitle = str_c("From tract", filter_tract, sep = " "),
       caption = "Based on 2015 US Census LODES dataset | @conor_tompkins") +
  theme_graph() +
  theme(legend.background = element_rect(fill = "light grey"),
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        panel.background = element_rect(fill = "black"))