library(tidyverse)
library(sf)
library(tidycensus)
library(tidygraph)
library(ggraph)
library(gganimate)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

theme_set(theme_bw())

geo_crosswalk <- read_csv("data/pa_xwalk.csv.gz", col_types = cols(.default = "c"))

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

df <- read_csv("data/pa_od_main_JT00_2015.csv.gz", col_types = cols(.default = "c"), 
               #n_max = 500000
) %>% 
  mutate(S000 = as.numeric(S000),) %>% 
  select(h_geocode, w_geocode, S000)

df_tracts_summarized <- df %>% 
  group_by(h_geocode, w_geocode) %>% 
  summarize(jobs = sum(S000)) %>% 
  ungroup() %>% 
  arrange(desc(jobs))

df_tracts_summarized <- df_tracts_summarized %>% 
  left_join(geo_crosswalk %>% select(tabblk2010, trct), by = c("h_geocode" = "tabblk2010")) %>% 
  rename(h_tract = trct) %>% 
  left_join(geo_crosswalk %>% select(tabblk2010, trct), by = c("w_geocode" = "tabblk2010")) %>% 
  rename(w_tract = trct)

df_tracts_summarized <- df_tracts_summarized %>% 
  group_by(h_tract, w_tract) %>% 
  summarize(jobs = sum(jobs)) %>% 
  ungroup()

df_map_jobs <- allegheny_tracts %>% 
  left_join(df_tracts_summarized %>% select(w_tract, jobs), by = c("GEOID" = "w_tract")) %>% 
  group_by(GEOID) %>% 
  summarize(jobs = sum(jobs))

#df_map_jobs %>% 
#  ggplot() +
#  geom_sf(aes(fill = jobs), color = NA) +
#  scale_fill_viridis_c()

df_map_homes <- allegheny_tracts %>% 
  left_join(df_tracts_summarized %>% select(h_tract, jobs), by = c("GEOID" = "h_tract")) %>% 
  group_by(GEOID) %>% 
  summarize(jobs = sum(jobs))

#df_map_homes %>% 
#  ggplot() +
#  geom_sf(aes(fill = jobs), color = NA) +
#  scale_fill_viridis_c()

df_tracts_summarized <- df_tracts_summarized %>% 
  semi_join(allegheny_tracts, by = c("h_tract" = "GEOID")) %>% 
  semi_join(allegheny_tracts, by = c("w_tract" = "GEOID"))

df_intermediate <- df_tracts_summarized %>% 
  arrange(h_tract) %>% 
  na.omit() %>% 
  filter(!(h_tract == w_tract))

df_intermediate

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

g <- df_intermediate %>% 
  as_tbl_graph(directed = TRUE)

allegheny_tracts_centroids <- cbind(allegheny_tracts, st_coordinates(st_centroid(allegheny_tracts))) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  rename(x = X,
         y = Y) %>% 
  select(GEOID, x, y)

allegheny_tracts_centroids <- allegheny_tracts_centroids %>% 
  semi_join(df_tracts_summarized, by = c("GEOID" = "h_tract"))

node_pos <- allegheny_tracts_centroids

minimum_jobs <- 200

g1 <- g %>% 
  activate(edges) %>% 
  filter(jobs > minimum_jobs)

layout <- create_layout(g1, 'manual',
                        node.positions = node_pos)

manual_layout <- create_layout(graph = g1,
                               layout = "manual", node.positions = node_pos)

legend_title <- str_c("Where do people commute from/to for work? \n", "Minimum: ", minimum_jobs, " commuters")
legend_title
ggraph(manual_layout) +
  geom_sf(data = allegheny_tracts) +
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
       title = NULL,
       subtitle = "Based on 2015 US Census LODES dataset",
       caption = "@conor_tompkins")


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

tract_nodes <- g %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  mutate(index = row_number())
selected_node <- tract_nodes %>% 
  filter(name == "42003459201") %>%
  pull(index)

tract_edges <- g %>% 
  activate(edges) %>% 
  as_tibble()
tract_edges


g2 <- g %>% 
  activate(edges) %>% 
  filter(from == selected_node)
g2 %>% 
  activate(edges) %>% 
  as_tibble() %>% 
  arrange(desc(jobs)) %>% 
  ggplot(aes(jobs)) +
  geom_density()

node_pos <- allegheny_tracts_centroids #%>% 
  #filter(GEOID != "42003020100")

layout <- create_layout(g2, 'manual',
                        node.positions = node_pos)

manual_layout <- create_layout(graph = g2,
                               layout = "manual", node.positions = node_pos)

ggraph(manual_layout) +
  geom_sf(data = allegheny_tracts) +
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
