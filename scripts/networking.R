library(tidyverse)
library(sf)
library(tidycensus)
library(tidygraph)
library(ggraph)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

theme_set(theme_bw())

df <- read_csv("data/pa_od_main_JT00_2015.csv.gz", col_types = cols(.default = "c"), n_max = 500000) %>% 
  mutate(S000 = as.numeric(S000),
         w_tract = str_sub(w_geocode, 1, 11),
         h_tract = str_sub(h_geocode, 1, 11)) %>% 
  select(w_tract, h_tract, S000)

df_tracts_summarized <- df %>% 
  group_by(w_tract, h_tract) %>% 
  summarize(jobs = sum(S000)) %>% 
  ungroup() %>% 
  arrange(desc(jobs))

df_tracts_summarized %>% 
  count(w_tract, sort = TRUE)

df_tracts_summarized %>% 
  count(h_tract, sort = TRUE)

#df_tracts_summarized <- df_tracts_summarized %>% 
#  pivot_longer(cols = c("w_tract", "h_tract"), names_to = "tract_type", values_to = "GEOID") %>% 
#  mutate(tract_type = case_when(tract_type == "w_tract" ~ "work",
#                                tract_type == "h_tract" ~ "home")) %>% 
#  group_by(GEOID, tract_type) %>% 
#  summarize(jobs = sum(jobs)) %>% 
#  ungroup()

df_tracts_summarized <- df_tracts_summarized %>% 
  group_by(w_tract, h_tract) %>% 
  summarize(jobs = sum(jobs))

df_tracts_summarized %>% 
  count(GEOID, sort = TRUE)

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

allegheny_tracts_centroids <- st_centroid(allegheny_tracts)

#df_tracts_summarized %>% 
#  filter(tract_type == "work")

#work_tract_map <- allegheny_tracts %>% 
#  left_join(df_tracts_summarized) %>% 
#  filter(tract_type == "work") %>% 
#  group_by(GEOID) %>% 
#  summarize(jobs = sum(jobs)) %>% 
#  ggplot() +
#  geom_sf(aes(fill = jobs), color = NA) +
#  scale_fill_viridis_c()
#work_tract_map

#home_tract_map <- allegheny_tracts %>% 
#  left_join(df_tracts_summarized) %>% 
#  filter(tract_type == "home") %>% 
#  group_by(GEOID) %>% 
#  summarize(jobs = sum(jobs)) %>% 
#  ggplot() +
#  geom_sf(aes(fill = jobs), color = NA) +
#  scale_fill_viridis_c()
#home_tract_map

#allegheny_tracts %>% 
#  count(NAME, sort = TRUE)

#spread.sf(data, key, value, fill = NA, convert = FALSE, drop = TRUE, sep = NULL)
#allegheny_tracts_work <- allegheny_tracts %>% 
#  left_join(df_tracts_summarized %>%  filter(tract_type == "work")) %>% 
#  rename(w_tract = GEOID) %>% 
#  select(-tract_type)

#allegheny_tracts_home <- allegheny_tracts %>% 
#  left_join(df_tracts_summarized %>%  filter(tract_type == "home")) %>% 
#  rename(h_tract = GEOID) %>% 
#  select(-tract_type)

#allegheny_tracts_combined <- rbind(allegheny_tracts_work, allegheny_tracts_home)
#allegheny_tracts_combined
df_tracts_summarized

test <- allegheny_tracts %>% 
  rename(w_tract = GEOID) %>% 
  inner_join(df_tracts_summarized, by = c("w_tract" = "h_tract"))
test

tibble(w_tract = unique(allegheny_tracts$GEOID), h_tract = unique(allegheny_tracts$GEOID))

expand.grid(w_tract = unique(allegheny_tracts$GEOID), h_tract = unique(allegheny_tracts$GEOID))

allegheny_tracts %>% 
  dplyr::left_join(df_tracts_summarized) %>% 
  ggplot() +
  geom_sf(aes(fill = jobs), color = NA) +
  scale_fill_viridis_c()

allegheny_tracts %>% 
  left_join(df_tracts_summarized)

df_tracts_summarized %>% 
  as_tbl_graph(directed = TRUE) %>% 
  activate(edges) %>% 
  ggraph() +
  geom_node_point() +
  geom_edge_fan()
