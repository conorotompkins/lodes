library(tidyverse)
library(sf)
library(tidycensus)

theme_set(theme_bw())

df <- read_csv("data/pa_od_main_JT00_2015.csv.gz", col_types = cols(.default = "c"), n_max = 100000) %>% 
  mutate(S000 = as.numeric(S000),
         w_tract = str_sub(w_geocode, 1, 11),
         h_tract = str_sub(h_geocode, 1, 11)) %>% 
  select(w_geocode, h_geocode, w_tract, h_tract, S000)

df

df %>% 
  count(w_geocode, sort = TRUE)

df %>% 
  count(h_geocode, sort = TRUE)

df %>% 
  count(w_geocode, h_geocode, sort = TRUE)

df %>% 
  count(w_tract, h_tract, sort = TRUE)

df_tracts_summarized <- df %>% 
  group_by(w_tract, h_tract) %>% 
  summarize(jobs = sum(S000)) %>% 
  arrange(desc(jobs))

allegheny_blocks <- get_decennial(geography = "block",
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

allegheny_tracts

allegheny_pop_map_blocks <- allegheny_blocks %>% 
  ggplot() +
  geom_sf(aes(fill = total_pop), color = NA) +
  scale_fill_viridis_c()

allegheny_pop_map_tracts <- allegheny_tracts %>% 
  ggplot() +
  geom_sf(aes(fill = total_pop), color = NA) +
  scale_fill_viridis_c()

allegheny_pop_map_tracts

allegheny_blocks_centroids <- st_centroid(allegheny_blocks)

allegheny_tracts_centroids <- st_centroid(allegheny_tracts)

allegheny_tracts_centroids %>% 
  ggplot() +
  geom_sf()

test <- allegheny_tracts %>% 
  rename(w_tract = GEOID) %>% 
  left_join(df_tracts_summarized %>% select(w_tract, h_tract, jobs))# %>% 
  #left_join(df_tracts_summarized %>% select(h_tract, jobs), by = c("GEOID" = "h_tract"))
  
test

#links
http://www.robertmanduca.com/projects/jobs.html
https://lehd.ces.census.gov/data/
  https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.3.pdf
https://walkerke.github.io/tidycensus/articles/basic-usage.html
