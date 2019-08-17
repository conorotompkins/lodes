library(tidyverse)
library(sf)
library(tidycensus)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

theme_set(theme_bw())

geo_crosswalk <- read_csv("data/pa_xwalk.csv.gz", col_types = cols(.default = "c"))

df <- read_csv("data/pa_od_main_JT00_2015.csv.gz", col_types = cols(.default = "c"), n_max = 100000) %>% 
  mutate(S000 = as.numeric(S000),
         w_tract = str_sub(w_geocode, 1, 11),
         h_tract = str_sub(h_geocode, 1, 11)) %>% 
  select(h_geocode, w_geocode, w_tract, h_tract, S000)

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
  group_by(h_geocode, w_geocode) %>% 
  summarize(jobs = sum(S000)) %>% 
  arrange(desc(jobs))

df_tracts_summarized <- df_tracts_summarized %>% 
  left_join(geo_crosswalk %>% select(tabblk2010, trct), by = c("h_geocode" = "tabblk2010")) %>% 
  rename(h_tract = trct) %>% 
  left_join(geo_crosswalk %>% select(tabblk2010, trct), by = c("w_geocode" = "tabblk2010")) %>% 
  rename(w_tract = trct)

df_tracts_summarized <- df_tracts_summarized %>% 
  group_by(h_tract, w_tract) %>% 
  summarize(jobs = sum(jobs)) %>% 
  arrange(desc(jobs))

allegheny_county <- get_decennial(geography = "county",
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

allegheny_pop_map_county <- allegheny_county %>% 
  ggplot() +
  geom_sf(aes(fill = total_pop), color = NA) +
  scale_fill_viridis_c()

allegheny_pop_map_tracts <- allegheny_tracts %>% 
  ggplot() +
  geom_sf(aes(fill = total_pop), color = NA) +
  scale_fill_viridis_c()


allegheny_pop_map_tracts

allegheny_tracts_centroids <- st_centroid(allegheny_tracts)

allegheny_tracts_centroids %>% 
  ggplot() +
  geom_sf()

test <- allegheny_tracts %>% 
  rename(w_tract = GEOID) %>% 
  left_join(df_tracts_summarized %>% select(w_tract, h_tract, jobs)) %>%
  select(w_tract, h_tract, NAME, geometry, total_pop, jobs) %>% 
  group_by(NAME) %>% 
  summarize(jobs = sum(jobs))
  
test %>% 
  ggplot() +
  geom_sf(aes(fill = jobs)) +
  scale_fill_viridis_c()



#links
http://www.robertmanduca.com/projects/jobs.html
https://lehd.ces.census.gov/data/
https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.3.pdf
https://walkerke.github.io/tidycensus/articles/basic-usage.html
