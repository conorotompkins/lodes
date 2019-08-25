library(tidyverse)
library(sf)
library(tidycensus)
library(tidygraph)
library(ggraph)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

theme_set(theme_bw())

geo_crosswalk <- read_csv("data/pa_xwalk.csv.gz", col_types = cols(.default = "c"))

allegheny_tracts <- get_decennial(geography = "tract",
                           variables = c(total_pop = "P001001"),
                           state = "PA",
                           county = "Allegheny County",
                           geometry = TRUE,
                           output = "wide")

allegheny_tracts %>% 
  ggplot() +
  geom_sf()

df <- read_csv("data/pa_od_main_JT00_2015.csv.gz", col_types = cols(.default = "c"), 
               #n_max = 500000
) %>% 
  mutate(S000 = as.numeric(S000)) %>% 
  select(h_geocode, w_geocode, S000)

df_tracts_summarized <- df %>% 
  group_by(h_geocode, w_geocode) %>% 
  summarize(commuters = sum(S000)) %>% 
  ungroup() %>% 
  arrange(desc(commuters))

df_tracts_summarized <- df_tracts_summarized %>% 
  left_join(geo_crosswalk %>% select(tabblk2010, trct), by = c("h_geocode" = "tabblk2010")) %>% 
  rename(h_tract = trct) %>% 
  left_join(geo_crosswalk %>% select(tabblk2010, trct), by = c("w_geocode" = "tabblk2010")) %>% 
  rename(w_tract = trct)

df_tracts_summarized <- df_tracts_summarized %>% 
  group_by(h_tract, w_tract) %>% 
  summarize(commuters = sum(commuters)) %>% 
  ungroup()

df_tracts_summarized <- df_tracts_summarized %>% 
  semi_join(allegheny_tracts, by = c("h_tract" = "GEOID")) %>% 
  semi_join(allegheny_tracts, by = c("w_tract" = "GEOID"))

write_csv(df_tracts_summarized, "data/df_tracts_summarized.csv")
