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

all_zips <- get_acs(geography = "zip code tabulation area",
                    variables = c(total_pop = "B01003_001"),
                    geometry = TRUE,
                    output = "wide")

allegheny_zcta <- st_intersection(allegheny, all_zips) %>% 
  select(-c(GEOID, NAME)) %>% 
  rename(GEOID = GEOID.1,
         NAME = NAME.1)

allegheny_zcta %>% 
  ggplot() +
  geom_sf()

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
  left_join(geo_crosswalk %>% select(tabblk2010, zcta), by = c("h_geocode" = "tabblk2010")) %>% 
  rename(h_zcta = zcta) %>% 
  left_join(geo_crosswalk %>% select(tabblk2010, zcta), by = c("w_geocode" = "tabblk2010")) %>% 
  rename(w_zcta = zcta)

df_tracts_summarized <- df_tracts_summarized %>% 
  group_by(h_zcta, w_zcta) %>% 
  summarize(jobs = sum(jobs)) %>% 
  ungroup()

df_tracts_summarized <- df_tracts_summarized %>% 
  semi_join(allegheny_zcta, by = c("h_zcta" = "GEOID")) %>% 
  semi_join(allegheny_zcta, by = c("w_zcta" = "GEOID"))

write_csv(df_tracts_summarized, "data/df_tracts_summarized.csv")
