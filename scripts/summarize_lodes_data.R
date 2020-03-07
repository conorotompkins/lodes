library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(tidygraph)
library(ggraph)
library(vroom)

geo_crosswalk <- vroom("data/pa_xwalk.csv.gz", col_types = cols(.default = "c"))

allegheny_tracts <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide")

st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

rivers <- area_water("PA", "Allegheny", class = "sf")

df <- vroom("data/pa_od_main_JT00_2017.csv.gz", col_types = cols(.default = "c")) %>% 
  mutate(S000 = as.numeric(S000)) %>% 
  select(h_geocode, w_geocode, S000)

df

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
  ungroup() %>% 
  arrange(desc(commuters))

df_tracts_summarized <- df_tracts_summarized %>% 
  semi_join(allegheny_tracts, by = c("h_tract" = "GEOID")) %>% 
  semi_join(allegheny_tracts, by = c("w_tract" = "GEOID"))


##QA
df_home <- df_tracts_summarized %>% 
  rename(tract = h_tract,
         commuters_out = commuters) %>% 
  select(-w_tract) %>% 
  group_by(tract) %>% 
  summarize(commuters_out = sum(commuters_out))

df_home %>% 
  count(tract, sort = TRUE) %>% 
  filter(n > 1)

df_work <- df_tracts_summarized %>% 
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

write_csv(df_tracts_summarized, "data/summarized_lodes_tracts.csv")
