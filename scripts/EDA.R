library(tidyverse)
library(sf)
library(tidycensus)

theme_set(theme_bw())

df <- read_csv("data/pa_od_aux_JT00_2015.csv.gz") 

df

allegheny <- get_decennial(geography = "block",
                              variables = c(total_pop = "P001001"),
                              state = "PA",
                              county = "Allegheny County",
                              geometry = TRUE,
                              output = "wide")

allegheny_pop_map <- allegheny %>% 
  ggplot() +
  geom_sf(aes(fill = total_pop), color = NA) +
  scale_fill_viridis_c()


