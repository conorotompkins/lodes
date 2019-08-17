library(tidyverse)
library(sf)
library(tidycensus)
library(mapview)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

allegheny_tracts <- get_decennial(geography = "tract",
                                  variables = c(total_pop = "P001001"),
                                  state = "PA",
                                  county = "Allegheny County",
                                  geometry = TRUE,
                                  output = "wide")
mapview(allegheny_tracts)
