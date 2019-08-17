library(tidyverse)
library(sf)
library(tidycensus)
library(mapview)

Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

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
  geom_sf(aes(fill = total_popE)) +
  scale_fill_viridis_c()

allegheny_zcta

mapview(allegheny_zcta)
