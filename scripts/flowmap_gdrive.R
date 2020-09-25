library(tidyverse)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(ggraph)
library(tidygraph)
library(flowmapblue)
library(googlesheets4)
library(googledrive)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

nodes <- read_csv("data/nodes.csv")

node_pos <- read_csv("data/node_pos.csv")

edges <- read_csv("data/edges.csv")


##
# nodes <- nodes %>%
#   left_join(node_pos, by = c("name" = "GEOID")) %>% 
#   select(-id) %>% 
#   rename(id = label) %>% 
#   select(id, lat, lon, name)
# 
# edges <- edges %>% 
#   left_join(node_pos, by = c("origin" = "id")) %>% 
#   select(-origin) %>% 
#   rename(origin = label) %>% 
#   left_join(node_pos, by = c("dest" = "id")) %>% 
#   select(-dest) %>% 
#   rename(dest = label) %>% 
#   select(origin, dest, count) %>% 
#   filter(count > 500)
# 
# node_pos <- node_pos %>% 
#   rename(name = label) %>% 
#   select(id, name, lat, lon)


##
my_properties <- c(
  "title"="Northeast Megalopolis commuter flow",
  "description"="Miniumum 500 commuters per origin-destination pair",
  "source.name"="2017 US Census LODES",
  "source.url"="https://lehd.ces.census.gov/data/",
  "createdBy.name"="Conor Tompkins",
  #"createdBy.email"="jens@mountainmath.ca",
  "createdBy.url"="https://ctompkins.netlify.app/",
  "mapbox.mapStyle"=NA,
  "flows.sheets" = "flows",
  "colors.scheme"="interpolateViridis",
  "colors.darkMode"="yes",
  "animate.flows"="no",
  "clustering"="yes"
)

properties <- tibble(property=names(my_properties)) %>%
  mutate(value=my_properties[property])

#gs4_auth()

drive_trash("lodes_flowmapblue")

ss <- gs4_create("lodes_flowmapblue", sheets = list(properties = properties,
                                                    locations = node_pos,
                                                    flows = edges))

googledrive::drive_find("lodes_flowmapblue") %>% 
  #filter(name == "lodes_flowmapblue") %>% 
  pull(id) %>% 
  gs4_get()

# write_sheet(properties, gdrive_sheet, "properties")
# write_sheet(node_pos, gdrive_sheet, "locations")
# write_sheet(edges, gdrive_sheet, "flows")

#https://flowmap.blue/

# How to make a flow map
# 
# To visualize and publish a data set as a flow map you need to have a Google account. Sign up here if you don't.
# 
# Follow these steps:
# 
#     Open the template spreadsheet
#     Make a copy of it (find “File” / “Make a copy…” in the menu)
#     Add data to the new spreadsheet. Read more…
#     Click the “Share” button, then change the selection from “Restricted” to “Anyone with the link” in the drop-down under “Get link”. Read more…
#     Copy the link to your spreadsheet and paste it here:
