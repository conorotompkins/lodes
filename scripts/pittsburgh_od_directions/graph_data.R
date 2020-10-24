library(tidyverse)
library(mapboxapi)
library(leaflet)
library(tidycensus)
library(janitor)
library(lehdr)
library(tigris)
library(sf)
library(hrbrthemes)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

lodes_od_ac_main <- grab_lodes(state = "pa", year = 2017, 
                               lodes_type = "od", job_type = "JT00", 
                               segment = "S000", state_part = "main", 
                               agg_geo = "tract") %>%
  select(state, w_tract, h_tract, S000, year) %>% 
  rename(commuters = S000) %>% 
  mutate(intra_tract_commute = h_tract == w_tract)

#95% work outside of the tract they live in
lodes_od_ac_main %>% 
  group_by(intra_tract_commute) %>% 
  summarize(commuters = sum(commuters)) %>% 
  ungroup() %>% 
  mutate(pct_commuters = commuters / sum(commuters)) %>% 
  janitor::adorn_totals()

#get tracts
allegheny_county_tracts <- tracts(state = "PA", county = "Allegheny", cb = TRUE) %>% 
  select(GEOID)

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

ac_water <- area_water("PA", "Allegheny", class = "sf")

allegheny_county_tracts <- st_erase(allegheny_county_tracts, ac_water)

main_rivers <- ac_water %>% 
  group_by(FULLNAME) %>% 
  summarize(AWATER = sum(AWATER)) %>% 
  arrange(desc(AWATER)) %>% 
  slice(1:4)

#read in shp file
st_read("data/tract_od_total_shape_combined/tract_od_total_shape_combined.shp") %>% 
  names()

tract_od_directions <- st_read("data/tract_od_total_shape_combined/tract_od_total_shape_combined.shp") %>% 
  rename(home_address = hm_ddrs,
         work_address = wrk_ddr,
         distance = distanc,
         duration = duratin,
         directions = dirctns,
         instructions = instrct,
         commuters = commtrs,
         source_file = sorc_fl)

glimpse(tract_od_directions)

tract_od_directions %>% 
 filter(is.na(geometry))

tract_od_stats <- tract_od_directions %>% 
  unnest(directions) %>%
  group_by(h_tract, home_address, w_tract, work_address) %>%
  summarize(duration = sum(duration),
            distance = sum(distance),
            steps = n(),
            commuters = unique(commuters)) %>% 
  ungroup()

# 92,000 commuters change tracts
tract_od_stats %>% 
  st_drop_geometry() %>% 
  summarize(commuters = sum(commuters))

#i focus on top 20% of od by commuters
top_20 <- lodes_od_ac_main %>% 
  filter(intra_tract_commute == FALSE) %>% 
  select(commuters) %>% 
  arrange(desc(commuters)) %>% 
  mutate(id = row_number(),
         pct_commuters = commuters / sum(commuters),
         cumulative_pct_commuters = cumsum(pct_commuters))

top_20 %>% 
  filter(cumulative_pct_commuters <= .200,
         cumulative_pct_commuters > .199
  )


# top_20 %>% 
#   #filter(cumulative_pct_commuters < .6) %>% 
#   ggplot(aes(id, cumulative_pct_commuters)) +
#   geom_ribbon(data = top_20 %>% 
#                 filter(cumulative_pct_commuters < .2),
#               aes(ymax = cumulative_pct_commuters, ymin = 0, 
#                   xmin = 0, xmax = 10370),
#               fill = "green") +
#   geom_ribbon(data = top_20 %>% 
#                 filter(cumulative_pct_commuters >= .2),
#               aes(ymax = cumulative_pct_commuters, ymin = 0, 
#                   xmin = 10370, xmax = max(top_20$id)),
#               fill = "grey") +
#   geom_step() +
#   geom_segment(x = 10370, xend = 10370,
#                y = 0, yend = .2,
#                lty = 2) +
#   #coord_cartesian(x = c(0, 20000), y = c(0, .5)) +
#   scale_y_percent() +
#   labs(title = "Top 20% of commuters that change tracts",
#        x = NA,
#        y = "Cumulative percent of commuters") +
#   theme_ipsum() +
#   theme(axis.text.x = element_blank())

top_20 %>% 
  mutate(top_20_flag = case_when(cumulative_pct_commuters > .2 ~ "Top 20%",
                                 cumulative_pct_commuters <= .2 ~ "Outside top 20%")) %>% 
  group_by(top_20_flag) %>% 
  summarize(commuters = sum(commuters)) %>% 
  ungroup() %>% 
  mutate(top_20_flag = as.factor(top_20_flag) %>% fct_rev) %>% 
  ggplot(aes(top_20_flag, commuters)) +
  geom_col(color = "black", fill = "grey") +
  scale_y_comma() +
  labs(title = "Most commuters are in top 20% of origin-destination tract pairs",
       x = NULL,
       y = "Commuters") +
  theme_ipsum()

#graph od stats
tract_od_stats %>% 
  ggplot(aes(distance, duration, size = commuters)) +
  geom_abline(linetype = 2) +
  geom_point(alpha = .1) +
  coord_equal() +
  theme_ipsum() +
  labs(title = "Commutes between census tracts",
       subtitle = "Allegheny County, PA",
       x = "Distance",
       y = "Duration",
       size = "Commuters")

median_duration <- tract_od_stats %>% 
  uncount(weights = commuters) %>% 
  summarize(median_duration = median(duration)) %>% 
  pull(median_duration)

tract_od_stats %>% 
  uncount(weights = commuters) %>% 
  ggplot(aes(duration)) +
  geom_density(fill = "grey") +
  geom_vline(xintercept = median_duration, lty = 2, color = "red") +
  annotate("text", x = 21, y = .05, label = "median", color = "red") +
  theme_ipsum() +
  labs(title = "Trip duration",
       x = "Duration",
       y = "Density of observations")

tract_od_directions %>% 
  group_by(h_tract, home_address, w_tract, work_address) %>% 
  summarize() %>% 
  ungroup() %>% 
  count(h_tract, w_tract, sort = TRUE) %>% 
  filter(n > 1)

#map routes
commute_path_road_map <- tract_od_stats %>% 
  ggplot() +
  geom_sf(data = allegheny_county_tracts, size = .1, fill = "black") +
  geom_sf(data = main_rivers, color = "white") +
  geom_sf(aes(#alpha = commuters, 
              size = commuters), 
          color = "#ffcc01",
          alpha = .025) +
  guides(color = FALSE) +
  scale_size_continuous(range = c(.3, 2.5)) +
  #scale_alpha_continuous(range = c(.01, .7)) +
  theme_void() +
  labs(title = "Commuter routes between Allegheny County census tracts",
       subtitle = "Driving routes",
       alpha = "Commuters",
       size = "Commuters")

commute_path_road_map

commute_path_road_map_big <- commute_path_road_map +
  scale_size_continuous(range = c(.3, 7)) +
  guides(size = guide_legend(override.aes= list(alpha = 1))) +
  #scale_alpha_continuous(range = c(.01, .2)) +
  #guides(alpha = FALSE) +
  theme_void(base_size = 30) +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.background = element_rect(fill = "black"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        plot.title = element_text(color = "white"))

commute_path_road_map_big %>% 
  ggsave(filename = "output/commute_path_road_map.png", width = 20, height = 20, dpi = 300)

tract_od_stats %>% 
  st_drop_geometry() %>% 
  group_by(h_tract) %>% 
  summarize(mean_duration = mean(duration, na.rm = TRUE)) %>% 
  full_join(allegheny_county_tracts, by = c("h_tract" = "GEOID")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = mean_duration), size = .1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Average commute time from a tract",
       fill = "Avg. time") +
  theme_void()

tract_od_stats %>% 
  st_drop_geometry() %>% 
  select(w_tract, duration) %>% 
  arrange(w_tract) %>% 
  group_by(w_tract) %>% 
  summarize(mean_duration = mean(duration, na.rm = TRUE)) %>% 
  full_join(allegheny_county_tracts, by = c("w_tract" = "GEOID")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = mean_duration), size = .1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Average commute time to a tract",
       fill = "Avg. time") +
  theme_void()

allegheny_county_tracts %>% 
  st_drop_geometry() %>% 
  left_join(tract_od_stats %>% 
              select(h_tract, w_tract, duration) %>% 
              pivot_longer(contains("tract")) %>% 
              group_by(name, value) %>% 
              summarize(avg_duration = mean(duration)) %>% 
              ungroup(),
            by = c("GEOID" = "value")) %>% 
  complete(GEOID, name) %>% 
  filter(!is.na(name)) %>% 
  left_join(allegheny_county_tracts) %>%
  mutate(name = case_when(name == "h_tract" ~ "Origin tract",
                          name == "w_tract" ~ "Destination tract"),
         name = as.factor(name) %>% fct_rev()) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = avg_duration), size = .1) +
  facet_wrap(~name, ncol = 1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Average commute duration",
       fill = "Minutes") +
  theme_void()



tract_od_stats %>% 
  select(h_tract, w_tract, duration) %>% 
  st_drop_geometry() %>% 
  pivot_longer(contains("tract")) %>%
  group_by(name, value) %>% 
  summarize(avg_duration = mean(duration)) %>% 
  ungroup() %>% 
  arrange(value) %>% 
  pivot_wider(names_from = name, values_from = avg_duration, values_fill = 0) %>% 
  ggplot(aes(h_tract, w_tract)) +
  geom_jitter(alpha = .5) +
  theme_ipsum()


tract_od_stats %>% 
  st_drop_geometry() %>% 
  select(h_tract, w_tract, duration, commuters) %>%
  complete(h_tract, w_tract, fill = list(duration = NA)) %>% 
  mutate(h_tract = fct_reorder(h_tract, commuters),
         w_tract = fct_reorder(w_tract, commuters)) %>%
  ggplot(aes(h_tract, w_tract, fill = duration)) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "grey90") +
  theme(panel.grid = element_blank())
  
