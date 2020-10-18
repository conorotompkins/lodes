library(tidyverse)
library(tidymodels)
library(tigris)
library(sf)
library(tidytext)
library(hrbrthemes)

options(tigris_use_cache = TRUE,
        scipen = 999,
        digits = 4)

set.seed(1234)

theme_set(theme_ipsum())

ac_water <- area_water("PA", "Allegheny", class = "sf")

main_rivers <- ac_water %>% 
  group_by(FULLNAME) %>% 
  summarize(AWATER = sum(AWATER)) %>% 
  arrange(desc(AWATER)) %>% 
  slice(1:4)

allegheny_county_tracts <- tracts(state = "PA", county = "Allegheny", cb = TRUE) %>% 
  select(GEOID)

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

ac_water <- area_water("PA", "Allegheny", class = "sf")

allegheny_county_tracts <- st_erase(allegheny_county_tracts, ac_water)

glimpse(tract_od_stats)

tract_od_directions <- st_read("data/tract_od_total_shape_combined/tract_od_total_shape_combined.shp") %>% 
  rename(home_address = hm_ddrs,
         work_address = wrk_ddr,
         distance = distanc,
         duration = duratin,
         directions = dirctns,
         instructions = instrct,
         commuters = commtrs,
         source_file = sorc_fl) %>% 
  st_transform(crs = "NAD83")

tract_od_stats <- tract_od_directions %>% 
  unnest(directions) %>%
  mutate(od_id = str_c("h_tract: ", h_tract, ", ", "w_tract: ", w_tract, sep = "")) %>% 
  group_by(h_tract, home_address, w_tract, work_address, od_id) %>%
  summarize(duration = sum(duration),
            distance = sum(distance),
            steps = n(),
            commuters = unique(commuters)) %>% 
  ungroup()

tract_od_stats <- tract_od_stats %>% 
  mutate(intersects_ohio = st_intersects(., main_rivers %>% 
                                           filter(FULLNAME == "Ohio Riv")) %>% as.logical(),
         intersects_allegheny = st_intersects(., main_rivers %>% 
                                                filter(FULLNAME == "Allegheny Riv")) %>% as.logical(),
         intersects_monongahela = st_intersects(., main_rivers %>% 
                                                  filter(FULLNAME == "Monongahela Riv")) %>% as.logical(),
         intersects_youghiogheny = st_intersects(., main_rivers %>% 
                                                   filter(FULLNAME == "Youghiogheny Riv")) %>% as.logical()) %>% 
  replace_na(list(intersects_ohio = FALSE,
                  intersects_allegheny = FALSE,
                  intersects_monongahela = FALSE,
                  intersects_youghiogheny = FALSE)) %>% 
  st_drop_geometry()


tract_od_stats %>% 
  count(w_tract, sort = TRUE)

tract_od_stats %>% 
  ggplot(aes(duration)) +
  geom_histogram()

tract_od_stats %>% 
  ggplot(aes(distance, duration)) +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  geom_abline(lty = 2, color = "red") +
  coord_equal()

tract_od_stats %>% 
  ggplot(aes(steps, duration)) +
  geom_jitter(alpha = .3)

tract_od_stats %>% 
  group_by(h_tract) %>% 
  summarize(duration = mean(duration, na.rm = TRUE)) %>% 
  ungroup() %>% 
  full_join(allegheny_county_tracts, by = c("h_tract" = "GEOID")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = duration), size = .1) +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_void()

tract_od_stats %>% 
  filter(w_tract != "42003020100")

#recipe
model_recipe <- recipe(duration ~ ., 
                       data = tract_od_stats) %>% 
  update_role(od_id, new_role = "id") %>%
  step_rm(h_tract, home_address, w_tract, work_address, commuters, intersects_youghiogheny) %>% 
  step_normalize(distance, steps)

model_recipe %>% 
  prep() %>% 
  summary()

model_recipe_prep <- model_recipe %>% 
  prep()

tract_vfold <- vfold_cv(tract_od_stats, v = 100)

tract_vfold %>% 
  pull(splits) %>% 
  .[[1]] %>% 
  .$data

#model specification
lm_model <- linear_reg(mode = "regression") %>% 
  set_engine("lm")

#logistic regression
lm_workflow <- workflow() %>% 
  add_recipe(model_recipe) %>% 
  add_model(lm_model)

lm_res <- lm_workflow %>% 
  fit_resamples(tract_vfold) %>% 
  mutate(model = "lm")

lm_res %>% 
  unnest(.notes) %>% 
  pull(.notes)

lm_res %>% 
  collect_metrics()

lm_res %>% 
  select(.metrics) %>% 
  unnest(.metrics) %>% 
  filter(.metric == "rsq")

lm_res %>% 
  select(.metrics) %>% 
  unnest(.metrics) %>% 
  arrange(.metric) %>% 
  ggplot(aes(.estimate, fill = .metric)) +
  geom_density() +
  facet_wrap(~.metric, scales = "free", ncol = 1)
  
#variable importance
lm_workflow %>% 
  fit(tract_od_stats) %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(estimate, term)) +
  geom_col(fill = "grey", color = "black")
  
#final model
tract_od_fit <- lm_workflow %>% 
  fit(tract_od_stats) %>% 
  predict(tract_od_stats) %>% 
  bind_cols(tract_od_stats) %>% 
  select(h_tract, w_tract, distance, steps, duration, .pred)

tract_od_fit %>% 
  ggplot(aes(duration, .pred)) +
  geom_point(alpha = .3) +
  geom_abline(lty = 2, color = "red") +
  coord_equal() +
  labs(x = "Duration",
       y = "Predicted duration")

tract_od_fit %>% 
  mutate(.resid = duration - .pred) %>% 
  ggplot(aes(duration, .resid)) +
  geom_point(alpha = .3)

tract_od_fit %>% 
  mutate(.resid = duration - .pred) %>% 
  select(contains("tract"), .resid) %>% 
  pivot_longer(cols = contains("tract")) %>% 
  group_by(name, value) %>% 
  summarize(avg_resid = mean(.resid)) %>% 
  ggplot(aes(avg_resid, fill = name, color = name)) +
  geom_density(alpha = .6) +
  labs(title = "Destination tract mostly decreases duration")

tract_od_fit %>% 
  mutate(.resid = duration - .pred) %>% 
  group_by(h_tract) %>% 
  summarize(avg_resid = mean(.resid)) %>% 
  ungroup() %>% 
  arrange(desc(avg_resid)) %>% 
  full_join(allegheny_county_tracts, by = c("h_tract" = "GEOID")) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = avg_resid), size = .1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Commute duration above exected: Origin",
       fill = "Minutes") +
  theme_void()

tract_od_fit %>% 
  mutate(.resid = duration - .pred) %>% 
  group_by(w_tract) %>% 
  summarize(avg_resid = mean(.resid)) %>% 
  ungroup() %>% 
  arrange(desc(avg_resid)) %>% 
  full_join(allegheny_county_tracts, by = c("w_tract" = "GEOID")) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = avg_resid), size = .1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Commute duration above exected: Desitination",
       fill = "Minutes") +
  theme_void()


allegheny_county_tracts %>% 
  st_drop_geometry() %>% 
  left_join(tract_od_fit %>% 
              mutate(.resid = duration - .pred) %>% 
              select(h_tract, w_tract, .resid) %>% 
              pivot_longer(contains("tract")) %>% 
              group_by(name, value) %>% 
              summarize(avg_resid = mean(.resid)) %>% 
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
  geom_sf(aes(fill = avg_resid), size = .1) +
  facet_wrap(~name, ncol = 1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Commute duration above/below prediction",
       fill = "Minutes") +
  theme_void()
