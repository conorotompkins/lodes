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

allegheny_county_tracts <- tracts(state = "PA", county = "Allegheny", cb = TRUE) %>% 
  select(GEOID)

tract_od_stats <- read_csv("data/tract_od_stats.csv",
                           col_types = cols(
                             h_tract = col_character(),
                             home_address = col_character(),
                             w_tract = col_character(),
                             work_address = col_character(),
                             duration = col_double(),
                             distance = col_double(),
                             steps = col_double(),
                             commuters = col_double()
                           )) %>%
  #uncount(commuters) %>% 
  select(h_tract, w_tract, distance, steps, commuters, duration) %>% 
  drop_na(duration) %>% 
  arrange(desc(duration)) %>% 
  #mutate(h_tract = fct_reorder(h_tract, commuters, .desc = FALSE),
  #       w_tract = fct_reorder(w_tract, commuters, .desc = FALSE)) %>% 
  mutate(od_id = str_c("h_tract: ", h_tract, ", ", "w_tract: ", w_tract, sep = ""))

glimpse(tract_od_stats)

tract_od_stats %>% 
  count(w_tract, sort = TRUE)

tract_od_stats %>% 
  ggplot(aes(duration)) +
  geom_histogram()

tract_od_stats %>% 
  ggplot(aes(distance, duration)) +
  geom_jitter(alpha = .3) +
  geom_smooth()

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
  scale_fill_viridis_c() +
  theme_void()

tract_od_stats %>% 
  filter(w_tract != "42003020100")

#recipe
model_recipe <- recipe(duration ~ ., 
                       data = tract_od_stats) %>% 
  update_role(od_id, new_role = "id") %>%
  step_rm(h_tract, w_tract, commuters) %>% 
  step_normalize(all_predictors())

model_recipe %>% 
  prep() %>% 
  summary()

model_recipe_prep <- model_recipe %>% 
  prep()

tract_vfold <- vfold_cv(tract_od_stats, times = 10)

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
  
#variable importance
var_imp <- lm_workflow %>% 
  fit(tract_od_stats) %>% 
  pull_workflow_fit() %>% 
  vip::vi()

var_imp %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>% 
  ggplot(aes(Importance, Variable)) +
  geom_point()

tract_od_fit <- lm_workflow %>% 
  fit(tract_od_stats) %>% 
  predict(tract_od_stats) %>% 
  bind_cols(tract_od_stats) %>% 
  select(h_tract, w_tract, distance, steps, duration, .pred)

tract_od_fit %>% 
  ggplot(aes(duration, .pred)) +
  geom_point(alpha = .3) +
  geom_abline(lty = 2, color = "red") +
  coord_equal()

tract_od_fit %>% 
  mutate(.resid = .pred - duration) %>% 
  ggplot(aes(duration, .resid)) +
  geom_point(alpha = .3)

tract_od_fit %>% 
  mutate(.resid = .pred - duration) %>% 
  group_by(h_tract) %>% 
  summarize(avg_resid = mean(.resid)) %>% 
  ungroup() %>% 
  arrange(desc(avg_resid)) %>% 
  full_join(allegheny_county_tracts, by = c("h_tract" = "GEOID")) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = avg_resid), size = .1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Origin")

tract_od_fit %>% 
  mutate(.resid = .pred - duration) %>% 
  group_by(w_tract) %>% 
  summarize(avg_resid = mean(.resid)) %>% 
  ungroup() %>% 
  arrange(desc(avg_resid)) %>% 
  full_join(allegheny_county_tracts, by = c("w_tract" = "GEOID")) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = avg_resid), size = .1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(title = "Destination")


allegheny_county_tracts %>% 
  st_drop_geometry() %>% 
  left_join(tract_od_fit %>% 
              mutate(.resid = .pred - duration) %>% 
              select(h_tract, w_tract, .resid) %>% 
              pivot_longer(contains("tract")) %>% 
              group_by(name, value) %>% 
              summarize(avg_resid = mean(.resid)) %>% 
              ungroup(),
            by = c("GEOID" = "value")) %>% 
  complete(GEOID, name) %>% 
  filter(!is.na(name)) %>% 
  left_join(allegheny_county_tracts) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = avg_resid), size = NA) +
  facet_wrap(~name) +
  scale_fill_viridis_c(na.value = "grey90")


tract_od_fit %>% 
  mutate(.resid = .pred - duration) %>% 
  select(contains("tract"), .resid) %>% 
  pivot_longer(cols = contains("tract")) %>% 
  group_by(name, value) %>% 
  summarize(avg_resid = mean(.resid)) %>% 
  ggplot(aes(avg_resid, fill = name, color = name)) +
  geom_density(alpha = .6)


#make model
model_df <- tract_od_stats %>% 
  select(-commuters) %>% 
  nest(od_data = everything()) %>% 
  mutate(model = map(od_data, ~lm(duration ~ distance + steps + h_tract + w_tract - 1, data = .)),
         fit = map(model, augment),
         coeff = map(model, tidy),
         glance = map(model, glance),
         summary = map(model, summary))

#summary
model_df %>% 
  select(summary) %>% 
  pull(summary)

#coeff
model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>%
  mutate(term = fct_reorder(term, estimate)) %>% 
  arrange(desc(term)) %>% 
  View()

model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>% 
  filter(str_detect(term, "42003020100"))

model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>% 
  filter(str_detect(term, "h_tract"))

tract_od_stats %>% 
  drop_na(duration) %>% 
  distinct(h_tract)

#check for tracts that are in model_df but not in tract_od_stats or allegheny_county_tracts
model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>% 
  filter(str_detect(term, "h_tract")) %>% 
  mutate(term = str_remove(term, "^h_tract")) %>% 
  anti_join(tract_od_stats, by = c("term" = "h_tract"))

model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>% 
  filter(str_detect(term, "h_tract")) %>% 
  mutate(term = str_remove(term, "^h_tract")) %>% 
  anti_join(allegheny_county_tracts, by = c("term" = "GEOID"))

model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>% 
  filter(str_detect(term, "w_tract")) %>% 
  mutate(term = str_remove(term, "^w_tract")) %>% 
  anti_join(tract_od_stats, by = c("term" = "w_tract"))

model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>% 
  filter(str_detect(term, "w_tract")) %>% 
  mutate(term = str_remove(term, "^w_tract")) %>% 
  anti_join(allegheny_county_tracts, by = c("term" = "GEOID"))



term_df <- model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>%
  mutate(term_type = case_when(str_detect(term, "h_tract") ~ "h_tract",
                               str_detect(term, "w_tract") ~ "w_tract",
                               TRUE ~ "other")) %>% 
  mutate(term_fct = tidytext::reorder_within(term, estimate, term_type))

term_df %>% 
  filter(term_type == "h_tract" | term_type == "w_tract") %>% 
  ggplot(aes(estimate, fill = term_type, color = term_type)) +
  geom_density(alpha = .5)

model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>% 
  ggplot(aes(estimate, p.value)) +
  geom_point(alpha = .3)

model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>%
  filter(str_detect(term, "h_tract")) %>% 
  mutate(term = str_remove(term, "^h_tract")) %>% 
  select(term, estimate) %>% 
  full_join(allegheny_county_tracts, by = c("term" = "GEOID")) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = estimate), size = .1) +
  scale_fill_viridis_c() +
  labs(title = "Origins")

model_df %>% 
  select(coeff) %>% 
  unnest(coeff) %>%
  filter(str_detect(term, "w_tract")) %>% 
  mutate(term = str_remove(term, "^w_tract")) %>% 
  #filter(str_detect(term, "42003020100"))
  select(term, estimate) %>% 
  full_join(allegheny_county_tracts, by = c("term" = "GEOID")) %>% 
  st_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = estimate), size = .1) +
  scale_fill_viridis_c() +
  labs(title = "Destinations")

allegheny_county_tracts %>% 
  st_drop_geometry() %>% 
  left_join(term_df %>% 
              select(term, term_type, estimate) %>% 
              filter(term_type %in% c("h_tract", "w_tract")) %>% 
              mutate(term = str_remove_all(term, "h_tract|w_tract")),
            by = c("GEOID" = "term")) %>% 
  complete(GEOID, term_type) %>% 
  filter(!is.na(term_type)) %>% 
  left_join(allegheny_county_tracts) %>% 
  st_sf() %>% 
  ggplot() +
  #geom_sf(data = allegheny_county_tracts, size = 0) +
  geom_sf(aes(fill = estimate), size = .1) +
  scale_fill_viridis_c(na.value = "grey50") +
  facet_wrap(~term_type) +
  theme_void()

#fit
model_df %>% 
  select(fit) %>% 
  unnest(fit) %>%
  ggplot(aes(duration, .fitted)) +
  geom_abline(lty = 2) +
  geom_jitter(alpha = .3) +
  coord_equal()

model_df %>% 
  select(fit) %>% 
  unnest(fit) %>% 
  select(duration, .fitted) %>% 
  pivot_longer(cols = everything(), names_to = "type", values_to = "value") %>% 
  ggplot(aes(value, fill = type, color = type)) +
  geom_density(alpha = .5)
