#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 02_make-survival-data.R
## Desc: Use data_cleaned.rds to create aggregated survival data
## Date created: 2025-04-10


# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")


# Get data ----------------------------------------------------------------

data <-
  readRDS(here::here("data", "derived", "data_cleaned.rds"))


# Remove left censored data -----------------------------------------------

# seedlings which were never recorded as alive
left_censored <-
  data %>%
  group_by(plant_id) %>%
  summarise(n = sum(survival)) %>%
  filter(n == 0) %>%
  select(plant_id)

data <-
  data %>%
  filter(! plant_id %in% left_censored$plant_id)

# Find interval censored data ---------------------------------------------

# time to first recorded dead
interval_censored <-
  data %>%
  filter(survival == 0) %>%
  group_by(plant_id) %>%
  slice_min(survey_date, with_ties = FALSE) %>%
  ungroup() %>%
  rename(time_to_dead = years) %>%
  select(plant_id, genus_species, plot,
         forest_type, cohort, time_to_dead) %>%
  mutate(censor = "interval")

# time to last recorded alive
interval_censored <-
  data %>%
  filter(plant_id %in% interval_censored$plant_id) %>%
  filter(survival == 1) %>%
  group_by(plant_id) %>%
  slice_max(survey_date, with_ties = FALSE) %>%
  ungroup() %>%
  rename(time_to_last_alive = years) %>%
  select(plant_id, time_to_last_alive, dbh_mean, dbase_mean) %>%
  right_join(interval_censored)


# Find right censored data ------------------------------------------------

# trees never recorded dead
right_censored <-
  data %>%
  filter(!plant_id %in% interval_censored$plant_id) %>%
  group_by(plant_id) %>%
  slice_max(survey_date, with_ties = FALSE) %>%
  ungroup() %>%
  rename(time_to_last_alive = years) %>%
  select(plant_id, genus_species, plot, forest_type,
         cohort, time_to_last_alive, dbh_mean, dbase_mean) %>%
  mutate(censor = "right")


# Combine right and interval censored data --------------------------------

# scale (but don't center) for modelling
# day zero needs to be slightly > 0 for Weibull model
data_aggregated <-
  bind_rows(interval_censored, right_censored) %>%
  mutate(time_to_last_alive = ifelse(time_to_last_alive == 0,
                                     0.0000001, time_to_last_alive)) %>%
  mutate(dbase_mean_sc = scale(dbase_mean, center = FALSE, scale = TRUE))


# Save --------------------------------------------------------------------

saveRDS(data_aggregated,
        here::here("data", "derived", "data_survival.rds"))
