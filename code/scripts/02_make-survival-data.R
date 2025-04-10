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


# Find interval censored data ---------------------------------------------

# time to first recorded dead
interval_censored <-
  data %>%
  filter(survival == 0) %>%
  group_by(plant_id) %>%
  slice_min(survey_date, with_ties = FALSE) %>%
  ungroup() %>%
  rename(time_to_dead = years) %>%
  select(plant_id, genus_species, plot, forest_type, cohort, time_to_dead) %>%
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

# centre and scale for modelling
data_aggregated <-
  bind_rows(interval_censored, right_censored) %>%
  filter(time_to_last_alive > 0) %>%
  mutate(dbase_mean_sc = scale(dbase_mean),
         dbh_mean_sc = scale(dbh_mean))


# Save --------------------------------------------------------------------

saveRDS(data_aggregated,
        here::here("data", "derived", "data_survival.rds"))
