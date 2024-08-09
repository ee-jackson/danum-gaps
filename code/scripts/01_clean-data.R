#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 01_clean-data.R
## Desc: Clean the combined raw data file
## Date created: 2024-08-09


# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("janitor")


# Get data ----------------------------------------------------------------

data_comb <-
  read_csv(here::here("data", "derived", "data_combined.csv"))


# Clean species -----------------------------------------------------------

# Some species are only recorded in one forest type
both_forest_sp <-
  data_comb %>%
  group_by(genus_species) %>%
  summarise(n_forests = n_distinct(forest_type)) %>%
  filter(n_forests == 2) %>%
  pull(genus_species)

data_comb <-
  data_comb %>%
  filter(genus_species %in% both_forest_sp)


# Clean survival ----------------------------------------------------------

lazarus_ids <- data_comb %>%
  group_by(plant_id) %>%
  filter(survival == "1" & lag(survival, order_by = survey_date) == "0") %>%
  pull(plant_id) %>%
  unique()

paste("There are", length(lazarus_ids), "Lazarus trees", sep = " ")

last_alive_dates <-
  data_comb %>%
  group_by(plant_id) %>%
  slice_max(survey_date, with_ties = FALSE) %>%
  select(plant_id, survey_date) %>%
  rename(last_alive = survey_date)

# Assuming that the most recent time a tree was found alive was correct
# and previous records of survival == 0 were incorrect
data_comb <-
  data_comb %>%
  left_join(last_alive_dates,
            by = "plant_id") %>%
  mutate(
    survival = case_when(
      survey_date <= last_alive ~ 1,

      survey_date > last_alive ~ 0
      )
    ) %>%
  select(- last_alive)


# Clean growth ------------------------------------------------------------

data_comb <-
  data_comb %>%
  rowwise() %>%
  mutate(
    dbh_mean = mean(c(dbh1, dbh2), na.rm = TRUE),
    dbase_mean = mean(c(diam1, diam2), na.rm = TRUE)
  ) %>%
  select(- dbh1, - dbh2, - diam1, - diam2)



# Save --------------------------------------------------------------------

data_comb <-
  data_comb %>%
  drop_na(survey_date) %>%
  mutate_if(is.character, as.factor)

saveRDS(data_comb,
        here::here("data", "derived", "data_cleaned.rds"))
