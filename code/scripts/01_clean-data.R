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
  filter(survival == "1") %>%
  group_by(plant_id) %>%
  slice_max(lubridate::ymd(survey_date), with_ties = FALSE) %>%
  select(plant_id, survey_date) %>%
  rename(last_alive = survey_date)

# Assuming that the most recent time a tree was found alive was correct
# and previous records of survival == 0 were incorrect
data_comb <-
  data_comb %>%
  filter(plant_id %in% lazarus_ids) %>%
  left_join(last_alive_dates,
            by = "plant_id") %>%
  mutate(
    survival = case_when(
      survey_date <= last_alive ~ 1,

      survey_date > last_alive ~ 0,

      is.na(survey_date) ~ NA
      )
    ) %>%
  select(- last_alive) %>%
  bind_rows(filter(data_comb, ! plant_id %in% lazarus_ids))


# Clean growth ------------------------------------------------------------

data_comb <-
  data_comb %>%
  rowwise() %>%
  mutate(
    dbh_mean = mean(c(dbh1, dbh2), na.rm = TRUE),
    dbase_mean = mean(c(diam1, diam2), na.rm = TRUE)
  ) %>%
  select(- dbh1, - dbh2, - diam1, - diam2)


# Clean census ------------------------------------------------------------

data_comb <-
  data_comb %>%
  mutate(census_no = case_when(
    census_no == "DanumGaps_Data_2015.xlsx" ~ "16",
    census_no == "DanumGaps_Data_2016.xlsx" ~ "17",
    census_no == "DanumGaps_Data_2017.xlsx" ~ "18",
    census_no == "DanumGaps_Data_2018.xlsx" ~ "19",
    census_no == "DanumGaps_Data_2019.xlsx" ~ "20",
    census_no == "DanumGaps_Data_2023.xlsx" ~ "21",
    census_no == "DanumGaps_Data_2024.xlsx" ~ "22",
    .default = census_no
  ))

# Save --------------------------------------------------------------------

data_comb <-
  data_comb %>%
  filter(!if_all(c(survival, dbh_mean, dbase_mean, height_apex), is.na)) %>%
  distinct() %>%
  mutate_if(is.character, as.factor)

saveRDS(data_comb,
        here::here("data", "derived", "data_cleaned.rds"))
