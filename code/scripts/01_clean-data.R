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
  filter(census_id != "intensive_01") %>% # this is a duplicate
  mutate(census_no = case_when(
    census_id == "1" ~ "01",
    census_id == "2" ~ "02",
    census_id == "3" ~ "03",
    census_id == "4" ~ "04",
    census_id == "5" ~ "05",
    census_id == "6" ~ "06",
    census_id == "7" ~ "07",
    census_id == "8" ~ "08",
    census_id == "9" ~ "09",
    census_id == "DanumGaps_Data_2015.xlsx" ~ "16",
    census_id == "DanumGaps_Data_2016.xlsx" ~ "17",
    census_id == "DanumGaps_Data_2017.xlsx" ~ "18",
    census_id == "DanumGaps_Data_2018.xlsx" ~ "19",
    census_id == "DanumGaps_Data_2019.xlsx" ~ "20",
    census_id == "DanumGaps_Data_2023.xlsx" ~ "21",
    census_id == "DanumGaps_Data_2024.xlsx" ~ "22",
    census_id == "full_measurement_01" ~ "01",
    census_id == "intensive_02" ~ "02",
    census_id == "intensive_03" ~ "03",
    census_id == "intensive_04" ~ "04",
    census_id == "intensive_05" ~ "05",
    census_id == "intensive_06" ~ "06",
    census_id == "intensive_07" ~ "07",
    census_id == "full_measurement_02" ~ "08",
    census_id == "climber_01" ~ "09",
    census_id == "climber_02" ~ "10",
    census_id == "climber_03" ~ "11",
    census_id == "climber_04" ~ "12",
    census_id == "climber_05" ~ "13",
    census_id == "climber_06" ~ "14",
    census_id == "climber_07" ~ "15",
    census_id == "climber_08" ~ "16",
    census_id == "climber_09" ~ "17",
    census_id == "climber_10" ~ "18",
    census_id == "climber_11" ~ "19",
    census_id == "intensive_08" ~ "20",
    census_id == "climber_12" ~ "21",
    census_id == "intensive_09" ~ "22",
    census_id == "climber_13" ~ "23",
    census_id == "climber_14" ~ "24",
    census_id == "intensive_10" ~ "25",
    .default = census_id
  ))


# Save --------------------------------------------------------------------

data_comb <-
  data_comb %>%
  distinct() %>%
  filter(!if_all(c(survival, dbh_mean, dbase_mean, height_apex), is.na))

saveRDS(data_comb,
        here::here("data", "derived", "data_cleaned.rds"))
