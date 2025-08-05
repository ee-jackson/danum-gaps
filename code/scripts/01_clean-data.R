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
  read_csv(here::here("data", "derived", "data_combined.csv")) %>%
  filter(canopy != "U") # now removing understory plots


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

# Some plants switch species
sp_switch_plants <-
  data_comb %>%
  group_by(plant_id) %>%
  summarise(n_distinct(genus_species, na.rm = TRUE)) %>%
  filter(`n_distinct(genus_species, na.rm = TRUE)` > 1) %>%
  pull(plant_id)

# Assume most recent species ID is accurate
sp_index <-
  data_comb %>%
  filter(! is.na(genus_species)) %>%
  filter(plant_id %in% sp_switch_plants) %>%
  group_by(plant_id) %>%
  slice_max(survey_date) %>%
  select(plant_id, genus, species, genus_species)

data_comb <-
  data_comb %>%
  rows_update(sp_index, by = "plant_id", unmatched = "ignore")


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
    census_id == "DanumGaps_Data_2025.xlsx" ~ "23",
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
    census_id == "full_measurement_03" ~ "26",
    .default = census_id
  ))


# Clean survey date -------------------------------------------------------

# get median survey dates for each census + plot
all_med_dates_pl <-
  data_comb %>%
  group_by(census_id, census_no, forest_type, plot) %>%
  summarise(median_date = median(survey_date, na.rm = TRUE))

# also get median survey dates for each census
# as plot 13 consistently has no survey date
all_med_dates_cen <-
  data_comb %>%
  group_by(census_id, census_no, forest_type) %>%
  summarise(median_date = median(survey_date, na.rm = TRUE))

# replace NAs with median
data_comb <-
  data_comb %>%
  left_join(all_med_dates_pl,
             by = c("census_id", "census_no",
                    "forest_type", "plot")) %>%
  mutate(survey_date = case_when(
    is.na(survey_date) ~ median_date,
    .default = survey_date
  )) %>%
  select(-median_date) %>%
  left_join(all_med_dates_cen,
            by = c("census_id", "census_no",
                   "forest_type")) %>%
  mutate(survey_date = case_when(
    is.na(survey_date) ~ median_date,
    .default = survey_date
  )) %>%
  select(-median_date)


# Backfill dead plants ----------------------------------------------------

# concatenate plot and line as not complete cases in each census
data_comb <-
  data_comb %>%
  mutate(plot_line = case_when(
    forest_type == "logged" ~ paste(plot, line, sep = "_"),
    forest_type == "primary" ~ paste(plot, canopy, sep = "_"),
    .default = plot
  ))

# get unique plant ids found in each plot and line
plants_in_plots <-
  data_comb %>%
  filter(! str_detect(plant_id, "NA")) %>%
  select(forest_type, plot_line, plant_id) %>%
  distinct() %>%
  group_by(forest_type, plot_line) %>%
  nest(.key = "id_list") %>%
  ungroup()

# key to pass to backfilling function
keys <-
  data_comb %>%
  select(census_id, census_no, plot_line, forest_type) %>%
  distinct() %>%
  left_join(plants_in_plots, by = c("plot_line", "forest_type"))

# function to backfill missing trees as dead
backfill_trees <- function(census_name, census, plot_no, site,
                           tree_ids, data) {
  data %>%
    filter(census_id == census_name,
           census_no == census,
           plot_line == plot_no,
           forest_type == site) %>%
    full_join(tree_ids, by = "plant_id") %>%
    mutate(survival = replace_na(survival, 0)) %>%
    ungroup() %>%
    tidyr::fill(forest_type, plot, census_id, census_no, plot_line)
}

# run function over all keys
data_backfilled <-
  pmap(
    .f = backfill_trees,
    .l = list(
      census_name = keys$census_id,
      census = keys$census_no,
      plot_no = keys$plot_line,
      site = keys$forest_type,
      tree_ids = keys$id_list
    ),
    data = data_comb
  ) %>%
  bind_rows()

# "new" cohort of plants were first surveyed in census "06"
data_backfilled <-
  data_backfilled %>%
  filter(!
           (forest_type == "logged" &
              old_new == "N" &
              census_no %in% c("01", "02", "03", "04", "05") )
  )

# fill missing plant level data for backfilled plants
data_backfilled <-
  data_backfilled %>%
  dplyr::group_by(plant_id) %>%
  arrange(census_no, .by_group = TRUE) %>%
  tidyr::fill(canopy:planting_date, .direction = "updown") %>%
  ungroup()

# add survey dates for for backfilled plants
data_backfilled <-
  data_backfilled %>%
  left_join(all_med_dates_pl,
            by = c("census_id", "census_no",
                   "forest_type", "plot")) %>%
  mutate(survey_date = case_when(
    is.na(survey_date) ~ median_date,
    .default = survey_date
  )) %>%
  select(-median_date) %>%
  left_join(all_med_dates_cen,
            by = c("census_id", "census_no",
                   "forest_type")) %>%
  mutate(survey_date = case_when(
    is.na(survey_date) ~ median_date,
    .default = survey_date
  )) %>%
  select(-median_date)


# Clean Lazarus trees -----------------------------------------------------

lazarus_ids <- data_backfilled %>%
  group_by(plant_id) %>%
  filter(survival == "1" & lag(survival, order_by = survey_date) == "0") %>%
  pull(plant_id) %>%
  unique()

paste("There are", length(lazarus_ids), "Lazarus trees", sep = " ")

last_alive_dates <-
  data_backfilled %>%
  filter(survival == "1") %>%
  group_by(plant_id) %>%
  slice_max(lubridate::ymd(survey_date), with_ties = FALSE) %>%
  select(plant_id, survey_date) %>%
  rename(last_alive = survey_date)

# Assuming that the most recent time a tree was found alive was correct
# and previous records of survival == 0 were incorrect
data_backfilled <-
  data_backfilled %>%
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
  bind_rows(filter(data_backfilled, ! plant_id %in% lazarus_ids))


# Clean growth ------------------------------------------------------------

data_backfilled <-
  data_backfilled %>%
  rowwise() %>%
  mutate(
    dbh_mean = mean(c(dbh1, dbh2), na.rm = TRUE),
    dbase_mean = mean(c(diam1, diam2), na.rm = TRUE)
  ) %>%
  ungroup()


# Clean cohort ------------------------------------------------------------

data_backfilled <-
  data_backfilled %>%
  mutate(cohort = case_when(
         forest_type == "primary" ~ 1,
         forest_type == "logged" & old_new == "O" ~ 1,
         forest_type == "logged" & old_new == "N" ~ 2
  ))


# Add time variable -------------------------------------------------------

data_backfilled <-
  data_backfilled %>%
  group_by(plant_id) %>%
  slice_min(survey_date, with_ties = FALSE) %>%
  select(plant_id, survey_date) %>%
  rename(first_survey = survey_date) %>%
  ungroup() %>%
  right_join(data_backfilled)

data_backfilled <-
  data_backfilled %>%
  rowwise() %>%
  mutate(
    days =
      survey_date - first_survey) %>%
  ungroup() %>%
  mutate(years = as.numeric(days, units = "weeks")/52.25,
         days_num = as.numeric(days))


# Add col indicating whether climber cut ----------------------------------

data_backfilled <-
  data_backfilled %>%
  mutate(climber_cut = case_when(
    forest_type == "logged" & plot == "05" |
      forest_type == "logged" & plot == "11" |
      forest_type == "logged" & plot == "14" ~ 1,
    .default  = 0
  ))


# Predict missing basal diameter ------------------------------------------


n_missing_base <-
  data_backfilled %>%
  filter(is.na(dbase_mean) & !is.na(dbh_mean)) %>%
  nrow()

paste(
  round(n_missing_base /
          nrow(filter(data_backfilled, survival == 1))
        * 100, digits = 2),
  "% trees are missing basal diameter",
  sep = " "
  )

# Taper model 1 from Cushman et al. 2014, doi: 10.1111/2041-210X.12187
get_basal <- function(dbh, pom, b1) {
  dbh /
    exp(b1 * (pom - 1))
}

# Choose b1 parameter which minimises RMSE in basal diameter for our data
get_rmse <- function(df, b1){

  # make predictions for basal diameter
  predictions <-
    with(df, get_basal(dbh = dbh_mean, pom = 1.3, b1))

  # get model errors
  errors <-
    with(df, dbase_mean - predictions)

  # return the rmse
  return( sqrt( sum(errors^2, na.rm = TRUE) / (length(errors)) ) )

}

optimiser_results <-
  optim(
    method = "Brent",
    par = c(0),
    lower = -5,
    upper = 5,
    fn = function(x) {
      get_rmse(df = data_backfilled, x[1])
    }
  )


data_backfilled <-
  data_backfilled %>%
  mutate(dbase_mean = case_when(
    is.na(dbase_mean) & !is.na(dbh_mean) ~
      get_basal(dbh = dbh_mean, pom = 1.3, b1 = optimiser_results$par),
    .default = dbase_mean
  ))


# Save --------------------------------------------------------------------

data_backfilled <-
  data_backfilled %>%
  select(plant_id, forest_type, climber_cut,
         genus, species, genus_species,
         plot, line, position, cohort, plant_no,
         first_survey, planting_date, census_no, census_id, survey_date,
         days, years, days_num,
         survival, height_apex, dbh_mean, dbase_mean) %>%
  distinct() %>%
  filter(! if_all(c(survival, dbh_mean, dbase_mean, height_apex), is.na)) %>%
  filter(! str_detect(plant_id, "NA")) %>%
  mutate(across(c(forest_type, plant_id, plot,
                  climber_cut, cohort, genus_species,
                  line, position, plant_no, census_no), as.factor))

saveRDS(data_backfilled,
        here::here("data", "derived", "data_cleaned.rds"))
