#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 00_combine-raw-data.R
## Desc: Combine data from 3 different sources into one csv file
## Date created: 2024-08-06


# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("janitor")


# Get primary forest data -------------------------------------------------

file_names <-
  as.list(dir(path = here::here("data", "raw", "dv"),
              pattern = "DanumGaps_Data_*",
              full.names = TRUE))

data_list <-
  lapply(X = file_names,
         FUN = readxl::read_excel,
         range = readxl::cell_cols("A:K"),
         na = c("", "NA"),
         col_types = c("date",
                       "text",
                       "text",
                       "text",
                       "text",
                       "numeric",
                       "numeric",
                       "numeric",
                       "numeric",
                       "numeric",
                       "text") )

names(data_list) <-
  lapply(file_names, basename)

data_dv_sps <-
  read_csv(
    here::here("data", "raw", "dv", "species_nos.csv"),
    col_types = "fcc"
  )

# Some older primary forest data is in this file and formatted differently
data_dv_older <-
  readxl::read_excel(
    here::here("data", "raw", "dv", "AllDataClean2018_DanumGaps.xlsx"),
    na = c("", "NA"),
    range = readxl::cell_cols("A:AE"),
    col_types = c("text","date","numeric","numeric",
                  "numeric","text","text","numeric",
                  "text","numeric","text","numeric",
                  "text","text","text","numeric",
                  "numeric","numeric","numeric","numeric",
                  "numeric","numeric","numeric","numeric",
                  "numeric","numeric","numeric","numeric",
                  "numeric","numeric","text") ) %>%
  select(Census, Survey.date, Year, Team, Canopy, Block, Subplot, Plant.no,
         Survival, Diam1, Diam2, DBH.1, DBH.2, Comments, Height.apex) %>%
  janitor::clean_names()

# Some data is replicated in DanumGaps_Data_* files
data_dv_pre2015 <-
  data_dv_older %>%
  filter(year < 2015 |
           year == 2015 & team == "Malua") %>%
  select(- year, - team) %>%
  rename(census_id = census) %>%
  mutate(census_id = as.factor(census_id))


# Combine primary forest data sources -------------------------------------

data_dv <-
  bind_rows(data_list, .id = 'census_id') %>%
  mutate(census_id = as.factor(census_id)) %>%
  clean_names() %>%
  mutate(height_apex = NA) %>%
  bind_rows(data_dv_pre2015) %>%
  rename(plot = block,
         dbh1 = dbh_1,
         dbh2 = dbh_2) %>%

  # Only gaps and plots without drought treatment
  filter(canopy == "G" & subplot == "A") %>%
  select(- canopy, - subplot) %>%

  # Repairing individual cases of missing plant number
  mutate(plant_no = case_when(is.na(plant_no) & plot == 1 ~ "8",
                              plant_no == "6.8" & plot == 6 ~ "8",
                              .default = plant_no) ) %>%

  # Plant nos are unique to species
  mutate(plant_id =
    str_extract(plant_no, pattern = "\\d+")
  ) %>%
  left_join(data_dv_sps, by = c("plant_id" = "plant_no")) %>%

  # Create unique ID per individual
  mutate(plot = as.numeric(plot)) %>%
  mutate(plot = formatC(plot,
                        width = 2,
                        format = "d",
                        flag = "0"),
         survey_date = ymd(survey_date)) %>%
  mutate(plant_id = paste(plot, plant_no, sep = "_"),
         genus_species = paste(genus, species, sep = "_")) %>%

  # Making cols match the primary forest data
  mutate(line = NA, position = NA, old_new = NA,
         planting_date = NA, height_apex = NA, forest_type = "primary",
         census_id = as.factor(census_id)) %>%
  select(forest_type, plant_id, plot, line, position, old_new, plant_no,
         genus, species, genus_species,
         planting_date, census_id, survey_date,
         survival, height_apex, diam1, diam2, dbh1, dbh2)

rm(data_list)


# Get secondary forest data -----------------------------------------------

data_sbe <-
  read_csv(
    here::here("data", "raw", "sbe", "SBE_compiled_dataFull.csv")
  ) %>%
  clean_names() %>%
  rename(old_new = o_n,
         planting_date = plantingdate,
         survey_date = surveydate,
         height_apex = heightapex) %>%

  # Only intensively censused plots
  filter(pl %in% c(3, 5, 8, 11, 14, 17)) %>%

  mutate(plot = ifelse(is.na(pl), NA,
                       formatC(pl,
                               width = 2,
                               format = "d",
                               flag = "0")),
         line = ifelse(is.na(li), NA,
                       formatC(li,
                               width = 2,
                               format = "d",
                               flag = "0")),
         position = ifelse(is.na(po), NA,
                     formatC(po,
                             width = 3,
                             format = "d",
                             flag = "0")),
         sample = ifelse(is.na(sample), NA,
                       formatC(sample,
                               width = 2,
                               format = "d",
                               flag = "0"))
         ) %>%

  mutate(
    planting_date = dmy(planting_date),
    survey_date = dmy(survey_date),
    census_id = as.factor(paste(data_origin, sample, sep = "_")),
    plant_id = paste(plot, line, position, old_new, sep = "_"),
    survival = case_when(
      survival == "yes" ~ 1,
      survival == "no" ~ 0,
      .default = NA
    )
  ) %>%
  mutate(plant_id = case_when(
    is.na(position) ~ paste(plant_id, species, sep = "_"),
    .default = plant_id
  )) %>%
  # Making cols match the primary forest data
  mutate(plant_no = NA, forest_type = "secondary") %>%
  select(forest_type, plant_id, plot, line, position, old_new, plant_no,
         genus, species, genus_species,
         planting_date, census_id, survey_date,
         survival, height_apex, diam1, diam2, dbh1, dbh2)


# Combine secondary and primary forest data -------------------------------

data_comb <-
  bind_rows(data_dv, data_sbe)

write_csv(data_comb, here::here("data", "derived", "data_combined.csv"))
