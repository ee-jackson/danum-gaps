#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 00_combine-raw-data.R
## Desc: Combines raw data from 3 different sources into one csv file
## Date created: 2024-08-06


# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("janitor")
library("zen4R")


# Get primary forest data -------------------------------------------------

# download Danum data from Zenodo
zen4R::download_zenodo(
  doi = "https://doi.org/10.5281/zenodo.10701332",
  files = "DataGrowthSurvivalClean_v2.txt",
  path = here::here(
    "data",
    "raw",
    "dv"
  )
)

data_dv_sps <-
  read_csv(
    here::here("data", "raw", "dv", "species_nos.csv"),
    col_types = "fcc"
  )

# Clean up and reformat the Danum data
data_dv <-
  read_tsv(
    here::here("data", "raw", "DataGrowthSurvivalClean_v2.txt")
  ) %>%
  mutate(Survey.date = as.Date(Survey.date, "%d/%m/%Y")) %>%
  clean_names() %>%
  # Only plots without drought treatment
  filter(subplot == "A") %>%
  mutate_at(c("canopy", "subplot"), toupper) %>%
  select(- plot) %>%
  rename(plot = block,
         dbh1 = dbh_1,
         dbh2 = dbh_2,
         survival = surv,
         census_id = census) %>%
  left_join(data_dv_sps) %>%
  mutate(plant_no_let =
           str_extract(plant_id, pattern = "[A-Z]+")) %>%
  unite(plant_no, c(plant_no, plant_no_let), sep = "", na.rm = TRUE) %>%
  # Create unique ID per individual
  mutate(plot = as.numeric(plot)) %>%
  mutate(plot = formatC(plot,
                        width = 2,
                        format = "d",
                        flag = "0"),
         survey_date = ymd(survey_date)) %>%
  mutate(plant_id = paste(plot, plant_no, canopy, sep = "_"),
         genus_species = paste(genus, species, sep = "_")) %>%
  # Making cols match the primary forest data
  mutate(line = NA, position = NA, old_new = NA,
         planting_date = NA, height_apex = NA, forest_type = "primary",
         census_id = as.factor(census_id)) %>%
  select(forest_type, plant_id, plot, canopy, line, position, old_new, plant_no,
         genus, species, genus_species,
         planting_date, census_id, survey_date,
         survival, height_apex, diam1, diam2, dbh1, dbh2)


# Get logged forest data -----------------------------------------------

# download SBE data from Zenodo
zen4R::download_zenodo(
  doi = "https://doi.org/10.5281/zenodo.10815814",
  path = here::here(
    "data",
    "raw",
    "sbe"
  )
)

# Clean up and reformat the SBE data
data_sbe <-
  read_csv(
    here::here("data", "raw", "sbe", "SBE_compiled_data_2002-2024.csv")
  ) %>%
  clean_names() %>%
  rename(old_new = o_n,
         planting_date = plantingdate,
         survey_date = surveydate,
         height_apex = heightapex) %>%
  mutate(across(c(old_new, survival), str_trim)) %>%

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
  mutate(plant_no = NA, forest_type = "logged", canopy = "C") %>%
  select(forest_type, plant_id, plot, canopy, line, position, old_new, plant_no,
         genus, species, genus_species,
         planting_date, census_id, survey_date,
         survival, height_apex, diam1, diam2, dbh1, dbh2)


# Combine logged and primary forest data -------------------------------

data_comb <-
  bind_rows(data_dv, data_sbe) %>%
  distinct() %>%
  filter(!if_all(c(survival,
                   diam1,
                   diam2,
                   dbh1,
                   dbh2,
                   height_apex), is.na))

write_csv(data_comb, here::here("data", "derived", "data_combined.csv"))
