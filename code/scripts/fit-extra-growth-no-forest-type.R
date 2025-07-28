#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 02_fit-growth-models.R
## Desc: Fit growth models of increasing complexity
## Date created: 2025-01-10


# Packages ----------------------------------------------------------------

library("tidyverse")
library("brms")


# Set options -------------------------------------------------------------

options(mc.cores = 4)
options(brms.file_refit = "on_change")


# Get data ----------------------------------------------------------------

data <-
  readRDS("data/derived/data_cleaned.rds")

well_sampled_trees <-
  data %>%
  group_by(plant_id) %>%
  summarise(records = sum(!is.na(dbase_mean))) %>%
  filter(records > 2)


# Set priors --------------------------------------------------------------

priors3 <- c(
  prior(lognormal(6, 1), nlpar = "A", lb = 0),
  prior(student_t(5, 0, 0.5), nlpar = "k", lb = 0),
  prior(student_t(5, 0, 5), nlpar = "delay"))


# climber cutting ---------------------------------------------------------

data_sample <-
  data %>%
  filter(forest_type == "logged") %>%
  filter(survival == 1) %>%
  filter(plant_id %in% well_sampled_trees$plant_id)

gompertz_cc <-
  bf(dbase_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + climber_cut +
       (0 + climber_cut|genus_species) +
       (1 | plant_id),
     k ~ 0 + climber_cut +
       (0 + climber_cut|genus_species) +
       (1 | plant_id),
     delay ~ 0 + climber_cut +
       (0 + climber_cut|genus_species) +
       (1 | plant_id),
     nl = TRUE)


growth_model_cc <-
  brm(gompertz_cc,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors3,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/grow-extra-noft/growth_model_climber")

print("growth_model_cc fit complete")

rm(growth_model_cc)


# cohort ------------------------------------------------------------------

gompertz_co <-
  bf(dbase_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + cohort +
       (0 + cohort|genus_species) +
       (1 | plant_id),
     k ~ 0 + cohort +
       (0 + cohort|genus_species) +
       (1 | plant_id),
     delay ~ 0 + cohort +
       (0 + cohort|genus_species) +
       (1 | plant_id),
     nl = TRUE)


growth_model_co <-
  brm(gompertz_co,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors3,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/grow-extra-noft/growth_model_cohort")

print("growth_model_co fit complete")

rm(growth_model_co)


# canopy ------------------------------------------------------------------

data_sample <-
  data %>%
  filter(forest_type == "primary") %>%
  filter(survival == 1) %>%
  filter(plant_id %in% well_sampled_trees$plant_id)

gompertz_ca <-
  bf(dbase_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + canopy +
       (0 + canopy|genus_species) +
       (1 | plant_id),
     k ~ 0 + canopy +
       (0 + canopy|genus_species) +
       (1 | plant_id),
     delay ~ 0 + canopy +
       (0 + canopy|genus_species) +
       (1 | plant_id),
     nl = TRUE)


growth_model_ca <-
  brm(gompertz_ca,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors3,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/grow-extra-noft/growth_model_canopy")

print("growth_model_ca fit complete")

rm(growth_model_ca)
