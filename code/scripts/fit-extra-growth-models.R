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

data_sample <-
  data %>%
  filter(survival == 1) %>%
  filter(plant_id %in% well_sampled_trees$plant_id) %>%
  filter(canopy != "U")


# Set priors --------------------------------------------------------------

priors3 <- c(
  prior(lognormal(6, 1), nlpar = "A", lb = 0),
  prior(student_t(5, 0, 0.5), nlpar = "k", lb = 0),
  prior(student_t(5, 0, 5), nlpar = "delay"))


# climber cutting ---------------------------------------------------------

gompertz_cc <-
  bf(dbase_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type + climber_cut +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     k ~ 0 + forest_type + climber_cut +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     delay ~ 0 + forest_type + climber_cut +
       (0 + forest_type|genus_species) +
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
      file = "output/models/grow-extra/growth_model_base_p3_cc")

add_criterion(x = growth_model_cc,
              newdata = drop_na(data = data_sample,
                                dbase_mean),
              criterion = "loo")

print("growth_model_cc fit complete")

rm(growth_model_cc)


# Plot --------------------------------------------------------------------

gompertz_pl <-
  bf(dbase_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | forest_type:plot) +
       (1 | plant_id),
     k ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | forest_type:plot) +
       (1 | plant_id),
     delay ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | forest_type:plot) +
       (1 | plant_id),
     nl = TRUE)


growth_model_pl <-
  brm(gompertz_pl,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors3,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/grow-extra/growth_model_base_p3_pl")

add_criterion(x = growth_model_pl,
              newdata = drop_na(data = data_sample,
                                dbase_mean),
              criterion = "loo")

print("growth_model_pl fit complete")

rm(growth_model_pl)

# Cohort ------------------------------------------------------------------

gompertz_co <-
  bf(dbase_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type + cohort +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     k ~ 0 + forest_type + cohort +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     delay ~ 0 + forest_type + cohort +
       (0 + forest_type|genus_species) +
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
      file = "output/models/grow-extra/growth_model_base_p3_co")

add_criterion(x = growth_model_co,
              newdata = drop_na(data = data_sample,
                                dbase_mean),
              criterion = "loo")

print("growth_model_co fit complete")

rm(growth_model_co)


# canopy ------------------------------------------------------------------

data_sample_ca <-
  data %>%
  filter(survival == 1) %>%
  filter(plant_id %in% well_sampled_trees$plant_id)

gompertz_ca <-
  bf(dbase_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type + canopy +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     k ~ 0 + forest_type + canopy +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     delay ~ 0 + forest_type + canopy +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     nl = TRUE)


growth_model_ca <-
  brm(gompertz_ca,
      data = data_sample_ca,
      family = brmsfamily("lognormal"),
      prior = priors3,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/grow-extra/growth_model_base_p3_ca")

add_criterion(x = growth_model_ca,
              newdata = drop_na(data = data_sample_ca,
                                dbase_mean),
              criterion = "loo")

print("growth_model_ca fit complete")

rm(growth_model_ca)
