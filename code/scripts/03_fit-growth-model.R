#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 03_fit-growth-model.R
## Desc: Fit final growth model
## Date created: 2025-03-05


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
  summarise(records = sum(!is.na(dbh_mean))) %>%
  filter(records > 2)

data_sample <-
  data %>%
  filter(survival == 1) %>%
  filter(plant_id %in% well_sampled_trees$plant_id)


# Set priors --------------------------------------------------------------

priors1 <- c(
  prior(lognormal(5, 1.2), nlpar = "A", lb = 0),
  prior(student_t(5, 0, 1), nlpar = "k", lb = 0),
  prior(student_t(5, 0, 10), nlpar = "delay"))

# Define formula ----------------------------------------------------------

gompertz <-
  bf(dbh_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     k ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     delay ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     nl = TRUE)


# Fit model ---------------------------------------------------------------

growth_model <-
  brm(gompertz,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors1,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      iter = 5000,
      control = list(adapt_delta = 0.9),
      file_refit = "never",
      file = "output/models/priors2/growth_model")

add_criterion(x = growth_model,
              criterion = "loo")

print("growth_model fit complete")

rm(growth_model)
rm(data_sample)
rm(well_sampled_trees)

# impute missing data -----------------------------------------------------

alive_trees <-
  data %>%
  filter(survival == 1) %>%
  filter(! if_all(c(dbh_mean, dbase_mean), is.na))

well_sampled_alive_trees <-
  alive_trees %>%
  group_by(plant_id) %>%
  summarise(n = n()) %>%
  filter(n > 2)

data_sample2 <-
  alive_trees %>%
  filter(plant_id %in% well_sampled_alive_trees$plant_id)

bform <-
  bf(dbh_mean | mi() ~ A * exp( -exp( -(k * (years - delay) ) ) ),
     A ~ 0 + forest_type + mi(dbase_mean) +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     k ~ 0 + forest_type + mi(dbase_mean) +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     delay ~ 0 + forest_type + mi(dbase_mean) +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     family = brmsfamily("gaussian"),
     nl = TRUE) +
  bf(dbase_mean |
       mi() ~ mi(dbh_mean),
     family = brmsfamily("gaussian")) +
  set_rescor(FALSE)

priors2 <- c(
  prior(student_t(5, 50, 100), nlpar = "A", lb = 0, resp = "dbhmean"),
  prior(student_t(5, 0, 1), nlpar = "k", lb = 0, resp = "dbhmean"),
  prior(student_t(5, 0, 10), nlpar = "delay", resp = "dbhmean"),
  prior(student_t(5, 50, 100), lb = 0, class = "b", resp = "dbasemean")
)

growth_model_impute <-
  brm(bform,
      data = data_sample2,
      prior = priors2,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      iter = 5000,
      file_refit = "always",
      file = "output/models/priors2/growth_model_impute")

add_criterion(x = growth_model_impute,
              newdata = drop_na(data = data_sample2,
                                dbh_mean, dbase_mean),
              criterion = "loo")

print("growth_model_impute fit complete")

rm(growth_model_impute)

# impute DBH -----------------------------------------------------

bform <-
  bf(dbase_mean | mi() ~ A * exp( -exp( -(k * (years - delay) ) ) ),
     A ~ 0 + forest_type + mi(dbh_mean) +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     k ~ 0 + forest_type + mi(dbh_mean) +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     delay ~ 0 + forest_type + mi(dbh_mean) +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     family = brmsfamily("gaussian"),
     nl = TRUE) +
  bf(dbh_mean |
       mi() ~ mi(dbase_mean),
     family = brmsfamily("gaussian")) +
  set_rescor(FALSE)

priors3 <- c(
  prior(student_t(5, 50, 100), nlpar = "A", lb = 0, resp = "dbasemean"),
  prior(student_t(5, 0, 1), nlpar = "k", lb = 0, resp = "dbasemean"),
  prior(student_t(5, 0, 10), nlpar = "delay", resp = "dbasemean"),
  prior(student_t(5, 50, 100), lb = 0, class = "b", resp = "dbhmean"),
)

growth_model_impute_base <-
  brm(bform,
      data = data_sample2,
      prior = priors3,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      iter = 5000,
      file_refit = "always",
      file = "output/models/priors2/growth_model_impute_base")

add_criterion(x = growth_model_impute_base,
              newdata = drop_na(data = data_sample2,
                                dbh_mean, dbase_mean),
              criterion = "loo")

print("growth_model_impute_base fit complete")

rm(growth_model_impute_base)
