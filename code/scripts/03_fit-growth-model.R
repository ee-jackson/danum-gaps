#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 03_fit-growth-model.R
## Desc: Fits the growth model. Note that this script is memory-intensive and
## has a very long run time (days). Needs to be run on a supercomputer/cluster.
## Date created: 2025-03-05


# Packages ----------------------------------------------------------------

library("tidyverse")
library("brms")


# Set options -------------------------------------------------------------

options(mc.cores = 4)
options(brms.file_refit = "on_change")


# Get data ----------------------------------------------------------------

data <-
  readRDS("data/derived/data_cleaned.rds") %>%
  filter(survival == 1)

well_sampled_trees <-
  data %>%
  group_by(plant_id) %>%
  summarise(records = sum(!is.na(dbase_mean))) %>%
  filter(records > 2)

data_sample <-
  data %>%
  filter(plant_id %in% well_sampled_trees$plant_id)


# Set priors --------------------------------------------------------------

priors2 <- c(
  prior(lognormal(5, 1.2), nlpar = "A", lb = 0),
  prior(student_t(5, 0, 1), nlpar = "k", lb = 0),
  prior(student_t(5, 0, 10), nlpar = "delay"))

priors3 <- c(
  prior(lognormal(6, 1), nlpar = "A", lb = 0),
  prior(student_t(5, 0, 0.5), nlpar = "k", lb = 0),
  prior(student_t(5, 0, 5), nlpar = "delay"))


# Define formula ----------------------------------------------------------

gompertz <-
  bf(dbase_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
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
      prior = priors3,
      sample_prior = "yes",
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      iter = 15000,
      #control = list(adapt_delta = 0.9),
      file_refit = "always",
      file = "output/models/growth_model_base_p3_allo")

add_criterion(x = growth_model,
              newdata = drop_na(data = data_sample,
                                dbase_mean),
              criterion = "loo")

print("growth_model fit complete")

rm(growth_model)
