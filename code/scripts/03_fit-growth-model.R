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
  filter(survival == 1) %>%
  drop_na(dbase_mean)

well_sampled_trees <-
  data %>%
  group_by(plant_id) %>%
  summarise(records = sum(!is.na(dbase_mean))) %>%
  filter(records > 2)

data_sample <-
  data %>%
  filter(plant_id %in% well_sampled_trees$plant_id)


# Set priors --------------------------------------------------------------

priors5 <- c(
  # logA (Student-t, heavy-tailed)
  # Centered at ln(50) ≈ 3.9120
  # Scale chosen so ~99% prior mass for A < 2000 mm
  set_prior("student_t(3, 3.9120, 0.78)", nlpar = "logA"),

  # logkG (Student-t, heavy-tailed)
  # Centered at ln(0.2) ≈ -1.6
  set_prior("student_t(3, -1.6094, 1)", nlpar = "logkG"),

  # Ti (heavy-tailed around 0 - can be negative)
  set_prior("student_t(3, 0, 10)", nlpar = "Ti")
)


# Define formula ----------------------------------------------------------

# dbase_mean ~ A * exp(-exp(-kG * (years - Ti)))
# is equation 1 in Tjørve & Tjørve (2017)
# to constrain A and kG to be positive we use:
# dbase_mean ~ exp(logA) * exp(-exp(- exp(logkG) * (years - Ti)))
# and, with `family = lognormal()`
# brms interprets our nonlinear predictor as mu on the log scale
# therefore we write the Gompertz on the log scale:
# dbase_mean ~ logA - exp(-(exp(logkG) * (years - Ti)))
# to predict dbase_mean on the response scale
# and give A and kG on the response scale as A = exp(logA) or kG = exp(logkG)

gompertz <-
  bf(dbase_mean ~ logA - exp(-(exp(logkG) * (years - Ti))),
     logA ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     logkG ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     Ti ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id),
     nl = TRUE)


# Fit model ---------------------------------------------------------------

growth_model <-
  brm(gompertz,
      data = data_sample,
      family = brmsfamily("lognormal",
                          link = "identity", link_sigma = "log"),
      prior = priors5,
      sample_prior = "yes",
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      iter = 5000,
      control = list(adapt_delta = 0.95),
      file_refit = "always",
      file = "output/models/growth_model")

add_criterion(x = growth_model,
              criterion = "loo")

print("growth_model fit complete")

rm(growth_model)
