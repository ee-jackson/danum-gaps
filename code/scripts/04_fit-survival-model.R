#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 04_fit-survival-model.R
## Desc: Fits the survival model
## Date created: 2025-04-10


# Set options -------------------------------------------------------------

options(mc.cores = 4)
options(brms.file_refit = "always")


# Packages ----------------------------------------------------------------

library("tidyverse")
library("brms")


# Get data ----------------------------------------------------------------

data <-
  readRDS("data/derived/data_survival.rds")


# Set priors --------------------------------------------------------------

priors3 <- c(
  prior(student_t(3, 0, 2.5), class = "b")
)


# Define formula ----------------------------------------------------------

bform <-
  bf(
    time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~
      0 + forest_type + dbase_mean_sc +
      (0 + forest_type | genus_species),
    family = brmsfamily("weibull")
  )


# Fit model ---------------------------------------------------------------

survival_model <-
  brm(bform,
      data = data,
      prior = priors3,
      sample_prior = "yes",
      iter = 5000,
      cores = 4,
      chains = 4,
      seed = 123,
      init = 0,
      file_refit = "always",
      file = "output/models/survival/survival_model")

add_criterion(survival_model,
              criterion = "loo")

print("survival_model fit complete")
rm(survival_model)

