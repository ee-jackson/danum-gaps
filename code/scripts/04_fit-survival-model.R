#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 04_fit-survival-model.R
## Desc: Fit survival model
## Date created: 2025-04-10


# Set options -------------------------------------------------------------

options(mc.cores = 4)
options(brms.file_refit = "always")


# Packages ----------------------------------------------------------------

library("tidyverse")
library("brms")


# Get data ----------------------------------------------------------------

data <-
  readRDS("data/derived/data_survival.rds") %>%
  filter(canopy != "U")


# no impute ---------------------------------------------------------------


priors1 <- c(
  prior(student_t(5, 0, 5), class = "b"))


survival_model <-
  brm(time_to_last_alive|cens(x = censor, y2 = time_to_dead) ~
        0 + forest_type + dbase_mean_sc +
        (0 + forest_type | genus_species),
      data = data,
      family = brmsfamily("weibull"),
      prior = priors1,
      iter = 5000,
      cores = 4,
      chains = 4,
      seed = 123,
      init = 0,
      file_refit = "never",
      file = "output/models/survival/survival_model")

add_criterion(x = survival_model,
              newdata = drop_na(data,
                                dbase_mean, dbh_mean),
              criterion = "loo")

print("survival_model fit complete")
rm(survival_model)



# with impute -------------------------------------------------------------


priors3 <- c(
  prior(student_t(5, 0, 5), class = "b", resp = "timetolastalive"),
  prior(student_t(5, 0, 5), class = "b", resp = "dbasemeansc"),
  prior(student_t(5, 0, 5), class = "b", resp = "dbhmeansc")
)

bform <-
  bf(
    time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~
      0 + forest_type + mi(dbase_mean_sc) + mi(dbh_mean_sc) +
      (0 + forest_type | genus_species),
    family = brmsfamily("weibull")
  ) +
  bf(dbh_mean_sc |
       mi() ~ mi(dbase_mean_sc),
     family = brmsfamily("gaussian")) +
  bf(dbase_mean_sc |
       mi() ~ mi(dbh_mean_sc),
     family = brmsfamily("gaussian")) +
  set_rescor(FALSE)

survival_model_impute <-
  brm(bform,
      data = data,
      prior = priors3,
      iter = 5000,
      cores = 4,
      chains = 4,
      seed = 123,
      init = 0,
      file_refit = "always",
      file = "output/models/survival/survival_model_impute")

add_criterion(survival_model_impute,
              newdata = drop_na(data,
                                dbase_mean, dbh_mean),
              criterion = "loo")

print("survival_model_impute fit complete")
rm(survival_model_impute)

