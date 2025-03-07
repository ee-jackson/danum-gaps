#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 02_fit-growth-models.R
## Desc: Fit growth models of increasing complexity
## Date created: 2025-01-10


# Set options -------------------------------------------------------------

options(mc.cores = 4)
options(brms.file_refit = "on_change")


# Packages ----------------------------------------------------------------

library("tidyverse")
library("brms")


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
  prior(student_t(5, 0, 2), nlpar = "k", lb = 0),
  prior(student_t(5, 0, 20), nlpar = "delay"))


# Forest type only models -------------------------------------------------

# family = Gaussian, flat priors
gompertz1 <-
  bf(dbh_mean ~ A * exp( -exp( -(k * (years - delay) ) ) ),
     A ~ 0 + forest_type + (1 | plant_id),
     k ~ 0 + forest_type + (1 | plant_id),
     delay ~ 0 + forest_type + (1 | plant_id),
     nl = TRUE)

ft_gau <-
  brm(gompertz1,
      data = data_sample,
      family = brmsfamily("gaussian"),
      prior = NULL,
      iter = 1000,
      cores = 4,
      chains = 4,
      seed = 123,
      file = "output/models/ft_gau")

add_criterion(x = ft_gau,
              criterion = "loo")

print("ft_gau fit complete")

rm(ft_gau)

# family = lognormal, flat priors
gompertz2 <-
  bf(dbh_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type + (1 | plant_id),
     k ~ 0 + forest_type + (1 | plant_id),
     delay ~ 0 + forest_type + (1 | plant_id),
     nl = TRUE)

ft_lognorm <-
  brm(gompertz2,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = NULL,
      iter = 1000,
      cores = 4,
      chains = 4,
      seed = 123,
      file = "output/models/ft_lognorm")

add_criterion(x = ft_lognorm,
              criterion = "loo")

print("ft_lognorm fit complete")

rm(ft_lognorm)

# family = lognormal
gompertz2 <-
  bf(dbh_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type + (1 | plant_id),
     k ~ 0 + forest_type + (1 | plant_id),
     delay ~ 0 + forest_type + (1 | plant_id),
     nl = TRUE)

ft_lognorm_priors <-
  brm(gompertz2,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors1,
      iter = 1000,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/priors2/ft_lognorm_priors")

add_criterion(x = ft_lognorm_priors,
              criterion = "loo")

print("ft_lognorm_priors fit complete")

rm(ft_lognorm_priors)


# add species -------------------------------------------------------------

gompertz3 <-
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

ft_sp_lognorm_priors <-
  brm(gompertz3,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors1,
      iter = 1000,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/priors2/ft_sp_lognorm_priors")

add_criterion(x = ft_sp_lognorm_priors,
              criterion = "loo")

print("ft_sp_lognorm_priors fit complete")

rm(ft_sp_lognorm_priors)


# add plot ----------------------------------------------------------------

gompertz4 <-
  bf(dbh_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id) +
       (1 | forest_type:plot),
     k ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id) +
       (1 | forest_type:plot),
     delay ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id) +
       (1 | forest_type:plot),
     nl = TRUE)

ft_sp_pl_lognorm_priors <-
  brm(gompertz4,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors1,
      iter = 1000,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/priors2/ft_sp_pl_lognorm_priors")

add_criterion(x = ft_sp_pl_lognorm_priors,
              criterion = "loo")

print("ft_sp_pl_lognorm_priors fit complete")

rm(ft_sp_pl_lognorm_priors)


# add cohort --------------------------------------------------------------

gompertz5 <-
  bf(dbh_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id) +
       (1 | cohort),
     k ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id) +
       (1 | cohort),
     delay ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id) +
       (1 | cohort),
     nl = TRUE)

ft_sp_co_lognorm_priors <-
  brm(gompertz5,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors1,
      iter = 1000,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/priors2/ft_sp_co_lognorm_priors")

add_criterion(x = ft_sp_co_lognorm_priors,
              criterion = "loo")

print("ft_sp_co_lognorm_priors fit complete")

rm(ft_sp_co_lognorm_priors)

# add climber cut ---------------------------------------------------------

data_sample <-
  data_sample %>%
  mutate(climber_cut = case_when(
    forest_type == "secondary" & plot == "05" |
    forest_type == "secondary" & plot == "11" |
    forest_type == "secondary" & plot == "14" ~ TRUE,
    .default  = FALSE
  ))

gompertz7 <-
  bf(dbh_mean ~ log(A) * exp( -exp( -(k * (years - delay) ) ) ),
     log(A) ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id) +
       (1 | climber_cut),
     k ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id) +
       (1 | climber_cut),
     delay ~ 0 + forest_type +
       (0 + forest_type|genus_species) +
       (1 | plant_id) +
       (1 | climber_cut),
     nl = TRUE)

ft_sp_cc_lognorm_priors <-
  brm(gompertz7,
      data = data_sample,
      family = brmsfamily("lognormal"),
      prior = priors1,
      iter = 1000,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/priors2/ft_sp_cc_lognorm_priors")

add_criterion(x = ft_sp_cc_lognorm_priors,
              criterion = "loo")

print("ft_sp_cc_lognorm_priors fit complete")

rm(ft_sp_cc_lognorm_priors)
