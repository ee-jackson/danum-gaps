#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 04_fit-suvival-models.R
## Desc: Fit survival models of increasing complexity
## Date created: 2025-03-07


# Set options -------------------------------------------------------------

options(mc.cores = 4)
options(brms.file_refit = "always")


# Packages ----------------------------------------------------------------

library("tidyverse")
library("brms")


# Get data ----------------------------------------------------------------

data <-
  readRDS("data/derived/data_cleaned.rds")


# Censor ------------------------------------------------------------------

# time to first recorded dead
interval_censored <-
  data %>%
  filter(survival == 0) %>%
  group_by(plant_id) %>%
  slice_min(survey_date, with_ties = FALSE) %>%
  ungroup() %>%
  rename(time_to_dead = years) %>%
  select(plant_id, genus_species, plot, forest_logged, cohort, time_to_dead) %>%
  mutate(censor = "interval")

# time to last recorded alive
interval_censored <-
  data %>%
  filter(plant_id %in% interval_censored$plant_id) %>%
  filter(survival == 1) %>%
  group_by(plant_id) %>%
  slice_max(survey_date, with_ties = FALSE) %>%
  ungroup() %>%
  rename(time_to_last_alive = years) %>%
  select(plant_id, time_to_last_alive, dbh_mean, dbase_mean) %>%
  right_join(interval_censored)

# trees never recorded dead
right_censored <-
  data %>%
  filter(!plant_id %in% interval_censored$plant_id) %>%
  group_by(plant_id) %>%
  slice_max(survey_date, with_ties = FALSE) %>%
  ungroup() %>%
  rename(time_to_last_alive = years) %>%
  select(plant_id, genus_species, plot, forest_logged,
         cohort, time_to_last_alive, dbh_mean, dbase_mean) %>%
  mutate(censor = "right")

data_aggregated <-
  bind_rows(interval_censored, right_censored) %>%
  filter(time_to_last_alive > 0) %>%
  mutate(dbase_mean_sc = scale(dbase_mean),
         dbh_mean_sc = scale(dbh_mean))


# prior -------------------------------------------------------------------

priors1 <- c(
  prior(normal(1, 100), class = "b", lb = 0))


# fit ---------------------------------------------------------------------

## simplest
ft_sp_sz_weibull <-
  brm(time_to_last_alive|cens(x = censor, y2 = time_to_dead) ~
        0 + forest_logged + dbase_mean_sc +
        (0 + forest_logged|genus_species),
      data = data_aggregated,
      family = brmsfamily("weibull"),
      prior = NULL,
      iter = 2000,
      cores = 4,
      chains = 4,
      seed = 123,
      init = 0,
      file = "output/models/survival/ft_sp_sz_weibull")

add_criterion(x = ft_sp_sz_weibull,
              criterion = "loo")

print("ft_sp_sz_weibull fit complete")
rm(ft_sp_sz_weibull)

## impute DBH?

bform <-
  bf(
    time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~
      0 + forest_logged + mi(dbase_mean_sc) + mi(dbh_mean_sc) +
      (0 + forest_logged | genus_species),
    family = brmsfamily("weibull")
  ) +
  bf(dbh_mean_sc |
       mi() ~ mi(dbase_mean_sc) + forest_logged,
     family = brmsfamily("gaussian")) +
  bf(dbase_mean_sc |
       mi() ~ mi(dbh_mean_sc) + forest_logged,
     family = brmsfamily("gaussian")) +
  set_rescor(FALSE)

ft_sp_sz_impute_weibull <-
  brm(bform,
      data = data_aggregated,      ,
      prior = NULL,
      iter = 2000,
      cores = 4,
      chains = 4,
      init = 0,
      seed = 123,
      file = "output/models/survival/ft_sp_sz_impute_weibull")

# add_criterion(x = ft_sp_sz_impute_weibull,
#               criterion = "loo")

print("ft_sp_sz_impute_weibull fit complete")
rm(ft_sp_sz_impute_weibull)

## interaction on intercept and slope
# https://bookdown.org/content/4857/conditional-manatees.html#adding-an-interaction-does-work.

inter_f <- bf(
  time_to_last_alive|cens(x = censor, y2 = time_to_dead) ~
    0 + a + c * dbase_mean_sc,
  a ~ 0 + forest_logged + (0 + forest_logged* dbase_mean_sc | genus_species),
  c ~ 0 + forest_logged + (0 + forest_logged* dbase_mean_sc | genus_species),
  nl = TRUE
  )

priors2 <- c(
  prior(normal(0, 100), class = b, coef = forest_logged0, nlpar = a),
  prior(normal(0, 100), class = b, coef = forest_logged1, nlpar = a),
  prior(normal(0, 100), class = b, coef = forest_logged0, nlpar = c),
  prior(normal(0, 100), class = b, coef = forest_logged1, nlpar = c)
)

ft_sp_sz_interact1_weibull <-
  brm(inter_f,
      data = data_aggregated,
      family = brmsfamily("weibull"),
      prior = priors2,
      init = 0,
      iter = 2000,
      cores = 4,
      chains = 4,
      seed = 123,
      file = "output/models/survival/ft_sp_sz_interact1_weibull")

print("ft_sp_sz_interact1_weibull fit complete")
rm(ft_sp_sz_interact1_weibull)

# or https://discourse.mc-stan.org/t/specifying-formulates-for-interaction-between-categorical-variables-with-the-index-coding-approach-in-brms/29449/2

priors3 <- c(
  prior(normal(0, 100), class = b, coef = forest_logged0:dbase_mean_sc),
  prior(normal(0, 100), class = b, coef = forest_logged1:dbase_mean_sc)
)

ft_sp_sz_interact2_weibull <-
  brm(time_to_last_alive|cens(x = censor, y2 = time_to_dead) ~
        0 + forest_logged:dbase_mean_sc +
        (0 + forest_logged:dbase_mean_sc | genus_species),
      data = data_aggregated,
      family = brmsfamily("weibull"),
      prior = priors3,
      init = 0,
      iter = 2000,
      cores = 4,
      chains = 4,
      seed = 123,
      file = "output/models/survival/ft_sp_sz_interact2_weibull")

print("ft_sp_sz_interact2_weibull fit complete")
