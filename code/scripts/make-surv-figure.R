#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 04_fit-survival-model.R
## Desc: Fit survival model
## Date created: 2025-04-10


# Packages ----------------------------------------------------------------

library("tidyverse")
library("tidybayes")
library("brms")
library("marginaleffects")
library("modelr")
library("patchwork")


# Get data ----------------------------------------------------------------

data_surv <-
  readRDS(here::here("data", "derived", "data_survival.rds"))


# data_gro <-
#   readRDS(here::here("data", "derived", "data_cleaned.rds"))


# Get models --------------------------------------------------------------

mod_surv <-
  readRDS(here::here("output", "models", "survival",
                     "survival_model_impute.rds"))

hypothesis(mod_surv,
           "timetolastalive_forest_typeprimary - timetolastalive_forest_typelogged = 0")


# -------------------------------------------------------------------------

pa <-
  mod_surv %>%
  gather_draws(
    b_timetolastalive_forest_typelogged,
    b_timetolastalive_forest_typeprimary, regex=T) %>%
  mutate(.variable = case_when(
    .variable == "b_timetolastalive_forest_typelogged" ~ "Logged",
    .variable == "b_timetolastalive_forest_typeprimary" ~ "Old growth")) %>%
  rename(forest_type = .variable) %>%
  ggplot(aes(x = .value,
             fill = forest_type)) +
  stat_halfeye(.width = c(0.95, 0.5), alpha = 0.5) +
  theme(legend.position = "bottom") +
  labs(x = "Years")

pb <-
  mod_surv %>%
  spread_draws(
    b_timetolastalive_forest_typelogged,
    b_timetolastalive_forest_typeprimary, regex=T) %>%
  mutate(diff = b_timetolastalive_forest_typelogged -
           b_timetolastalive_forest_typeprimary) %>%
  ggplot(aes(x = diff)) +
  stat_halfeye(.width = c(0.95, 0.5), alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme(legend.position = "bottom") +
  labs(x = "Years difference")


pc <-
  mod_surv %>%
  spread_draws(r_genus_species__timetolastalive[genus_species,forest_type],
               b_timetolastalive_forest_typelogged,
               b_timetolastalive_forest_typeprimary, regex=T) %>%
  mutate(value = case_when(forest_type == "forest_typeprimary" ~
                             r_genus_species__timetolastalive + b_timetolastalive_forest_typeprimary,
                           forest_type == "forest_typelogged" ~
                             r_genus_species__timetolastalive + b_timetolastalive_forest_typelogged)) %>%
  mutate(forest_type = case_when(
    forest_type == "forest_typelogged" ~ "Logged",
    forest_type == "forest_typeprimary" ~ "Old growth")) %>%
  ggplot(aes(y = genus_species, x = value,
             fill = forest_type)) +
  stat_halfeye(.width = c(0.95, 0.5), alpha = 0.5) +
  labs(x = "Years", y = "Species")

pd <-
  mod_surv %>%
  spread_draws(r_genus_species__timetolastalive[genus_species,forest_type],
               b_timetolastalive_forest_typelogged,
               b_timetolastalive_forest_typeprimary, regex=T) %>%
  mutate(value = case_when(forest_type == "forest_typeprimary" ~
                             r_genus_species__timetolastalive + b_timetolastalive_forest_typeprimary,
                           forest_type == "forest_typelogged" ~
                             r_genus_species__timetolastalive + b_timetolastalive_forest_typelogged)) %>%
  pivot_wider(values_from=value, names_from= forest_type, id_cols = c(genus_species, .draw, .chain, .iteration)) %>%
  mutate(diff = forest_typelogged - forest_typeprimary) %>%
  ggplot(aes(y = genus_species, x = diff)) +
  stat_halfeye(.width = c(0.95, 0.5), alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Years difference", y = "Species")

pa+pb+pc+pd+
  patchwork::plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(guides = "collect",
                         heights = c(1, 2)) &
  theme_gray(base_size = 15) +
  theme(legend.position = "bottom")




