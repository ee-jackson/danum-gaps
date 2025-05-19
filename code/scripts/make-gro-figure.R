#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script:
## Desc: Make growth fig
## Date created: 2025-04-21


# Packages ----------------------------------------------------------------

library("tidyverse")
library("tidybayes")
library("brms")
library("marginaleffects")
library("modelr")
library("patchwork")


# Get data ----------------------------------------------------------------

data <-
  readRDS("data/derived/data_cleaned.rds")

well_sampled_trees <-
  data %>%
  group_by(plant_id) %>%
  summarise(records = sum(!is.na(dbase_mean))) %>%
  filter(records > 2)

data_gro <-
  data %>%
  filter(survival == 1) %>%
  filter(plant_id %in% well_sampled_trees$plant_id) %>%
  drop_na(dbase_mean)


# Get models --------------------------------------------------------------

mod_gro <-
  readRDS(here::here("output", "models", "priors2",
                     "growth_model_base.rds"))

hypothesis(mod_gro,
           "A_forest_typesecondary - A_forest_typeprimary = 0")

hypothesis(mod_gro,
           "k_forest_typeprimary - k_forest_typesecondary = 0")

hypothesis(mod_gro,
           "delay_forest_typeprimary - delay_forest_typesecondary = 0")

# -------------------------------------------------------------------------

pa <-
  mod_gro %>%
  gather_draws(
    b_A_forest_typeprimary,
    b_A_forest_typesecondary,
    b_k_forest_typeprimary,
    b_k_forest_typesecondary,
    b_delay_forest_typeprimary,
    b_delay_forest_typesecondary) %>%
  mutate(forest_type = case_when(
    grepl("secondary", .variable) ~ "Secondary",
    grepl("primary", .variable) ~ "Primary")) %>%
  mutate(parameter = case_when(
    grepl("A", .variable) ~ "A",
    grepl("k", .variable) ~ "k",
    grepl("delay", .variable) ~ "delay")) %>%
  ggplot(aes(x = .value,
             fill = forest_type)) +
  stat_halfeye(.width = c(0.95, 0.5), alpha = 0.5,
               normalize = "panels") +
  theme(legend.position = "bottom") +
  facet_wrap(~parameter, scales = "free", nrow = 3) +
  labs(x = "Estimate")

pb <-
  mod_gro %>%
  spread_draws(
    b_A_forest_typeprimary,
    b_A_forest_typesecondary,
    b_k_forest_typeprimary,
    b_k_forest_typesecondary,
    b_delay_forest_typeprimary,
    b_delay_forest_typesecondary) %>%
  mutate(A = b_A_forest_typesecondary -
           b_A_forest_typeprimary) %>%
  mutate(k = b_k_forest_typeprimary -
           b_k_forest_typesecondary) %>%
  mutate(delay = b_delay_forest_typeprimary -
           b_delay_forest_typesecondary) %>%
  pivot_longer(cols = c(A, k, delay), names_to = "parameter") %>%
  ggplot(aes(x = value)) +
  stat_halfeye(.width = c(0.95, 0.5), alpha = 0.5,
               normalize = "panels") +
  theme(legend.position = "bottom") +
  facet_wrap(~parameter, scales = "free", nrow = 3) +
  labs(x = "Difference")

# -------------------------------------------------------------------------

pc_data_A <-
  mod_gro %>%
  spread_draws(
    b_A_forest_typeprimary,
    b_A_forest_typesecondary,
    `r_genus_species__A.*`, regex = TRUE) %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_A_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typesecondary]"),
                ~ .x + b_A_forest_typesecondary)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("r_genus_species__A")) %>%

  mutate(forest_type = case_when(
    grepl("secondary", name) ~ "Secondary",
    grepl("primary", name) ~ "Primary")) %>%
  mutate(Species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(Species = str_split_i(string = Species, pattern =",", i = 1)) %>%
  mutate(Parameter = "A")

pc_data_k <-
  mod_gro %>%
  spread_draws(
    b_k_forest_typeprimary,
    b_k_forest_typesecondary,
    `r_genus_species__k.*`, regex = TRUE) %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_k_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typesecondary]"),
                ~ .x + b_k_forest_typesecondary)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("r_genus_species__k")) %>%

  mutate(forest_type = case_when(
    grepl("secondary", name) ~ "Secondary",
    grepl("primary", name) ~ "Primary")) %>%
  mutate(Species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(Species = str_split_i(string = Species, pattern =",", i = 1)) %>%
  mutate(Parameter = "k")

pc_data_delay <-
  mod_gro %>%
  spread_draws(
    b_delay_forest_typeprimary,
    b_delay_forest_typesecondary,
    `r_genus_species__delay.*`, regex = TRUE) %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_delay_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typesecondary]"),
                ~ .x + b_delay_forest_typesecondary)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("r_genus_species__delay")) %>%

  mutate(forest_type = case_when(
    grepl("secondary", name) ~ "Secondary",
    grepl("primary", name) ~ "Primary")) %>%
  mutate(Species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(Species = str_split_i(string = Species, pattern =",", i = 1)) %>%
  mutate(Parameter = "delay")

pc_data <-
  bind_rows(pc_data_A,
            pc_data_delay,
            pc_data_k)

 pc <-
  pc_data %>%
  ggplot(aes(x = value, y = Species,
             fill = forest_type)) +
  stat_halfeye(.width = c(0.95, 0.5), alpha = 0.5,
               normalize = "panels") +
  theme(legend.position = "bottom") +
  facet_wrap(~Parameter, scales = "free", nrow = 3) +
  labs(x = "Estimate")

# -------------------------------------------------------------------------


pd_data_A <-
   pc_data_A %>%
   pivot_wider(names_from = forest_type,
               values_from = value,
               id_cols = c(Species, .draw, .chain, .iteration)) %>%
   mutate(Difference = abs(Primary - Secondary)) %>%
   mutate(Parameter = "A")

pd_data_k <-
   pc_data_k %>%
   pivot_wider(names_from = forest_type,
               values_from = value,
               id_cols = c(Species, .draw, .chain, .iteration)) %>%
   mutate(Difference = abs(Primary - Secondary)) %>%
   mutate(Parameter = "k")

pd_data_delay <-
   pc_data_delay %>%
   pivot_wider(names_from = forest_type,
               values_from = value,
               id_cols = c(Species, .draw, .chain, .iteration)) %>%
   mutate(Difference = Primary - Secondary) %>%
   mutate(Parameter = "delay")

pd_data <-
  bind_rows(pd_data_A,
            pd_data_delay,
            pd_data_k)

pd <-
  pd_data %>%
  ggplot(aes(x = Difference, y = Species)) +
  stat_halfeye(.width = c(0.95, 0.5), alpha = 0.5,
               normalize = "groups") +
  theme(legend.position = "bottom") +
  facet_wrap(~Parameter, scales = "free", nrow = 3) +
  labs(x = "Difference")


# -------------------------------------------------------------------------


pa+pb+pc+pd+
  patchwork::plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(guides = "collect",
                         heights = c(1, 2)) &
  theme_gray(base_size = 15) +
  theme(legend.position = "bottom")

