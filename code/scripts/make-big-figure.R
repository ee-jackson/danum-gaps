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


data <-
   readRDS(here::here("data", "derived", "data_cleaned.rds"))

alive_trees <-
  data %>%
  filter(survival == 1) %>%
  filter(! if_all(c(dbh_mean, dbase_mean), is.na))

well_sampled_alive_trees <-
  alive_trees %>%
  group_by(plant_id) %>%
  summarise(n = n()) %>%
  filter(n > 2)

data_gro <-
  alive_trees %>%
  filter(plant_id %in% well_sampled_alive_trees$plant_id)

# Get models --------------------------------------------------------------

mod_surv <-
  readRDS(here::here("output", "models", "survival",
                     "survival_model_impute.rds"))

mod_gro <-
  readRDS(here::here("output", "models",
                     "growth_model_base_p3.rds"))

# -------------------------------------------------------------------------

pred_df_sz <-
  data_surv %>%
  data_grid(bsp_timetolastalive_midbase_mean_sc =
              seq(0.1, 100.1, 1)) %>%
  mutate(.chain = NA) %>%
  mutate(shape_timetolastalive = NA) %>%
  mutate(.draw = NA) %>%
  mutate(.iteration = NA) %>%
  mutate(b_timetolastalive_forest_typeprimary = NA) %>%
  mutate(b_timetolastalive_forest_typelogged = NA)

# put together the relevant estimates from the mcmc chains
mcmc_df_sz <-
  mod_surv %>%
  spread_draws(b_timetolastalive_forest_typeprimary,
               b_timetolastalive_forest_typelogged,
               shape_timetolastalive) %>%
  mutate(bsp_timetolastalive_midbase_mean_sc = NA)

# combine information for prediction and MCMC chains,
# estimate survival, hazard and relative hazard curves
curves_df_sz <-
  union(pred_df_sz, mcmc_df_sz) %>%
  expand(
    nesting(
      .chain,
      .iteration,
      .draw,
      b_timetolastalive_forest_typeprimary,
      b_timetolastalive_forest_typelogged,
      shape_timetolastalive
    ),
    bsp_timetolastalive_midbase_mean_sc
  ) %>%
  filter (!is.na(bsp_timetolastalive_midbase_mean_sc)) %>%
  filter (!is.na(.draw)) %>%
  # survival curves
  mutate (primary = exp (-(((bsp_timetolastalive_midbase_mean_sc - 0) /
                                          exp (b_timetolastalive_forest_typeprimary))^shape_timetolastalive))) %>%
  mutate (logged = exp (-(((bsp_timetolastalive_midbase_mean_sc - 0) /
                                            exp (b_timetolastalive_forest_typelogged))^shape_timetolastalive)))

pb <-
  curves_df_sz %>%
  pivot_longer(c(primary, logged)) %>%
  ggplot(aes(x = bsp_timetolastalive_midbase_mean_sc,
             y = value,
             colour = name,
             fill = name)) +
  stat_lineribbon (.width= .95, alpha = 0.5) +
  ylab("Survival probability") +
  xlab("Basal diameter /mm")


# -------------------------------------------------------------------------

grp_eff_im <-
  get_variables(mod_surv) %>%
  str_subset(pattern = "^r_genus_species")

pred_df_sz_sp <-
  data_surv %>%
  data_grid(bsp_timetolastalive_midbase_mean_sc =
              seq(0.1, 100.1, 1)) %>%
  mutate(.chain = NA) %>%
  mutate(shape_timetolastalive = NA) %>%
  mutate(.draw = NA) %>%
  mutate(.iteration = NA) %>%
  mutate(b_timetolastalive_forest_typeprimary = NA) %>%
  mutate(b_timetolastalive_forest_typelogged = NA)

pred_df_sz_sp[grp_eff_im] <- NA

mcmc_df_sz_sp <-
  mod_surv %>%
  spread_draws(shape_timetolastalive, `r_.*`,
               b_timetolastalive_forest_typeprimary,
               b_timetolastalive_forest_typelogged,
               bsp_timetolastalive_midbase_mean_sc,
               regex = TRUE) %>%
  mutate(bsp_timetolastalive_midbase_mean_sc = NA)  %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_timetolastalive_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typelogged]"),
                ~ .x + b_timetolastalive_forest_typelogged))

curves_df_sz_sp <-
  union(pred_df_sz_sp, mcmc_df_sz_sp) %>%
  expand(
    nesting(
      .chain,
      .iteration,
      .draw,
      b_timetolastalive_forest_typeprimary,
      b_timetolastalive_forest_typelogged,
      shape_timetolastalive,
      `r_genus_species__timetolastalive[Dipterocarpus_conformis,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Dryobalanops_lanceolata,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Hopea_sangal,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Parashorea_malaanonan,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Parashorea_tomentella,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_argentifolia,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_beccariana,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_faguetiana,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_gibbosa,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_johorensis,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_leprosula,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_macrophylla,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_macroptera,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_ovalis,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Shorea_parvifolia,forest_typeprimary]`,
      `r_genus_species__timetolastalive[Dipterocarpus_conformis,forest_typelogged]`,
      `r_genus_species__timetolastalive[Dryobalanops_lanceolata,forest_typelogged]`,
      `r_genus_species__timetolastalive[Hopea_sangal,forest_typelogged]`,
      `r_genus_species__timetolastalive[Parashorea_malaanonan,forest_typelogged]`,
      `r_genus_species__timetolastalive[Parashorea_tomentella,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_argentifolia,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_beccariana,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_faguetiana,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_gibbosa,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_johorensis,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_leprosula,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_macrophylla,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_macroptera,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_ovalis,forest_typelogged]`,
      `r_genus_species__timetolastalive[Shorea_parvifolia,forest_typelogged]`
    ),
    bsp_timetolastalive_midbase_mean_sc
  ) %>%
  filter(!is.na(bsp_timetolastalive_midbase_mean_sc)) %>%
  filter (!is.na(.draw)) %>%
  # survival curves
  mutate(across(contains("r_genus_species__timetolastalive["),
                ~ exp (-(((bsp_timetolastalive_midbase_mean_sc - 0) / exp (.x))^shape_timetolastalive))))

plotting_data_sp <-
  curves_df_sz_sp %>%
  pivot_longer(contains("r_genus_species__")) %>%
  mutate(genus_species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(genus_species = str_split_i(string = genus_species, pattern =",", i = 1)) %>%
  mutate(forest_type = str_split_i(string = name, pattern =",", i = 2)) %>%
  mutate(forest_type = ifelse(forest_type== "forest_typeprimary]", "primary", "logged"))

pd <-
  plotting_data_sp %>%
  ggplot (aes (x = bsp_timetolastalive_midbase_mean_sc,
               y = value,
               colour = forest_type,
               fill = forest_type)) +
  stat_lineribbon (.width = 0.95, alpha = 0.5) +
  ylab("Survival probability")+
  xlab("Basal diameter /mm") +
  facet_wrap(~genus_species, nrow = 1) +
  theme(legend.position = "bottom")


# growth -------------------------------------------------------------------


tidy_epred <-
  data_gro %>%
  data_grid(years = c(0:20),
            forest_type) %>%
  add_epred_draws(mod_gro,
                  re_formula = NA)

agr <-
  tidy_epred %>%
  group_by(forest_type, years) %>%
  point_interval(.epred,
                 .width = 0.95,
                 .point = median,
                 .interval = qi,
                 na.rm = TRUE) %>%
  group_by(forest_type) %>%
  mutate(lag_dbh_pred = lag(.epred, n = 1, order_by = years),
         lag_dbh_pred_low = lag(.lower, n = 1, order_by = years),
         lag_dbh_pred_up = lag(.upper, n = 1, order_by = years)) %>%
  rowwise() %>%
  mutate(growth_cmyr = .epred - lag_dbh_pred,
         growth_cmyr_low = .lower - lag_dbh_pred_low,
         growth_cmyr_up = .upper - lag_dbh_pred_up) %>%
  ungroup()

pa <-
  agr %>%
  ggplot() +
  geom_pointinterval(aes(x = .epred, y = growth_cmyr,
                         xmin = .lower, xmax = .upper,
                         colour = forest_type, fill = forest_type),
                     alpha = 0.5) +
  geom_pointinterval(aes(x = .epred, y = growth_cmyr,
                         ymin = growth_cmyr_low, ymax = growth_cmyr_up,
                         colour = forest_type, fill = forest_type),
                     alpha = 0.5) +
  xlab("Basal diameter (mm)") +
  ylab("Basal diamter growth (mm/yr)")


# sp level growth ---------------------------------------------------------


tidy_epred_sp <-
  data_gro %>%
  data_grid(years = c(0:20),
            forest_type,
            genus_species,
            .model = mod_gro) %>%
  add_epred_draws(object = mod_gro, ndraws = NULL,
                  re_formula = ~ (0 + forest_type |genus_species),
                  allow_new_levels = TRUE, dpar = TRUE)

agr_sp <-
  tidy_epred_sp %>%
  group_by(forest_type, genus_species, years) %>%
  point_interval(.epred,
                 .width = 0.95,
                 .point = median,
                 .interval = qi,
                 na.rm = TRUE) %>%
  group_by(forest_type, genus_species) %>%
  mutate(lag_epred = lag(.epred, n = 1, order_by = years),
         lag_epred_low = lag(.lower, n = 1, order_by = years),
         lag_epred_up = lag(.upper, n = 1, order_by = years)) %>%
  rowwise() %>%
  mutate(growth_cmyr = .epred - lag_epred,
         growth_cmyr_low = .lower - lag_epred_low,
         growth_cmyr_up = .upper - lag_epred_up) %>%
  ungroup()

pc <-
  agr_sp %>%
  ggplot() +
  geom_pointinterval(aes(x = .epred, y = growth_cmyr,
                         xmin = .lower, xmax = .upper,
                         colour = forest_type),
                     point_alpha = 1,
                     point_size = 0.75,
                     interval_alpha = 0.5,
                     interval_linewidth = 0.5) +
  geom_pointinterval(aes(x = .epred, y = growth_cmyr,
                         ymin = growth_cmyr_low, ymax = growth_cmyr_up,
                         colour = forest_type),
                     point_alpha = 1,
                     point_size = 0.75,
                     interval_alpha = 0.5,
                     interval_linewidth = 0.5) +
  facet_wrap(~genus_species, scales = "fixed", nrow = 1) +
  xlab("Basal diameter (mm)") +
  ylab("Basal diamter growth (mm/yr)") +
  theme(legend.position = "bottom")

# combine -----------------------------------------------------------------


( pa + pb ) /
  pc /
  pd + patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "none")



# combine data ------------------------------------------------------------


gro_sp <-
  agr_sp %>%
  mutate(type = "growth") %>%
  rename(basal = .epred,
         value = growth_cmyr) %>%
  select(- lag_epred, -years, -lag_epred_low, -growth_cmyr_up)


surv_sp <-
  plotting_data_sp %>%
  mutate(across(c(genus_species,forest_type), as.factor)) %>%
  rename(basal = bsp_timetolastalive_midbase_mean_sc) %>%
  select(-b_timetolastalive_forest_typeprimary,
         - b_timetolastalive_forest_typelogged,
         -shape_timetolastalive,
         -name) %>%
  group_by(forest_type, genus_species,basal) %>%
  point_interval(value, .point = median,
                 .interval = hdi) %>%
  mutate(type = "survival",
         growth_cmyr_low = NA,
         growth_cmyr_up = NA)

gro_sp %>%
  ggplot() +
  geom_lineribbon(aes(x = basal, y = value,
                      ymin = .lower, ymax = .upper,
                      colour = forest_type, fill = forest_type),
                  alpha = 0.5) +
  facet_grid(~genus_species,
             scales = "free_y")


p3 <- gro_sp %>%
  rename(.lowerx = .lower,
         .upperx = .upper) %>%
  bind_rows(surv_sp) %>%
  ggplot() +
  geom_pointinterval(aes(x = basal, y = value, # grow
                         xmin = .lowerx, xmax = .upperx,
                         colour = forest_type),
                     point_alpha = 1,
                     point_size = 0.75,
                     interval_alpha = 0.5,
                     interval_linewidth = 0.5) +
  geom_pointinterval(aes(x = basal, y = value,
                         ymin = growth_cmyr_low, ymax = growth_cmyr_up,
                         colour = forest_type),
                     point_alpha = 1,
                     point_size = 0.75,
                     interval_alpha = 0.5,
                     interval_linewidth = 0.5) +
  geom_lineribbon(aes(x = basal, y = value, # surv
                      ymin = .lower,
                      ymax = .upper,
                      colour = forest_type,
                      fill = forest_type),
                  alpha = 0.5)+
  facet_grid(type~genus_species,
             scales = "free_y") +
  xlim(0,100) +
  xlab("Basal diameter (mm)") +
  theme(legend.position = "none")


( pa + pb ) /
  p3 + patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "top")
