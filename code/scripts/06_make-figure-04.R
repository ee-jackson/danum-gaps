#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-04.R
## Desc: Make figure 4
## Date created: 2025-08-07


# Packages ----------------------------------------------------------------

library("tidyverse")
library("tidybayes")
library("brms")
library("modelr")
library("patchwork")
library("ggtext")


# Get models --------------------------------------------------------------

mod_surv <-
  readRDS(here::here("output", "models",
                     "survival_model.rds"))

mod_gro <-
  readRDS(here::here("output", "models",
                     "growth_model.rds"))


# Get data ----------------------------------------------------------------

data_surv <-
  readRDS("data/derived/data_survival.rds")

data_gro <-
  mod_gro$data

data <-
  readRDS(here::here("data", "derived", "data_cleaned.rds"))

names <-
  readr::read_csv(
    here::here(
      "data",
      "derived",
      "taxonomy.csv")) %>%
  select(genus_species.ORIG,
         scientificName,
         taxonID,
         scientificNameAuthorship) %>%
  mutate(genus_species = str_replace(genus_species.ORIG, " ", "_"))

# Functions to scale and unscale basal diameter values
scale_basal <- function(x) {
  x / attr(data_surv$dbase_mean_sc, "scaled:scale")
}

unscale_basal <- function(x) {
  x*attr(data_surv$dbase_mean_sc, "scaled:scale")
}


# Get species predictions of survival -------------------------------------

sp_sizes <-
  data_surv %>%
  group_by(genus_species) %>%
  summarise(min = min(dbase_mean_sc, na.rm = T),
            max = pluck(quantile(dbase_mean_sc, na.rm = T), 4)) %>%
  mutate(dbase_mean_sc = map2(min(data_surv$dbase_mean_sc, na.rm = T),
                              scale_basal(100),
                              .f = seq,
                              length.out = 20))

sp_sizes_l <- sp_sizes %>% mutate(forest_type = "logged")
sp_sizes_p <- sp_sizes %>% mutate(forest_type = "primary")

sp_sizes_pl <-
  bind_rows(sp_sizes_l, sp_sizes_p) %>%
  select(-c(min, max))

surv_pred_sp <-
  data_surv %>%
  data_grid(forest_type = c("logged", "primary"),
            genus_species = unique(data_surv$genus_species)) %>%
  left_join(sp_sizes_pl, by = c("genus_species", "forest_type")) %>%
  unnest(dbase_mean_sc) %>%
  add_linpred_draws(object = mod_surv, ndraws = NULL,
                    re_formula = NULL, dpar = TRUE, transform = TRUE
  ) %>%
  rowwise() %>%
  mutate(scale = mu/gamma(1+(1/shape))) %>%
  mutate(surv = exp(-(20/scale)^shape)) %>%
  group_by(dbase_mean_sc, forest_type, genus_species) %>%
  point_interval(surv,
                 .width = 0.95,
                 .point = median,
                 .interval = qi,
                 na.rm = TRUE) %>%
  rename(y_min = .lower,
         y_max = .upper,
         y_value = surv,
         x_value = dbase_mean_sc) %>%
  mutate(type = "survival")


# Get species predictions of growth ---------------------------------------

gro_epred_sp <-
  data_gro %>%
  data_grid(years = c(0:20),
            forest_type,
            genus_species,
            .model = mod_gro) %>%
  add_epred_draws(object = mod_gro, ndraws = NULL,
                  re_formula =
                    logA ~ 0 + forest_type|genus_species,
                  logkG ~ 0 + forest_type|genus_species,
                  Ti ~ 0 + forest_type|genus_species
  )

gro_rate_epred_sp <-
  gro_epred_sp %>%
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
  mutate(y_value = .epred - lag_epred,
         y_min = .lower - lag_epred_low,
         y_max = .upper - lag_epred_up) %>%
  ungroup() %>%
  rename(x_min = .lower,
         x_max = .upper,
         x_value = .epred) %>%
  mutate(type = "growth")


# Plot --------------------------------------------------------------------

pal <-
  c("Logged" = "#e69f00", "Old-growth" = "#009e73")


# Build figure ------------------------------------------------------------

p1 <-
  gro_rate_epred_sp %>%
  left_join(names) %>%
  mutate(Species = paste0("<i>", scientificName, "</i>", sep = "")) %>%
  mutate(forest_type = case_when(
    grepl("logged", forest_type) ~ "Logged",
    grepl("primary", forest_type) ~ "Old-growth")) %>%
  ggplot(aes(x = x_value, y = y_value,
             xmin = x_min, xmax = x_max,
             ymin = y_min, ymax = y_max,
             colour = forest_type,
             fill = forest_type)) +
  geom_pointinterval(interval_alpha = 0.5, orientation = "y",
                     size = 0.01, show.legend = FALSE, stroke = 0) +
  geom_pointinterval(interval_alpha = 0.5, orientation = "x",
                     size = 0.01, show.legend = FALSE, stroke = 0) +
  geom_line(show.legend = FALSE, linewidth = 0.25) +
  facet_wrap(~Species, ncol = 1, scales = "free") +
  scale_y_continuous(n.breaks = 3) +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  labs(x = "Basal diameter (mm)",
       y = "Basal diameter growth (mm year<sup>-1</sup>)")

p2 <-
  surv_pred_sp %>%
  mutate(x_value = unscale_basal(x_value)) %>%
  left_join(names) %>%
  mutate(Species = paste0("<i>", scientificName, "</i>", sep = "")) %>%
  mutate(forest_type = case_when(
    grepl("logged", forest_type) ~ "Logged",
    grepl("primary", forest_type) ~ "Old-growth")) %>%
  ggplot(aes(x = x_value, y = y_value,
             ymin = y_min, ymax = y_max,
             colour = forest_type,
             fill = forest_type)) +
  geom_pointinterval(interval_alpha = 0.5, orientation = "x",
                     size = 0.01, stroke = 0) +
  geom_line(show.legend = FALSE, linewidth = 0.25) +
  facet_wrap(~Species, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  labs(x = "Basal diameter (mm)",
       y = "Survival probability")


png(
  here::here("output", "figures", "figure_04.png"),
  width = 8,
  height = 20,
  res = 600,
  pointsize = 6,
  units = "cm",
  type = "cairo",
  bg = "white"
)
(p1 | p2) +
  patchwork::plot_annotation(tag_levels = "a") &
  theme_bw(base_size = 6) +
  theme(legend.position = "right",
        legend.margin = margin(0, 0, 0, 0),
        axis.title.y = element_markdown(),
        strip.text = element_markdown(),
        legend.title = element_blank(),
        legend.text = element_text(size = 6))
dev.off()
