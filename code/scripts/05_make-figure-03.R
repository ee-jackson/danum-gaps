#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-03.R
## Desc: Make figure 3
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
  mod_surv$data

data_gro <-
  mod_gro$data

data <-
  readRDS(here::here("data", "derived", "data_cleaned.rds"))


# Get forest type predictions of survival ---------------------------------

# Functions to scale and unscale basal diameter values
scale_basal <- function(x) {
  x / attr(data_surv$dbase_mean_sc, "scaled:scale")
}

unscale_basal <- function(x) {
  x*attr(data_surv$dbase_mean_sc, "scaled:scale")
}

# Function to generate a sequence with increasing step sizes
increasing_step_sequence <- function(min_val, max_val, n_steps, growth_rate = 1.2) {
  # Input validation
  if (min_val >= max_val) {
    stop("min_val must be less than max_val")
  }
  if (n_steps < 2) {
    stop("n_steps must be at least 2")
  }
  if (growth_rate <= 1) {
    stop("growth_rate must be greater than 1")
  }

  # Initialize the sequence with the starting value
  sequence <- numeric(n_steps)
  sequence[1] <- min_val

  # Calculate total distance to cover
  total_distance <- max_val - min_val

  # Generate weights that increase exponentially
  weights <- growth_rate ^ (0:(n_steps-2))

  # Normalize weights so they sum to the total distance
  step_sizes <- weights * (total_distance / sum(weights))

  # Build the sequence by adding decreasing steps
  for (i in 2:n_steps) {
    sequence[i] <- sequence[i-1] + step_sizes[i-1]
  }

  # Ensure the last value exactly equals max_val (handle floating point precision)
  sequence[n_steps] <- max_val

  return(sequence)
}

my_sequence <-
  increasing_step_sequence(
    min_val = min(data_surv$dbase_mean_sc, na.rm = T),
    max_val = max(data_surv$dbase_mean_sc, na.rm = T),
    n_steps = 20)

surv_pred <-
  data_surv %>%
  data_grid(dbase_mean_sc = my_sequence,
            forest_type = c("logged", "primary")) %>%
  add_linpred_draws(object = mod_surv, ndraws = NULL,
                    re_formula = NA, dpar = TRUE
  ) %>%
  rowwise() %>%
  mutate(scale = mu/gamma(1+(1/shape))) %>%
  mutate(surv = exp(-(dbase_mean_sc/scale)^shape)) %>%
  group_by(dbase_mean_sc, forest_type) %>%
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


# Get species predictions of survival -------------------------------------

sp_sizes <-
  data_surv %>%
  group_by(genus_species) %>%
  summarise(min = min(dbase_mean_sc, na.rm = T),
            max = max(dbase_mean_sc, na.rm = T)) %>%
  mutate(dbase_mean_sc = map2(min, max,
                              .f = increasing_step_sequence,
                              n_steps = 10,
                              growth_rate = 1.5))

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
                    re_formula = NULL, dpar = TRUE
  ) %>%
  rowwise() %>%
  mutate(scale = mu/gamma(1+(1/shape))) %>%
  mutate(surv = exp(-(dbase_mean_sc/scale)^shape)) %>%
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


# Get forest type predictions of growth -----------------------------------

# predicting diameter at 1 year time points
gro_epred <-
  data_gro %>%
  data_grid(years = c(0:20),
            forest_type) %>%
  add_epred_draws(mod_gro,
                  re_formula = NA)

# calculate growth rate
gro_rate_epred <-
  gro_epred %>%
  group_by(forest_type, years) %>%
  point_interval(.epred,
                 .width = 0.95,
                 .point = median,
                 .interval = qi,
                 na.rm = TRUE) %>%
  group_by(forest_type) %>%
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
    gro_rate_epred %>%
    mutate(forest_type = case_when(
      grepl("logged", forest_type) ~ "Logged",
      grepl("primary", forest_type) ~ "Old-growth")) %>%
    ggplot(aes(x = x_value, y = y_value,
               xmin = x_min, xmax = x_max,
               ymin = y_min, ymax = y_max,
               colour = forest_type,
               fill = forest_type)) +
    geom_pointinterval(interval_alpha = 0.5, orientation = "y",
                       size = 0.5, show.legend = FALSE) +
    geom_pointinterval(interval_alpha = 0.5, orientation = "x",
                       size = 0.5, show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    labs(x = "Basal diameter (mm)",
         y = "Basal diameter growth (mm year<sup>-1</sup>)")

  p2 <-
    surv_pred %>%
    mutate(x_value = unscale_basal(x_value)) %>%
    mutate(forest_type = case_when(
      grepl("logged", forest_type) ~ "Logged",
      grepl("primary", forest_type) ~ "Old-growth")) %>%
    ggplot(aes(x = x_value, y = y_value,
               ymin = y_min, ymax = y_max,
               colour = forest_type,
               fill = forest_type)) +
    geom_pointinterval(interval_alpha = 0.5, orientation = "x",
                       size = 0.5) +
    geom_line() +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    labs(x = "Basal diameter (mm)",
         y = "Survival probability")

  p3 <-
    gro_rate_epred_sp %>%
    mutate(Species = str_replace(genus_species, "_", "<br>")) %>%
    mutate(Species = paste0("<i>", Species, "</i>", sep = "")) %>%
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
    facet_wrap(~Species, nrow = 1, scales = "free") +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    labs(x = "Basal diameter (mm)",
         y = "Basal diameter growth (mm year<sup>-1</sup>)")

  p4 <-
    surv_pred_sp %>%
    mutate(x_value = unscale_basal(x_value)) %>%
    mutate(Species = str_replace(genus_species, "_", "<br>")) %>%
    mutate(Species = paste0("<i>", Species, "</i>", sep = "")) %>%
    mutate(forest_type = case_when(
      grepl("logged", forest_type) ~ "Logged",
      grepl("primary", forest_type) ~ "Old-growth")) %>%
    ggplot(aes(x = x_value, y = y_value,
               ymin = y_min, ymax = y_max,
               colour = forest_type,
               fill = forest_type)) +
    geom_pointinterval(interval_alpha = 0.5, orientation = "x",
                       size = 0.01, show.legend = FALSE, stroke = 0) +
    geom_line(show.legend = FALSE, linewidth = 0.25) +
    facet_wrap(~Species, nrow = 1, scales = "free_x") +
    scale_fill_manual(values = pal) +
    scale_colour_manual(values = pal) +
    labs(x = "Basal diameter (mm)",
         y = "Survival probability")

jpeg(
    here::here("output", "figures", "figure_03.jpeg"),
    width = 18,
    height = 12,
    res = 600,
    pointsize = 6,
    units = "cm",
    type = "cairo"
  )
  ( (p1 | p2) / p3 / p4 ) +
    patchwork::plot_annotation(tag_levels = "a") +
    patchwork::plot_layout(heights = c(4,1,1)) &
    theme_bw(base_size = 4.5) +
    theme(legend.position = "inside",
          legend.position.inside = c(0.9, 0.9),
          axis.title.y = element_markdown(),
          strip.text = element_markdown(),
          legend.title = element_blank(),
          legend.text = element_text(size = 6))
dev.off()
