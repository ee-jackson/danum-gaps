#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 05_relate-sp-traits.R
## Desc: Relates species growth and survival responses to functional traits
## Date created: 2025-07-30


# Packages ----------------------------------------------------------------

library("tidyverse")
library("tidybayes")
library("patchwork")
library("rtry")
library("ggtext")


# Get growth --------------------------------------------------------------

mod_gro <-
  readRDS(here::here("output", "models",
                     "growth_model_base_p3_allo.rds"))

pc_data_A <-
  mod_gro %>%
  spread_draws(
    b_A_forest_typeprimary,
    b_A_forest_typelogged,
    `r_genus_species__A.*`, regex = TRUE) %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_A_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typelogged]"),
                ~ .x + b_A_forest_typelogged)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("r_genus_species__A")) %>%

  mutate(forest_type = case_when(
    grepl("logged", name) ~ "logged",
    grepl("primary", name) ~ "Primary")) %>%
  mutate(Species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(Species = str_split_i(string = Species, pattern =",", i = 1)) %>%
  mutate(Parameter = "A") %>%
  select(- b_A_forest_typeprimary, -b_A_forest_typelogged, -name)

pc_data_k <-
  mod_gro %>%
  spread_draws(
    b_k_forest_typeprimary,
    b_k_forest_typelogged,
    `r_genus_species__k.*`, regex = TRUE) %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_k_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typelogged]"),
                ~ .x + b_k_forest_typelogged)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("r_genus_species__k")) %>%

  mutate(forest_type = case_when(
    grepl("logged", name) ~ "logged",
    grepl("primary", name) ~ "Primary")) %>%
  mutate(Species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(Species = str_split_i(string = Species, pattern =",", i = 1)) %>%
  mutate(Parameter = "k") %>%
  select(- b_k_forest_typeprimary, -b_k_forest_typelogged, -name) %>%
  mutate(value = (value / exp(1))*100)

pc_data_delay <-
  mod_gro %>%
  spread_draws(
    b_delay_forest_typeprimary,
    b_delay_forest_typelogged,
    `r_genus_species__delay.*`, regex = TRUE) %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_delay_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typelogged]"),
                ~ .x + b_delay_forest_typelogged)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("r_genus_species__delay")) %>%

  mutate(forest_type = case_when(
    grepl("logged", name) ~ "logged",
    grepl("primary", name) ~ "Primary")) %>%
  mutate(Species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(Species = str_split_i(string = Species, pattern =",", i = 1)) %>%
  mutate(Parameter = "delay") %>%
  select(- b_delay_forest_typeprimary, -b_delay_forest_typelogged, -name)

growth_params <-
  bind_rows(pc_data_A,
            pc_data_delay,
            pc_data_k) %>%
  mutate(value = case_when(
    Parameter == "k" ~ (value / exp(1))*100,
    .default = value
  )) %>%
  pivot_wider(names_from = forest_type, values_from = value) %>%
  rowwise() %>%
  mutate(
    diff = logged - Primary
  )


# Get survival ------------------------------------------------------------

mod_surv <-
  readRDS(here::here("output", "models", "survival",
                     "survival_model_allo_nocenter.rds"))

surv_params <-
  mod_surv %>%
  spread_draws(r_genus_species[genus_species,forest_type],
               b_forest_typelogged,
               b_forest_typeprimary, regex=T) %>%
  mutate(value = case_when(forest_type == "forest_typeprimary" ~
                             r_genus_species +
                             b_forest_typeprimary,
                           forest_type == "forest_typelogged" ~
                             r_genus_species +
                             b_forest_typelogged)) %>%
  mutate(forest_type = case_when(
    forest_type == "forest_typelogged" ~ "logged",
    forest_type == "forest_typeprimary" ~ "Primary")) %>%
  pivot_wider(names_from = forest_type,
              id_cols = c(genus_species, .chain, .iteration, .draw),
              values_from = value) %>%
  mutate(diff = logged - Primary,
         Parameter = "survival") %>%
  rename(Species = genus_species)


# Combine growth and survival responses -----------------------------------

all_params <-
  bind_rows(growth_params, surv_params)


# Get traits --------------------------------------------------------------

# From Both et al. 2018 https://doi.org/10.1111/nph.15444
traits_both <-
  readxl::read_excel(
    here::here(
      "data",
      "raw",
      "traits",
      "Both_tree_functional_traits_subset RV.xlsx"
    ),
    sheet = 4,
    skip = 6,
    na = c("", " ", "NA")
  ) %>%
  mutate(Species =
           str_replace(species, "\\.", "_")) %>%
  select(tree_id, Species, forest_type, location,
         WD_NB, LA_cm2_mean, dry_weight_mg_mean) %>%
  filter(forest_type == "OG") %>%
  filter(Species %in% all_params$Species) %>%
  mutate(LA_mm2_mean = LA_cm2_mean * 100) %>%
  mutate(sla = LA_mm2_mean / dry_weight_mg_mean) %>%
  group_by(Species) %>%
  summarise(sla_med = median(sla, na.rm = TRUE),
            sla_iqr = IQR(sla, na.rm = TRUE),
            wood_density_med = median(WD_NB, na.rm = TRUE),
            wood_density_iqr = IQR(WD_NB, na.rm = TRUE))


# Combine traits and model estimates --------------------------------------

param_traits_median <-
  traits_both %>%
  left_join(all_params) %>%
  mutate(names = case_when(
    Parameter == "A" ~ "<i>A</i>, Asymptotic basal<br>diameter (mm)",
    Parameter == "k" ~ "<i>k<sub>G</sub> / e</i>, Maximum relative<br>growth rate (% year<sup>-1</sup>)",
    Parameter == "delay" ~ "<i>T<sub>i</sub></i>, Time to reach max<br>growth rate (years)",
    Parameter == "survival" ~ "<i>survival</i>, Time to typical<br>mortality (years)"
  )) %>%
  group_by(Species, Parameter, names,
           sla_med, sla_iqr, wood_density_med, wood_density_iqr) %>%
  summarise(
    diff_mean = mean(diff, na.rm = TRUE))

# Plot --------------------------------------------------------------------

# SLA
fig_sla <-
  param_traits_median %>%
  ggplot(aes(y = diff_mean,
             x = sla_med)) +
  geom_point(shape = 16, alpha = 0.6) +
  facet_wrap(~names, scales = "free") +
  geom_hline(yintercept = 0, colour = "red", linetype = 2, linewidth = 0.25) +
  ggpubr::stat_cor(size = 2, label.y.npc = 0.85) +
  labs(y = "Additional effect of logging
       <br>(logged forest estimate - old-growth forest estimate)",
       x = "Specific leaf area (mm<sup>2</sup>/mg)") +
  theme_bw(base_size = 8) +
  theme(
    axis.title.y = element_markdown(),
    axis.title.x = element_markdown(),
    strip.text = element_markdown(lineheight = 0.5)
  )

# Wood density
fig_wd <-
  param_traits_median %>%
  ggplot(aes(y = diff_mean,
             x = wood_density_med)) +
  geom_point(shape = 16, alpha = 0.6) +
  facet_wrap(~names, scales = "free") +
  geom_hline(yintercept = 0, colour = "red", linetype = 2, linewidth = 0.25) +
  ggpubr::stat_cor(size = 2, label.y.npc = 0.85) +
  labs(y = "Additional effect of logging
       <br>(logged forest estimate - old-growth forest estimate)",
       x = "Wood density (g/cm<sup>3</sup>)") +
  scale_x_continuous(n.breaks = 4) +
  theme_bw(base_size = 8) +
  theme(
    axis.title.y = element_markdown(),
    axis.title.x = element_markdown(),
    strip.text = element_markdown(lineheight = 0.5)
  )


# Combined figure

jpeg(
  here::here("output", "figures", "figure_05.jpeg"),
  width = 8,
  height = 16,
  res = 600,
  pointsize = 6,
  units = "cm",
  type = "cairo"
)

fig_sla + fig_wd +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "a")

dev.off()
