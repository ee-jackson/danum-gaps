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
  mutate(LA_mm2_mean = LA_cm2_mean * 100) %>% # make units match TRY data
  mutate(sla = LA_mm2_mean / dry_weight_mg_mean) %>%
  group_by(Species) %>%
  summarise(sla = median(sla, na.rm = TRUE),
            wood_density = median(WD_NB, na.rm = TRUE)) %>%
  mutate(dataset = "Both")

# get TRY traits https://www.try-db.org
traits_try <-
  read_tsv(
    here::here(
      "data",
      "raw",
      "traits",
      "43247.txt"
    ),
    na = c("", " ", "NA")
   ) %>%
  filter(DatasetID != 761) %>%
  rtry::rtry_remove_dup() %>%
  mutate(Species =
           str_replace(AccSpeciesName, " ", "_")) %>%
  mutate(trait = case_when(
    DataName == "Wood density; stem specific density; wood specific gravity (SSD)" ~ "wood_density",
    DataName == "SLA: undefined if petiole in- or excluded" |
      DataName == "SLA: petiole  excluded" ~ "sla"
  )) %>%
  drop_na(trait) %>%
  pivot_wider(names_from = trait,
              values_from = StdValue) %>%
  group_by(Species) %>%
  summarise(sla = median(sla, na.rm = TRUE),
            wood_density = median(wood_density, na.rm = TRUE)) %>%
  mutate(dataset = "TRY")


# combine model parameters with traits
# preference for Both trait data
param_traits <-
  traits_try %>%
  bind_rows(traits_both) %>%
  pivot_wider(names_from = "dataset", values_from = c(wood_density, sla)) %>%
  mutate(wood_density_g_cm3 = ifelse(is.na(wood_density_Both),
                                     wood_density_TRY,
                                     wood_density_Both )) %>%
  mutate(sla_mm2_mg = ifelse(is.na(sla_Both),
                             sla_TRY,
                             sla_Both )) %>%
  #select(Species, wood_density_g_cm3, sla_mm2_mg) %>%
  right_join(all_params) %>%
  mutate(names = case_when(
    Parameter == "A" ~ "<i>A</i>, Asymptotic basal<br>diameter (mm)",
    Parameter == "k" ~ "<i>k<sub>G</sub> / e</i>, Maximum relative<br>growth rate (% year<sup>-1</sup>)",
    Parameter == "delay" ~ "<i>T<sub>i</sub></i>, Time to reach max<br>growth rate (years)",
    Parameter == "survival" ~ "<i>survival</i>, Time to typical<br>mortality (years)"
  ))


# Plot --------------------------------------------------------------------

param_traits_median <-
  param_traits %>%
  summarise(
            iqr = IQR(diff, na.rm = TRUE),
            diff = median(diff, na.rm = TRUE),
            .by = c(Species, Parameter, names, sla_Both, wood_density_Both))

# SLA
fig_sla <-
  param_traits %>%
  ggplot(aes(y = diff,
             x = sla_Both,
             group = Species)) +
  stat_interval(interval_alpha = 0.5, show_point = TRUE,
                point_size = 1, point_fill = "white",
                point_colour = "black", size = 1.75,
                shape = 21, show.legend = FALSE,
                stroke = 0.5) +
  geom_smooth(aes(y = diff,
                  x = sla_Both,
                  group = Parameter),
              se = TRUE,
              level = 0.95,
              method = "lm",
              fill = "grey", linewidth = 0.5,
              data = param_traits_median) +
  facet_wrap(~names, scales = "free") +
  geom_hline(yintercept = 0, colour = "red", linetype = 2, linewidth = 0.25) +
  labs(y = "Additional effect of logging
       <br>(logged forest estimate - old-growth forest estimate)",
       x = "Specific leaf area (mm<sup>2</sup>/mg)") +
  theme_bw(base_size = 8) +
  theme(
    axis.title.y = element_markdown(),
    axis.title.x = element_markdown(),
    strip.text = element_markdown(lineheight = 0.5)
  )

jpeg(
  here::here("output", "figures", "SLA.jpeg"),
  width = 8.5,
  height = 8.5,
  res = 600,
  pointsize = 6,
  units = "cm",
  type = "cairo"
)
fig_sla
dev.off()

# Wood density
fig_wd <-
  param_traits %>%
  ggplot(aes(y = diff,
             x = wood_density_Both,
             group = Species)) +
  stat_interval(interval_alpha = 0.5, show_point = TRUE,
                point_size = 1, point_fill = "white",
                point_colour = "black", size = 1.75,
                shape = 21, show.legend = FALSE,
                stroke = 0.5) +
  geom_smooth(aes(y = diff,
                  x = wood_density_Both,
                  group = names),
              method = "lm",
              fill = "grey", linewidth = 0.5,
              data = param_traits_median) +
  facet_wrap(~names, scales = "free") +
  geom_hline(yintercept = 0, colour = "red", linetype = 2, linewidth = 0.25) +
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

jpeg(
  here::here("output", "figures", "wood_density.jpeg"),
  width = 8.5,
  height = 8.5,
  res = 600,
  pointsize = 6,
  units = "cm",
  type = "cairo"
)
fig_wd
dev.off()

# Combined figure

jpeg(
  here::here("output", "figures", "figure_03.jpeg"),
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

# Fit linear models -------------------------------------------------------

# get SLA model results
fit_sla_lm <- function(param, param_data, trait_data) {

  param_data %>%
    filter(Parameter == param) %>%
    group_by(Species) %>%
    summarise(diff = median(diff, na.rm = TRUE)) %>%
    inner_join(trait_data) %>%
    lm(formula = diff ~ sla) %>%
    broom::tidy(conf.int = TRUE) %>%
    mutate(y = param,
           x = "SLA")
}

results_sla <-
  map(.f = fit_sla_lm, .x = unique(param_traits$Parameter),
      param_data = all_params, trait_data = traits_both) %>%
  bind_rows()

# get wood density model results
fit_wd_lm <- function(param, param_data, trait_data) {

  param_data %>%
    filter(Parameter == param) %>%
    group_by(Species) %>%
    summarise(diff = median(diff, na.rm = TRUE)) %>%
    inner_join(trait_data) %>%
    lm(formula = diff ~ wood_density) %>%
    broom::tidy(conf.int = TRUE) %>%
    mutate(y = param,
           x = "Wood density")
}

results_wd <-
  map(.f = fit_wd_lm, .x = unique(param_traits$Parameter),
      param_data = all_params, trait_data = traits_both) %>%
  bind_rows()

all_lm_results <-
  bind_rows(
  results_wd,
  results_sla
)

write_csv(all_lm_results,
          here::here("output", "results", "trait_lms.csv"))
