#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-04.R
## Desc: Make figure 4
## Date created: 2025-04-21


# Packages ----------------------------------------------------------------

library("tidyverse")
library("tidybayes")
library("brms")
library("modelr")
library("patchwork")
library("ggtext")


# Get models --------------------------------------------------------------

mod_gro <-
  readRDS(here::here("output", "models",
                     "growth_model.rds"))

mod_surv <-
  readRDS(here::here("output", "models",
                     "survival_model.rds"))


# Test hypotheses ---------------------------------------------------------

hypothesis(mod_surv,
           "forest_typelogged - forest_typeprimary = 0")

hypothesis(mod_gro,
           "logA_forest_typelogged - logA_forest_typeprimary = 0")

hypothesis(mod_gro,
           "logkG_forest_typelogged - logkG_forest_typeprimary = 0")

hypothesis(mod_gro,
           "Ti_forest_typelogged - Ti_forest_typeprimary = 0")


# Make summary tables -----------------------------------------------------

# In lognormal models, brms provides the median on the log scale
post_summary_gro <-
  summary(mod_gro)$fixed %>%
  rownames_to_column(var = "Parameter") %>%
  mutate(`Forest type` = str_split_i(string = Parameter, pattern ="_", i = 3)) %>%
  mutate(`Forest type` = case_when(
    grepl("typelogged", `Forest type`) ~ "Logged",
    grepl("typeprimary", `Forest type`) ~ "Old-growth")) %>%
  mutate(Parameter = str_split_i(string = Parameter, pattern ="_", i = 1)) %>%
  mutate(across(c(Estimate, `l-95% CI`, `u-95% CI`), ~
                  case_when(
                    Parameter == "logkG" | Parameter == "logA" ~ exp(.),
                    .default = .
                  ))) %>%
  mutate(across(c(Estimate, `l-95% CI`, `u-95% CI`), ~
                  case_when(
                    Parameter == "logkG" ~ (. / exp(1))*100,
                    .default = .
                  ))) %>%
  mutate(Parameter = ifelse(Parameter == "Ti", "T~i~", Parameter)) %>%
  mutate(Parameter = ifelse(Parameter == "logkG", "k~G~/e", Parameter)) %>%
  mutate(Parameter = ifelse(Parameter == "logA", "A", Parameter)) %>%
  select(Parameter, `Forest type`, Estimate, `l-95% CI`,
         `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS) %>%
  rename(`Posterior median` = Estimate,
         `Bulk effective sample size` = Bulk_ESS,
         `Tail effective sample size` = Tail_ESS) %>%
  mutate(across(!Parameter & !`Forest type`, \(x) round(x, 3)))

write_csv(post_summary_gro,
          here::here(
            "output",
            "results",
            "posterior_summary_growth.csv"))

# For the survival model we need to specify robust = TRUE to get the median and MAD
post_summary_surv <-
  summary(mod_surv, robust = TRUE)$fixed %>%
  rownames_to_column(var = "Parameter") %>%
  mutate(Parameter = case_when(
    grepl("forest_typelogged", Parameter) ~ "Logged forest",
    grepl("forest_typeprimary", Parameter) ~ "Old-growth forest",
    grepl("dbase_mean_sc", Parameter) ~ "Basal diameter")) %>%
  mutate(Parameter = str_split_i(string = Parameter, pattern ="_", i = 1)) %>%
  select(Parameter, Estimate, `l-95% CI`,
         `u-95% CI`, Rhat, Bulk_ESS, Tail_ESS) %>%
  rename(`Posterior median` = Estimate,
         `Bulk effective sample size` = Bulk_ESS,
         `Tail effective sample size` = Tail_ESS) %>%
  mutate(across(!Parameter, \(x) round(x, 3)))

write_csv(post_summary_surv,
          here::here(
            "output",
            "results",
            "posterior_summary_survival.csv"))


# Get forest type estimates -----------------------------------------------

# growth model parameter estimates
ft_ests_grow <-
  mod_gro %>%
  gather_draws(
    b_logA_forest_typeprimary,
    b_logA_forest_typelogged,
    b_logkG_forest_typeprimary,
    b_logkG_forest_typelogged,
    b_Ti_forest_typeprimary,
    b_Ti_forest_typelogged
  ) %>%
  mutate(forest_type = case_when(
    grepl("logged", .variable) ~ "Logged",
    grepl("primary", .variable) ~ "Old-growth"
  )) %>%
  mutate(parameter = case_when(
    grepl("logA", .variable) ~ "A",
    grepl("logkG", .variable) ~ "kG",
    grepl("Ti", .variable) ~ "Ti"
  )) %>%
  mutate(.value =
           case_when(parameter == "kG" |
                       parameter == "A" ~ exp(.value),
                     .default = .value)) %>%
  mutate(.value =
           case_when(parameter == "kG" ~ (.value / exp(1)) * 100,
                     .default = .value))

# survival model parameter estimates
ft_ests_surv <-
  mod_surv %>%
  gather_draws(
    b_forest_typelogged,
    b_forest_typeprimary, regex = T) %>%
  mutate(forest_type = case_when(
    grepl("logged", .variable) ~ "Logged",
    grepl("primary", .variable) ~ "Old-growth")) %>%
  mutate(parameter = "survival")

# combine growth and survival
ft_ests <-
  bind_rows(ft_ests_grow, ft_ests_surv) %>%
  mutate(name = case_when(
    parameter == "A" ~ "<i>A</i>, Asymptotic basal diameter (mm)",
    parameter == "kG" ~ "<i>k<sub>G</sub> / e</i>, Maximum relative growth rate (% year<sup>-1</sup>)",
    parameter == "Ti" ~ "<i>T<sub>i</sub></i>, Time to reach max growth rate (years)",
    parameter == "survival" ~ "Effect of forest type on <i>time to mortality</i> (years)"
  ))


# Get species-level estimates ---------------------------------------------

# A parameter
sp_ests_A <-
  mod_gro %>%
  spread_draws(
    b_logA_forest_typeprimary,
    b_logA_forest_typelogged,
    `r_genus_species__logA.*`, regex = TRUE) %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_logA_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typelogged]"),
                ~ .x + b_logA_forest_typelogged)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("r_genus_species__logA")) %>%

  mutate(forest_type = case_when(
    grepl("logged", name) ~ "Logged",
    grepl("primary", name) ~ "Old-growth")) %>%
  mutate(Species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(Species = str_split_i(string = Species, pattern =",", i = 1)) %>%
  mutate(parameter = "A") %>%
  select(-c("b_logA_forest_typeprimary", "b_logA_forest_typelogged")) %>%
  mutate(value = exp(value))

# kG parameter
sp_ests_kG <-
  mod_gro %>%
  spread_draws(
    b_logkG_forest_typeprimary,
    b_logkG_forest_typelogged,
    `r_genus_species__logkG.*`, regex = TRUE) %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_logkG_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typelogged]"),
                ~ .x + b_logkG_forest_typelogged)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("r_genus_species__logkG")) %>%

  mutate(forest_type = case_when(
    grepl("logged", name) ~ "Logged",
    grepl("primary", name) ~ "Old-growth")) %>%
  mutate(Species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(Species = str_split_i(string = Species, pattern =",", i = 1)) %>%
  mutate(parameter = "kG") %>%
  mutate(value = exp(value)) %>%
  mutate(value = (value / exp(1))*100) %>%
  select(-c("b_logkG_forest_typeprimary", "b_logkG_forest_typelogged"))

# delay parameter
sp_ests_Ti <-
  mod_gro %>%
  spread_draws(
    b_Ti_forest_typeprimary,
    b_Ti_forest_typelogged,
    `r_genus_species__Ti.*`, regex = TRUE) %>%
  rowwise() %>%
  mutate(across(contains(",forest_typeprimary]"),
                ~ .x + b_Ti_forest_typeprimary)) %>%
  mutate(across(contains(",forest_typelogged]"),
                ~ .x + b_Ti_forest_typelogged)) %>%
  ungroup() %>%
  pivot_longer(cols = contains("r_genus_species__Ti")) %>%
  mutate(forest_type = case_when(
    grepl("logged", name) ~ "Logged",
    grepl("primary", name) ~ "Old-growth")) %>%
  mutate(Species = str_split_i(string = name, pattern ="\\[", i = 2)) %>%
  mutate(Species = str_split_i(string = Species, pattern =",", i = 1)) %>%
  mutate(parameter = "Ti") %>%
  select(-c("b_Ti_forest_typeprimary", "b_Ti_forest_typelogged"))

# survival
sp_ests_surv <-
  mod_surv %>%
  spread_draws(r_genus_species[genus_species,forest_type],
               b_forest_typelogged,
               b_forest_typeprimary, regex = TRUE) %>%
  mutate(value = case_when(forest_type == "forest_typeprimary" ~
                             r_genus_species + b_forest_typeprimary,
                           forest_type == "forest_typelogged" ~
                             r_genus_species + b_forest_typelogged)) %>%
  mutate(forest_type = case_when(
    forest_type == "forest_typelogged" ~ "Logged",
    forest_type == "forest_typeprimary" ~ "Old-growth")) %>%
  rename(Species = genus_species) %>%
  mutate(parameter = "survival") %>%
  select(-c("b_forest_typelogged", "b_forest_typeprimary", "r_genus_species"))

# combine
sp_ests <-
  bind_rows(sp_ests_A,
            sp_ests_Ti,
            sp_ests_kG,
            sp_ests_surv)%>%
  mutate(name = case_when(
    parameter == "A" ~ "<i>A</i>, Asymptotic basal diameter (mm)",
    parameter == "kG" ~ "<i>k<sub>G</sub> / e</i>, Maximum relative growth rate (% year<sup>-1</sup>)",
    parameter == "Ti" ~ "<i>T<sub>i</sub></i>, Time to reach max growth rate (years)",
    parameter == "survival" ~ "Effect of forest type on <i>time to mortality</i> (years)"
  )) %>%
  mutate(Species = str_replace(Species, "_", " ")) %>%
  mutate(Species = paste0("<i>", Species, "</i>", sep = ""))


# Make figure panels ------------------------------------------------------

pal <-
  c("Logged" = "#e69f00", "Old-growth" = "#009e73")

pa <-
  ft_ests %>%
  ggplot(aes(x = .value,
             fill = forest_type)) +
  stat_halfeye(.width = 0.95, slab_alpha = 0.5, point_interval = "median_qi",
               size = 0.25, normalize = "panels", show.legend = FALSE) +
  theme(legend.position = "bottom") +
  facet_wrap(~name, scales = "free", nrow = 4) +
  scale_fill_manual(values = pal) +
  labs(x = "Estimate", y = "Density")

pb <-
  ft_ests %>%
  ungroup() %>%
  select(-.variable) %>%
  pivot_wider(names_from = forest_type,
              values_from = .value) %>%
  mutate(diff = Logged - `Old-growth`) %>%
  ggplot(aes(x = diff)) +
  stat_halfeye(.width = 0.95, slab_alpha = 0.5, point_interval = "median_qi",
               size = 0.25, normalize = "panels") +
  theme(legend.position = "bottom") +
  facet_wrap(~name, scales = "free", nrow = 4) +
  geom_vline(xintercept = 0, linetype = 2, colour = "red", linewidth = 0.5) +
  labs(x = "Additional effect of logging
       <br>(logged forest estimate - old-growth forest estimate)",
       y = "Density")

pc <-
  sp_ests %>%
  ggplot(aes(x = value, y = Species,
             fill = forest_type)) +
  stat_halfeye(.width = 0.95, slab_alpha = 0.5, interval_size = 0.05,
               normalize = "groups", point_size = 0.25,
               point_interval = "median_qi") +
  theme(legend.position = "bottom") +
  facet_wrap(~name, scales = "free", nrow = 4) +
  scale_fill_manual(values = pal) +
  labs(x = "Estimate", y = "Species")

pd <-
  sp_ests %>%
  ungroup() %>%
  pivot_wider(names_from = forest_type,
              values_from = value) %>%
  mutate(diff = Logged - `Old-growth`) %>%
  ggplot(aes(x = diff, y = Species)) +
  stat_halfeye(.width = 0.95, slab_alpha = 0.5, interval_size = 0.05,
               normalize = "groups", point_size = 0.25,
               point_interval = "median_qi") +
  theme(legend.position = "bottom") +
  facet_wrap(~name, scales = "free", nrow = 4) +
  geom_vline(xintercept = 0, linetype = 2, colour = "red", linewidth = 0.25) +
  labs(x = "Additional effect of logging
       <br>(logged forest estimate - old-growth forest estimate)",
       y = "Species")


# Combine panels ----------------------------------------------------------

jpeg(
  here::here("output", "figures", "figure_04.jpeg"),
  width = 17,
  height = 24,
  res = 600,
  pointsize = 6,
  units = "cm",
  type = "cairo"
)
pa+pb+pc+pd+
  patchwork::plot_annotation(tag_levels = "a") +
  patchwork::plot_layout(guides = "collect",
                         heights = c(1, 2)) &
  theme_bw(base_size = 7) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_markdown(),
        axis.text.y = element_markdown(),
        axis.title.x = element_markdown(),
        strip.text = element_markdown(lineheight = 0.5),
        legend.text = element_text(size = 8))
dev.off()
