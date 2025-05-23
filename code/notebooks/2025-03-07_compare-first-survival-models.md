# Compare survival models
eleanorjackson
2025-04-03

- [Censor data](#censor-data)
- [Simple model](#simple-model)
- [Imputing missing values](#imputing-missing-values)
- [Interaction model](#interaction-model)
- [Compare all models](#compare-all-models)
- [Comparing simple models](#comparing-simple-models)
- [Comparing impute models](#comparing-impute-models)

``` r
library("tidyverse")
library("brms")
library("patchwork")
library("broom.mixed")
library("tidybayes")
library("modelr")
```

``` r
file_names <- as.list(dir(path = here::here("output", "models", "survival"),
                          full.names = TRUE))

model_list <- map(file_names, readRDS, environment())

names(model_list) <- lapply(file_names, basename)
```

## Censor data

``` r
data <-
  readRDS(here::here("data", "derived", "data_cleaned.rds"))

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
```

## Simple model

With random slope for forest type allowed for species:

``` r
time_to_last_alive|cens(x = censor, y2 = time_to_dead) ~
        0 + forest_logged + dbase_mean_sc +
        (0 + forest_logged|genus_species)
```

``` r
plot(model_list$ft_sp_sz_weibull.rds,
     param = "b_", ask = FALSE)
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-5-1.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-5-2.png)

With random slope for size allowed for species:

``` r
time_to_last_alive|cens(x = censor, y2 = time_to_dead) ~
        0 + forest_logged + dbase_mean_sc +
        (0 + forest_logged + dbase_mean_sc|genus_species)
```

``` r
plot(model_list$ft_sp_sz_weibull2.rds,
     param = "b_", ask = FALSE)
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-7-1.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-7-2.png)

With random intercept for species:

``` r
time_to_last_alive|cens(x = censor, y2 = time_to_dead) ~
        0 + forest_logged + dbase_mean_sc +
        (1 | genus_species)
```

``` r
plot(model_list$ft_sp_sz_weibull3.rds,
     param = "b_", ask = FALSE)
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-9-1.png)

## Imputing missing values

``` r
bform <-
  bf(
    time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~
      0 + forest_logged + mi(dbase_mean_sc) + mi(dbh_mean_sc) +
      (0 + forest_logged | genus_species),
    family = brmsfamily("weibull")
  ) +
  bf(dbh_mean_sc |
       mi() ~ mi(dbase_mean_sc),
     family = brmsfamily("gaussian")) +
  bf(dbase_mean_sc |
       mi() ~ mi(dbh_mean_sc),
     family = brmsfamily("gaussian")) +
  set_rescor(FALSE)
```

``` r
plot(model_list$ft_sp_sz_impute_weibull.rds,
     param = "b_", ask = FALSE)
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-11-1.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-11-2.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-11-3.png)

With species random effect explaining size:

``` r
bform <-
  bf(
    time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~
      0 + forest_logged + mi(dbase_mean_sc) + mi(dbh_mean_sc) +
      (0 + forest_logged  | genus_species),
    family = brmsfamily("weibull")
  ) +
  bf(dbh_mean_sc |
       mi() ~ mi(dbase_mean_sc) + (1 | genus_species),
     family = brmsfamily("gaussian")) +
  bf(dbase_mean_sc |
       mi() ~ mi(dbh_mean_sc) + (1 | genus_species),
     family = brmsfamily("gaussian")) +
  set_rescor(FALSE)
```

``` r
plot(model_list$ft_sp_sz_impute_weibull2.rds,
     param = "b_", ask = FALSE)
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-13-1.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-13-2.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-13-3.png)

With species as a fixed effect explaining size:

``` r
bform3 <-
  bf(
    time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~
      0 + forest_logged + mi(dbase_mean_sc) + mi(dbh_mean_sc) +
      (0 + forest_logged | genus_species),
    family = brmsfamily("weibull")
  ) +
  bf(dbh_mean_sc |
       mi() ~ mi(dbase_mean_sc) + genus_species,
     family = brmsfamily("gaussian")) +
  bf(dbase_mean_sc |
       mi() ~ mi(dbh_mean_sc) + genus_species,
     family = brmsfamily("gaussian")) +
  set_rescor(FALSE)
```

``` r
plot(model_list$ft_sp_sz_impute_weibull3.rds,
     param = "b_", ask = FALSE)
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-15-1.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-15-2.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-15-3.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-15-4.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-15-5.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-15-6.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-15-7.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-15-8.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-15-9.png)

## Interaction model

``` r
time_to_last_alive|cens(x = censor, y2 = time_to_dead) ~
        0 + forest_logged:dbase_mean_sc +
        (0 + forest_logged:dbase_mean_sc | genus_species)
```

``` r
plot(model_list$ft_sp_sz_interact2_weibull.rds,
     param = "b_", ask = FALSE)
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-17-1.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-17-2.png)

## Compare all models

``` r
loo_compare(
  model_list$ft_sp_sz_weibull.rds,
  model_list$ft_sp_sz_weibull2.rds,
  model_list$ft_sp_sz_weibull3.rds,
  model_list$ft_sp_sz_impute_weibull.rds,
  model_list$ft_sp_sz_impute_weibull2.rds,
  model_list$ft_sp_sz_impute_weibull3.rds,
  model_list$ft_sp_sz_interact2_weibull.rds
) %>% 
  data.frame() %>% 
  rownames_to_column(var = "model_name") %>% 
  ggplot(aes(x    = reorder(model_name, elpd_diff), 
             y    = elpd_diff, 
             ymin = elpd_diff - se_diff, 
             ymax = elpd_diff + se_diff)) +
  geom_pointrange(shape = 21, fill = "white") +
  coord_flip() +
  geom_hline(yintercept = 0, colour = "blue", linetype = 2) +
  labs(x = NULL, y = "difference from model with the largest ELPD", 
       title = "expected log predictive density (ELPD)") 
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-18-1.png)

``` r
my_coef_tab <-
  tibble(fit = model_list[c(1,2,3,6,7,8)],
         model = names(model_list[c(1,2,3,6,7,8)])) %>%
  mutate(tidy = purrr::map(
    fit,
    tidy,
    parameters = c(
      "forest_logged0",
      "forest_logged1",
      "dbase_mean_sc",
      "timetolastalive_midbase_mean_sc",
      "timetolastalive_midbh_mean_sc"
    )
  )) %>%
  unnest(tidy)
```

``` r
my_coef_tab %>% 
  filter(term == "b_forest_logged0" |
           term == "b_forest_logged1" |
           term == "b_timetolastalive_forest_logged0" |
           term == "b_timetolastalive_forest_logged1" 
           ) %>% 
  rowwise() %>% 
  mutate(parameter = pluck(strsplit(term,"_"),1,-1)) %>% 
  ggplot(aes(x = parameter, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(shape = 21, fill = "white") +
  labs(x = NULL,
       y = NULL) +
  coord_flip() +
  facet_wrap(~model, ncol = 1) 
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-20-1.png)

``` r
my_coef_tab %>% 
  filter(term == "b_dbase_mean_sc" |
           term == "bsp_timetolastalive_midbase_mean_sc"|
           term == "bsp_timetolastalive_midbh_mean_sc"
           ) %>% 
  rowwise() %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(shape = 21, fill = "white") +
  labs(x = NULL,
       y = NULL) +
  coord_flip() +
  facet_wrap(~model, ncol = 1) 
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-21-1.png)

## Comparing simple models

``` r
loo_compare(
  model_list$ft_sp_sz_weibull.rds,
  model_list$ft_sp_sz_weibull2.rds,
  model_list$ft_sp_sz_weibull3.rds
) %>% 
  data.frame() %>% 
  rownames_to_column(var = "model_name") %>% 
  ggplot(aes(x    = reorder(model_name, elpd_diff), 
             y    = elpd_diff, 
             ymin = elpd_diff - se_diff, 
             ymax = elpd_diff + se_diff)) +
  geom_pointrange(shape = 21, fill = "white") +
  coord_flip() +
  geom_hline(yintercept = 0, colour = "blue", linetype = 2) +
  labs(x = NULL, y = "difference from model with the largest ELPD", 
       title = "expected log predictive density (ELPD)") 
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-22-1.png)

## Comparing impute models

``` r
loo_compare(
  model_list$ft_sp_sz_impute_weibull.rds,
  model_list$ft_sp_sz_impute_weibull2.rds,
  model_list$ft_sp_sz_impute_weibull3.rds
) %>% 
  data.frame() %>% 
  rownames_to_column(var = "model_name") %>% 
  ggplot(aes(x    = reorder(model_name, elpd_diff), 
             y    = elpd_diff, 
             ymin = elpd_diff - se_diff, 
             ymax = elpd_diff + se_diff)) +
  geom_pointrange(shape = 21, fill = "white") +
  coord_flip() +
  geom_hline(yintercept = 0, colour = "blue", linetype = 2) +
  labs(x = NULL, y = "difference from model with the largest ELPD", 
       title = "expected log predictive density (ELPD)") 
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-23-1.png)

``` r
my_coef_tab <-
  tibble(fit = model_list[c(1,2,3)],
         model = names(model_list[c(1,2,3)])) %>%
  mutate(tidy = purrr::map(
    fit,
    tidy,
    parameters = c(
      "bsp_dbhmeansc_midbase_mean_sc",
      "bsp_dbasemeansc_midbh_mean_sc",
      "bsp_timetolastalive_midbase_mean_sc",
      "bsp_timetolastalive_midbh_mean_sc",
      "b_dbhmeansc_Intercept",
      "b_dbasemeansc_Intercept"
    )
  )) %>%
  unnest(tidy)
```

``` r
my_coef_tab %>% 
  rowwise() %>% 
  ggplot(aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange(shape = 21, fill = "white") +
  labs(x = NULL,
       y = NULL) +
  facet_wrap(~model, ncol = 1) 
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-25-1.png)
