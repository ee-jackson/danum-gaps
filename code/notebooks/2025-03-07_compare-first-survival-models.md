# Compare survival models
eleanorjackson
2025-03-20

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
       mi() ~ mi(dbase_mean_sc) + forest_logged,
     family = brmsfamily("gaussian")) +
  bf(dbase_mean_sc |
       mi() ~ mi(dbh_mean_sc) + forest_logged,
     family = brmsfamily("gaussian")) +
  set_rescor(FALSE)
```

``` r
plot(model_list$ft_sp_sz_impute_weibull.rds,
     param = "b_", ask = FALSE)
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-7-1.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-7-2.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-7-3.png)

## Interaction

``` r
time_to_last_alive|cens(x = censor, y2 = time_to_dead) ~
        0 + forest_logged:dbase_mean_sc +
        (0 + forest_logged:dbase_mean_sc | genus_species)
```

``` r
plot(model_list$ft_sp_sz_interact2_weibull.rds,
     param = "b_", ask = FALSE)
```

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-9-1.png)

![](figures/2025-03-07_compare-first-survival-models/unnamed-chunk-9-2.png)
