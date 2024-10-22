# A summary of survival
eleanorjackson
2024-10-22

Here we will look at:

- Compare survival between forest types
- Compare survival between species
- Interactions of species & forest types
- Effect of climber cutting on survival

<details class="code-fold">
<summary>Code</summary>

``` r
library("tidyverse")
library("here")
library("patchwork")
library("survival")
library("ggsurvfit")
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
# reading in data and adding a column distinguishing cohorts 1 & 2 from
# the primary forest seedlings
data <- 
  readRDS(here::here("data", "derived", "data_cleaned.rds")) %>% 
  mutate(census_no = as.ordered(census_no)) %>% 
  mutate(cohort = paste(forest_type, old_new, sep = "_")) %>% 
  filter(! cohort == "secondary_NA") %>% 
  filter(! str_detect(plant_id, "NA")) 
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
data %>% 
  mutate(survival = as.factor(survival)) %>% 
  ggplot(aes(x = census_no, group = survival, 
             fill = survival)) +
  geom_bar(position = "fill") +
  facet_wrap(~cohort, ncol = 1,
             axis.labels = "all_x", axes = "all_x") +
  guides(x =  guide_axis(angle = 90)) +
  theme(legend.position = "top", legend.justification = "left") 
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-2-1.png)

I think we need to remove seedlings that were already dead at the time
of their first census. These seedlings are “left-censored”. Accounting
for left-censored data in models is tricky and it’s common to remove
them. Also - not sure how biologically relevant these points are -
likely it was moving from nursery to field that killed them?

First creating a “time” variable. I’m using days since first survey at
the individual plant level. I’ll also create a column which has the
units in years rather than days, as it might be easier to read (years
will still be continuous though e.g. 627 days = 1.717808 years).

<details class="code-fold">
<summary>Code</summary>

``` r
data <-
  data %>%
  group_by(plant_id) %>%
  slice_min(survey_date, with_ties = FALSE) %>%
  select(plant_id, survey_date) %>%
  rename(first_survey = survey_date) %>%
  ungroup() %>% 
  right_join(data)

data <-
  data %>%
  rowwise() %>% 
  mutate(
    days =
      survey_date - first_survey) %>% 
  ungroup() %>% 
  mutate(years = as.numeric(days, units= "weeks")/52.25,
         days_num = as.numeric(days))
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
left_censored <- 
  data %>% 
  filter(survival == 0 & days == 0) %>%  
  select(plant_id) %>% 
  distinct()

data %>% 
  filter(!plant_id %in% left_censored$plant_id) %>% 
  mutate(survival = as.factor(survival)) %>% 
  ggplot(aes(x = census_no, group = survival, 
             fill = survival)) +
  geom_bar(position = "fill") +
  facet_wrap(~cohort, ncol = 1,
             axis.labels = "all_x", axes = "all_x") +
  guides(x =  guide_axis(angle = 90)) +
  theme(legend.position = "top", legend.justification = "left") +
  geom_hline(yintercept = 0.5, colour = "white", linetype = 2) +
  geom_hline(yintercept = 0.75, colour = "white", linetype = 3) +
  geom_hline(yintercept = 0.25, colour = "white", linetype = 3)
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-4-1.png)

Above is plotted by census, but we can also plot by year. Note though
that some censuses stretch across year boundaries and not every plot was
surveyed every year, so I expect proportion surviving to bounce around a
bit due to the denominator not being constant.

I’ll also remove “climber” surveys for this figure since these only
surveyed two planting lines out of 20 for five of the SBE plots.

<details class="code-fold">
<summary>Code</summary>

``` r
data %>% 
  filter(!plant_id %in% left_censored$plant_id) %>% 
  filter(!str_detect(census_id, "climber")) %>% 
  mutate(survival = as.factor(survival)) %>% 
  mutate(year = year(survey_date)) %>% 
  ggplot(aes(x = year, group = survival, 
             fill = survival)) +
  geom_bar(position = "fill") +
  facet_wrap(~cohort, ncol = 1,
             axis.labels = "all_x", axes = "all_x") +
  theme(legend.position = "top", legend.justification = "left") +
  geom_hline(yintercept = 0.5, colour = "white", linetype = 2) +
  geom_hline(yintercept = 0.75, colour = "white", linetype = 3) +
  geom_hline(yintercept = 0.25, colour = "white", linetype = 3)
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-5-1.png)

<details class="code-fold">
<summary>Code</summary>

``` r
data %>% 
  filter(!plant_id %in% left_censored$plant_id) %>% 
  filter(!str_detect(census_id, "climber")) %>% 
  filter(survival == "0") %>%
  group_by(plant_id) %>%
  slice_min(lubridate::ymd(survey_date), with_ties = FALSE) %>%
  ungroup() %>% 
  ggplot(aes(x = years, 
             colour = cohort,
             group = cohort)) +
  geom_density(linewidth = 0.75) +
  ylab("Event density")
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-6-1.png)

Seedling deaths seem to happen fairly gradually over time in the primary
forest, for the secondary forest we see big spikes in deaths over time -
but only due to sampling effort.

Also looks like after 15 years, there are no deaths in the
2<sup>nd</sup> cohort of secondary forest seedlings, but this is because
we have no data past this point - it’s in the future!

Perhaps we can visualise events (deaths) a little differently:

<details class="code-fold">
<summary>Code</summary>

``` r
data %>% 
  filter(!plant_id %in% left_censored$plant_id) %>% 
  filter(!str_detect(census_id, "climber")) %>% 
  filter(survival == "0") %>%
  group_by(plant_id) %>%
  slice_min(lubridate::ymd(survey_date), with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(year = year(survey_date)) %>% 
  ggplot(aes(x = year, 
             fill = cohort,
             group = cohort)) +
  geom_bar() +
  facet_wrap(~cohort, ncol = 1, scales = "free_y") +
  labs(y = "# of new deaths")
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-7-1.png)

Patterns still obscured by *when* the surveys took place, i.e. spike in
mortality in 2010 in the secondary forest probably just because
seedlings hadn’t been censused for four years.

<details class="code-fold">
<summary>Code</summary>

``` r
data %>% 
  filter(!plant_id %in% left_censored$plant_id) %>% 
  filter(case_when(
    forest_type == "primary" & census_no == 22 ~ TRUE,
    forest_type == "secondary" & census_no == 26 ~ TRUE
  )) %>% 
  group_by(cohort, genus_species) %>% 
  summarise(n_dead = sum(survival == "0"), 
            n_alive = sum(survival == "1")) %>% 
  mutate(prop_alive = n_alive / (n_dead + n_alive)) %>% 
  select(- n_dead, - n_alive) %>% 
  pivot_wider(names_from = cohort, values_from = prop_alive) %>% 
  arrange(desc(primary_NA)) %>% 
  knitr::kable(digits = 2)
```

</details>

| genus_species           | primary_NA | secondary_N | secondary_O |
|:------------------------|-----------:|------------:|------------:|
| Hopea_sangal            |       0.79 |        0.39 |        0.35 |
| Dryobalanops_lanceolata |       0.71 |        0.31 |        0.19 |
| Shorea_beccariana       |       0.71 |        0.33 |        0.27 |
| Parashorea_malaanonan   |       0.67 |        0.30 |        0.22 |
| Shorea_macroptera       |       0.62 |        0.32 |        0.10 |
| Shorea_gibbosa          |       0.57 |        0.21 |        0.16 |
| Dipterocarpus_conformis |       0.50 |          NA |        0.29 |
| Shorea_macrophylla      |       0.43 |        0.23 |        0.17 |
| Parashorea_tomentella   |       0.40 |        0.35 |        0.19 |
| Shorea_johorensis       |       0.36 |        0.39 |        0.19 |
| Shorea_ovalis           |       0.36 |        0.27 |        0.28 |
| Shorea_argentifolia     |       0.29 |        0.13 |        0.02 |
| Shorea_faguetiana       |       0.29 |        0.07 |        0.10 |
| Shorea_parvifolia       |       0.25 |        0.16 |        0.11 |
| Shorea_leprosula        |       0.08 |        0.16 |        0.10 |

Table shows proportion alive in the most recent census by species and
forest type.

## Survival curves

I want to fit some survival curves - to do this we need to sort out the
censoring.

A common problem with continuous time to event data is that we often
don’t know when exactly an event happened - a seedling was alive in one
census and dead in the next but could have died at any point between
those two censuses.

This is what we call interval censored data. You can also get right
censored data (e.g. seedling still alive at the end of the study), and
left censored data (seedling already dead at the start of the study,
which we filtered out above).

<details class="code-fold">
<summary>Code</summary>

``` r
interval_censored <-
  data %>% 
  filter(survival == 0) %>% 
  group_by(plant_id) %>% 
  slice_min(survey_date, with_ties = FALSE) %>% 
  ungroup() %>% 
  rename(time_to_dead = years) %>% 
  select(plant_id, genus_species, plot, cohort, time_to_dead) %>% 
  mutate(censor = "interval") 


interval_censored <-
  data %>% 
  filter(plant_id %in% interval_censored$plant_id) %>% 
  filter(survival == 1) %>% 
  group_by(plant_id) %>% 
  slice_max(survey_date, with_ties = FALSE) %>% 
  ungroup() %>% 
  rename(time_to_last_alive = years) %>% 
  select(plant_id, time_to_last_alive) %>% 
  right_join(interval_censored) 
  
  
right_censored <- 
  data %>% 
  filter(!plant_id %in% interval_censored$plant_id) %>% 
  group_by(plant_id) %>% 
  slice_max(survey_date, with_ties = FALSE) %>% 
  ungroup() %>% 
  rename(time_to_last_alive = years) %>% 
  select(plant_id, genus_species, plot, cohort, time_to_last_alive) %>% 
  mutate(censor = "right")

data_aggregated <- 
  bind_rows(interval_censored, right_censored) %>% 
  filter(time_to_last_alive > 0) %>% 
  mutate(plot_id = as.factor(paste(cohort, plot, sep = "_")))

data_aggregated <- 
  data_aggregated %>% 
  mutate(time_to_dead = case_when(
    censor == "right" ~ time_to_last_alive,
    .default = time_to_dead
  )) 
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
data_aggregated %>% 
  slice_sample(n = 30) %>%
  pivot_longer(cols = c(time_to_last_alive, time_to_dead),
               values_to = "years") %>% 
  ggplot(aes(y = plant_id, x = years, colour = name, group = plant_id)) +
  geom_path(colour = "lightgrey") +
  geom_point(alpha = 0.5, size = 2) +
  theme(legend.position = "top")
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-10-1.png)

Seedlings with only one data point are right censored and seedlings with
two data points are interval censored.

Censoring is built into survival models by incorporating it into the
likelihood function underlying the analysis.

The survival probability at a certain time is a conditional probability
of surviving beyond that time, given that an individual has survived
just prior to that time.

The [Kaplan-Meier
estimate](https://en.wikipedia.org/wiki/Kaplan–Meier_estimator) of
survival probability at a given time is the product of these conditional
probabilities up until that given time.

We can fit a survival curve and create a K-M plot using the `{survival}`
package.

<details class="code-fold">
<summary>Code</summary>

``` r
# different packages want censors formatted in different ways
data_surv <- 
  data_aggregated %>% 
  mutate(event = recode(censor, "interval" = 3, "right" = 0, "left" = 2)) %>% 
  mutate(time_to_dead = case_when(
    censor == "right" ~ NA,
    .default = time_to_dead
  ))

fit_surv <- 
  survfit(data = data_surv,
          Surv(time = time_to_last_alive, 
               time2 = time_to_dead,
               type = "interval2") ~ cohort)
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
fit_surv %>% 
  ggsurvfit::ggsurvfit(theme = theme_classic(base_size = 10)) +
  labs(
    x = "Years",
    y = "Survival probability"
  ) + 
  add_confidence_interval() +
  ggtitle("Kaplan-Meier plot")
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-12-1.png)

<details class="code-fold">
<summary>Code</summary>

``` r
fit_surv_sp <- 
  survfit(data = data_surv,
          Surv(time = time_to_last_alive, 
               time2 = time_to_dead,
               type = "interval2") ~ genus_species)
```

</details>

I’ve removed the CI’s for species else you can’t see anything.

<details class="code-fold">
<summary>Code</summary>

``` r
fit_surv_sp %>% 
  ggsurvfit::ggsurvfit(theme = theme_classic(base_size = 10)) +
  labs(
    x = "Years",
    y = "Survival probability"
  ) + 
  scale_colour_viridis_d() +
  ggtitle("Kaplan-Meier plot")
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-14-1.png)

<details class="code-fold">
<summary>Code</summary>

``` r
fit_surv_sp_p <- 
  survfit(data = filter(data_surv, cohort == "primary_NA"),
          Surv(time = time_to_last_alive, 
               time2 = time_to_dead,
               type = "interval2") ~ genus_species)

fit_surv_sp_s <- 
  survfit(data = filter(data_surv, str_detect(cohort, "secondary")),
          Surv(time = time_to_last_alive, 
               time2 = time_to_dead,
               type = "interval2") ~ genus_species)
```

</details>
<details class="code-fold">
<summary>Code</summary>

``` r
fit_surv_sp_p %>% 
  ggsurvfit::ggsurvfit(theme = theme_classic(base_size = 10)) +
  labs(
    x = "Years",
    y = "Survival probability"
  ) + 
  scale_colour_viridis_d() +
  ggtitle("Primary forest") +
  
  fit_surv_sp_s %>% 
  ggsurvfit::ggsurvfit(theme = theme_classic(base_size = 10)) +
  labs(
    x = "Years",
    y = "Survival probability"
  ) + 
  scale_colour_viridis_d() +
  ggtitle("Secondary forest") +
  
  plot_layout(guides = "collect", ncol = 1)
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-16-1.png)

It’s hard to see what’s going on with so many colours.

## Binomial survival

We can also plot survival as a binomial.

<details class="code-fold">
<summary>Code</summary>

``` r
p <- ggplot() +
  geom_smooth(data = filter(data, !plant_id %in% left_censored$plant_id), 
              aes(x = days_num, y = survival), 
              method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~forest_type)

p +
  ggdist::geom_dots(data = filter(data, !plant_id %in% left_censored$plant_id), 
                    aes(y = as.numeric(survival), x = days_num, 
                        side = ifelse(survival == 0, "bottom", "top")), 
                    color = "grey20", binwidth = 150, overflow = "compress", 
                    shape = 16, alpha = 0.3, size = 0.5) 
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-17-1.png)

<details class="code-fold">
<summary>Code</summary>

``` r
ggplot(data = filter(data, !plant_id %in% left_censored$plant_id)) +
  geom_smooth(aes(x = years, 
                  y = survival, 
                  colour = cohort), 
              method = "glm", method.args = list(family = "binomial")) 
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-18-1.png)

The shape of survival looks similar in the two forest types, but higher
for primary forest seedlings.

<details class="code-fold">
<summary>Code</summary>

``` r
data %>%  
  filter(!plant_id %in% left_censored$plant_id) %>% 
  ggplot() +
  geom_smooth(aes(survival, x = years, 
                  colour = genus_species), 
              se = FALSE, linewidth = 0.7,
              method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~forest_type) +
  scale_colour_viridis_d() 
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-19-1.png)

<details class="code-fold">
<summary>Code</summary>

``` r
data %>%  
  filter(!plant_id %in% left_censored$plant_id) %>% 
  ggplot(aes(survival, x = years, 
                  colour = cohort)) +
  geom_smooth(se = FALSE, linewidth = 0.7,
              method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~genus_species, ncol = 5)  +
  ggtitle("geom_smooth(method = `glm`)") +
  
  data %>%  
  filter(!plant_id %in% left_censored$plant_id) %>% 
  ggplot(aes(survival, x = years, 
                  colour = cohort)) +
  geom_smooth(se = FALSE, linewidth = 0.7,
              method = "gam", method.args = list(family = "binomial")) +
  facet_wrap(~genus_species, ncol = 5)  +
  ggtitle("geom_smooth(method = `gam`)") +
  
  plot_layout(guides = "collect", ncol = 1) &
  theme(legend.position = "bottom", legend.justification = "left") 
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-20-1.png)

This is quite interesting, survival always higher for primary forest
seedlings.

## Climber-cut vs non-climber cut intensively sampled plots

<details class="code-fold">
<summary>Code</summary>

``` r
data %>% 
  filter(forest_type == "secondary") %>% 
  mutate(climber_cut = ifelse(
    plot %in% c("05", "11", "14"),
    TRUE, FALSE
  )) %>% 
  filter(!plant_id %in% left_censored$plant_id) %>% 
  filter(census_no == 26) %>% 
  group_by(climber_cut, genus_species) %>% 
  summarise(n_dead = sum(survival == "0"), 
            n_alive = sum(survival == "1")) %>%  
  mutate(prop_alive = n_alive / (n_dead + n_alive)) %>% 
  select(- n_dead, - n_alive) %>% 
  pivot_wider(names_from = climber_cut, values_from = prop_alive) %>% 
  rename(climbers_cut = `TRUE`, climbers_not_cut = `FALSE`) %>% 
  arrange(desc(climbers_cut)) %>% 
  knitr::kable(digits = 2)
```

</details>

| genus_species           | climbers_not_cut | climbers_cut |
|:------------------------|-----------------:|-------------:|
| Hopea_sangal            |             0.36 |         0.36 |
| Shorea_johorensis       |             0.29 |         0.32 |
| Dipterocarpus_conformis |             0.26 |         0.32 |
| Dryobalanops_lanceolata |             0.27 |         0.28 |
| Parashorea_tomentella   |             0.29 |         0.28 |
| Shorea_beccariana       |             0.28 |         0.28 |
| Shorea_ovalis           |             0.29 |         0.27 |
| Parashorea_malaanonan   |             0.29 |         0.25 |
| Shorea_macroptera       |             0.24 |         0.23 |
| Shorea_macrophylla      |             0.17 |         0.21 |
| Shorea_gibbosa          |             0.18 |         0.18 |
| Shorea_leprosula        |             0.09 |         0.17 |
| Shorea_parvifolia       |             0.12 |         0.15 |
| Shorea_faguetiana       |             0.06 |         0.12 |
| Shorea_argentifolia     |             0.04 |         0.12 |

Table shows proportion alive in the most recent census in the secondary
forest.

<details class="code-fold">
<summary>Code</summary>

``` r
data %>%  
  filter(forest_type == "secondary") %>% 
  mutate(climber_cut = ifelse(
    plot %in% c("05", "11", "14"),
    TRUE, FALSE
  )) %>% 
  filter(!plant_id %in% left_censored$plant_id) %>% 
  ggplot() +
  geom_smooth(aes(survival, x = years, 
                  colour = climber_cut), 
              linewidth = 0.7,
              method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~genus_species, ncol = 5) +
  theme(legend.position = "bottom")
```

</details>

![](figures/2024-10-11_describe-survival/unnamed-chunk-22-1.png)
