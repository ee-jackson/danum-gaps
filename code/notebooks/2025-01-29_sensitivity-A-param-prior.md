# Prior sensitivity for *A* in the growth models
eleanorjackson
2025-01-29

I’ve fit models with the full data using priors which [I selected
here](./code/notebooks/2025-01-24_test-A-param-prior.md).

I want to check the sensitivity of the pior -  
are the parameter estimates from these two models very different?

``` r
library("tidyverse")
library("here")
library("patchwork")
library("brms")
library("tidybayes")
library("modelr")
```

``` r
priors1 <-
  readRDS(here::here("output", "models", "priors1", "ft_sp_pl_lognorm_priors.rds"))


priors2 <-
  readRDS(here::here("output", "models", "priors2", "ft_sp_pl_lognorm_priors.rds"))

my_vars <- c("b_A_forest_typeprimary", "b_A_forest_typesecondary",
             "b_delay_forest_typeprimary", "b_delay_forest_typesecondary",
             "b_k_forest_typeprimary", "b_k_forest_typesecondary")

my_regex <- paste0(my_vars, collapse="|")
```

The `prior1` model uses a `lognormal(4, 1)` prior on the *A* parameter,
and the `prior2` model uses a wider `lognormal(5, 1.2)` prior on the *A*
parameter.

``` r
posterior_1 <-
  priors1 %>% 
  spread_draws(!!sym(my_regex), regex = TRUE) %>% 
  pivot_longer(cols = all_of(my_vars), 
               names_to = "var") %>% 
  mutate(prior = "prior_1") %>% 
  select(value, var, prior)

posterior_2 <-
  priors2 %>% 
  spread_draws(!!sym(my_regex), regex = TRUE) %>% 
  pivot_longer(cols = all_of(my_vars), 
               names_to = "var") %>% 
  mutate(prior = "prior_2") %>% 
  select(value, var, prior)
```

``` r
bind_rows(posterior_1, 
          posterior_2) %>% 
  rowwise() %>% 
  mutate(parameter = pluck(strsplit(var,"_"),1,2)) %>% 
  ggplot(aes(x = value, y = var)) +
  stat_halfeye(normalize = "groups",
               point_interval = "mode_hdi") +
  facet_grid(prior~parameter, scales = "free") +
  theme_bw()
```

![](figures/2025-01-29_sensitivity-A-param-prior/unnamed-chunk-4-1.png)

``` r
posterior_summary(priors1,
                  variable = my_vars)
```

                                   Estimate   Est.Error        Q2.5      Q97.5
    b_A_forest_typeprimary       62.8225802 10.89794423 41.28231701 84.7068092
    b_A_forest_typesecondary     82.1276242  7.34973294 67.85351751 96.9262241
    b_delay_forest_typeprimary    0.6937898  0.38167435 -0.05445186  1.4538287
    b_delay_forest_typesecondary  4.9462027  0.32875827  4.29332631  5.6014282
    b_k_forest_typeprimary        0.3163772  0.03297143  0.25474227  0.3838579
    b_k_forest_typesecondary      0.2136449  0.03193256  0.14625652  0.2754225

``` r
posterior_summary(priors2,
                  variable = my_vars)
```

                                   Estimate   Est.Error        Q2.5      Q97.5
    b_A_forest_typeprimary       64.4867308 10.58541199 43.85002234 86.5614183
    b_A_forest_typesecondary     83.2242381  7.29513389 69.13053755 97.6747760
    b_delay_forest_typeprimary    0.7069788  0.38079823 -0.06509532  1.4414730
    b_delay_forest_typesecondary  4.9565200  0.33139681  4.31238040  5.6207911
    b_k_forest_typeprimary        0.3135577  0.03281867  0.24857951  0.3828072
    b_k_forest_typesecondary      0.2162173  0.03421288  0.14749496  0.2858273

Look very similar to me!
