# Compare growth models
eleanorjackson
2025-03-23

Here I’m comparing models of different complexity (in terms of their
random effect structure).

``` r
library("tidyverse")
library("here")
library("patchwork")
library("brms")
library("broom.mixed")
```

``` r
file_names <- as.list(dir(path = here::here("output", "models", "priors2"),
                          full.names = TRUE))

model_list <- map(file_names, readRDS, environment())

names(model_list) <- lapply(file_names, basename)
```

Leave-one-out cross-validation (LOO-CV) is a popular method for
comparing Bayesian models based on their estimated predictive
performance on new/unseen data.

Expected log predictive density (ELPD): If new observations are
well-accounted by the posterior predictive distribution, then the
density of the posterior predictive distribution is high and so is its
logarithm. So higher ELPD = better predictive performance.

``` r
comp <- loo_compare(model_list$ft_sp_pl_lognorm_priors.rds,
                    model_list$ft_sp_co_lognorm_priors.rds,
                    model_list$ft_sp_cc_lognorm_priors.rds,
                    model_list$growth_model.rds,
                    criterion = "loo")

print(comp, digits = 3, simplify = FALSE)
```

                                           elpd_diff  se_diff    elpd_loo  
    model_list$ft_sp_co_lognorm_priors.rds      0.000      0.000 -30727.751
    model_list$ft_sp_pl_lognorm_priors.rds     -0.813     17.824 -30728.564
    model_list$growth_model.rds                -1.202     16.866 -30728.953
    model_list$ft_sp_cc_lognorm_priors.rds    -15.353     19.205 -30743.104
                                           se_elpd_loo p_loo      se_p_loo  
    model_list$ft_sp_co_lognorm_priors.rds    179.176    3035.759     83.262
    model_list$ft_sp_pl_lognorm_priors.rds    177.961    3023.100     82.189
    model_list$growth_model.rds               177.518    3023.337     81.680
    model_list$ft_sp_cc_lognorm_priors.rds    178.614    3054.369     84.959
                                           looic      se_looic  
    model_list$ft_sp_co_lognorm_priors.rds  61455.502    358.352
    model_list$ft_sp_pl_lognorm_priors.rds  61457.129    355.922
    model_list$growth_model.rds             61457.906    355.036
    model_list$ft_sp_cc_lognorm_priors.rds  61486.209    357.228

``` r
loo_compare(model_list$ft_sp_pl_lognorm_priors.rds,
            model_list$ft_sp_co_lognorm_priors.rds,
            model_list$ft_sp_cc_lognorm_priors.rds,
            model_list$growth_model.rds) %>% 
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

![](figures/2025-01-20_compare-growth-models/unnamed-chunk-4-1.png)

In the [loo package
documentation](https://mc-stan.org/loo/articles/online-only/faq.html#how-to-use-cross-validation-for-model-selection-)
they say:

> If elpd difference (`elpd_diff` in loo package) is less than 4, the
> difference is small [(Sivula, Magnusson and Vehtari,
> 2020)](https://doi.org/10.48550/arXiv.2008.10296)). If elpd difference
> is larger than 4, then compare that difference to standard error of
> `elpd_diff` (provided e.g. by loo package) [(Sivula, Magnusson and
> Vehtari, 2020)](https://doi.org/10.48550/arXiv.2008.10296).

``` r
loo_compare(model_list$ft_sp_pl_lognorm_priors.rds,
            model_list$ft_sp_co_lognorm_priors.rds,
            model_list$ft_sp_cc_lognorm_priors.rds,
            model_list$growth_model.rds,
            criterion = "loo") %>% 
  data.frame() %>% 
  rownames_to_column(var = "model_name") %>% 
  mutate(model_name = fct_reorder(model_name, p_loo, .desc = T)) %>% 
  ggplot(aes(y    = model_name, 
             x    = elpd_loo, 
             xmin = elpd_loo - se_elpd_loo, 
             xmax = elpd_loo + se_elpd_loo)) +
  geom_pointrange(shape = 21, fill = "white") +
  labs(y = "model", 
       x = "expected log predictive density (ELPD)") 
```

![](figures/2025-01-20_compare-growth-models/unnamed-chunk-5-1.png)

https://mc-stan.org/loo/reference/loo-glossary.html

## Compare parameter estimates

``` r
my_coef_tab <-
  tibble(fit = model_list[c(3:6)],
         model = names(model_list[c(3:6)])) %>%
  mutate(tidy = purrr::map(
    fit,
    tidy,
    parameters = c(
      "b_A_forest_typeprimary",
      "b_A_forest_typesecondary",
      "b_k_forest_typeprimary",
      "b_k_forest_typesecondary",
      "b_delay_forest_typeprimary",
      "b_delay_forest_typesecondary"
    )
  )) %>%
  unnest(tidy)
```

``` r
my_coef_tab %>% 
  rowwise() %>% 
  mutate(parameter = pluck(strsplit(term,"_"),1,2)) %>% 
  mutate(forest_type = pluck(strsplit(term,"_"),1,4)) %>% 
  mutate(forest_type = str_remove(forest_type, "type")) %>% 
  ggplot(aes(x = forest_type, 
             y = estimate, 
             ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(shape = 21, fill = "white") +
  labs(x = NULL,
       y = NULL) +
  coord_flip() +
  facet_grid(model~parameter, 
             scales = "free") 
```

![](figures/2025-01-20_compare-growth-models/unnamed-chunk-7-1.png)

These are the estimates of the posterior distribution with 95% credible
intervals based on quantiles.

``` r
my_vars <- c("b_A_forest_typeprimary", "b_A_forest_typesecondary",
             "b_delay_forest_typeprimary", "b_delay_forest_typesecondary",
             "b_k_forest_typeprimary", "b_k_forest_typesecondary")

my_regex <- paste0(my_vars, collapse="|")

draws <-
  map(model_list[c(3:6)],
           tidybayes::spread_draws, !!sym(my_regex), regex = TRUE) %>% 
  bind_rows(.id = "model") %>% 
  pivot_longer(cols = all_of(my_vars), 
               names_to = "var") %>% 
  rowwise() %>% 
  mutate(parameter = pluck(strsplit(var,"_"), 1, 2)) %>% 
  mutate(forest_type = pluck(strsplit(var,"_"),1,4)) %>% 
  mutate(forest_type = str_remove(forest_type, "type")) 

draws %>% 
  ggplot(aes(x = value, y = forest_type)) +
  tidybayes::stat_halfeye(normalize = "groups",
               point_interval = "mode_hdi") +
  facet_grid(model~parameter, scales = "free")
```

![](figures/2025-01-20_compare-growth-models/unnamed-chunk-8-1.png)

Here looking at the mode (point) and highest density interval (I think
sometimes prefered since they allow for skewed posterior distributions).

Adding cohort into the model seems to introduce a lot of uncertainty in
the estimates for *delay* and *k*, yet this model supposedly has the
best predictive performance? Do I need to give the chains longer to
converge?

### ft_sp_co_lognorm_priors

``` r
plot(model_list[[4]], 
       variable = "^b_*",
       ask = FALSE,
       regex = TRUE,
       nvariables = 6) 
```

![](figures/2025-01-20_compare-growth-models/unnamed-chunk-9-1.png)

Yeah.. not good

### ft_sp_co_lognorm_priors

``` r
plot(model_list[[3]], 
       variable = "^b_*",
       ask = FALSE,
       regex = TRUE,
       nvariables = 6) 
```

![](figures/2025-01-20_compare-growth-models/unnamed-chunk-10-1.png)

### cc_sp_lognorm_priors

This model is secondary forest only. Maybe a very small difference in
*A* and *k* in seedlings in climber cut plots.

``` r
plot(model_list[[1]], 
       variable = "^b_*",
       ask = FALSE,
       regex = TRUE,
       nvariables = 6) 
```

![](figures/2025-01-20_compare-growth-models/unnamed-chunk-11-1.png)

### co_sp_lognorm_priors

Also secondary forest only - looking at the effect of cohort. We can see
a small difference in *delay* between cohorts.

``` r
plot(model_list[[2]], 
       variable = "^b_*",
       ask = FALSE,
       regex = TRUE,
       nvariables = 6) 
```

![](figures/2025-01-20_compare-growth-models/unnamed-chunk-12-1.png)
