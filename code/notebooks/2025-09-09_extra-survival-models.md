# Fit supplementary survival models
eleanorjackson
2025-09-09

``` r
library("tidyverse")
library("brms")
```

``` r
data <-
  readRDS(here::here("data", "derived", "data_survival.rds"))
```

``` r
priors3 <- c(
  prior(student_t(3, 0, 2.5), class = "b")
)
```

``` r
bform <-
  bf(
    time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~
      0 + forest_type + dbase_mean_sc + climber_cut +
      (0 + forest_type | genus_species),
    family = brmsfamily("weibull")
  )
```

``` r
survival_model_cc <-
  brm(bform,
      data = data,
      prior = priors3,
      sample_prior = "no",
      iter = 3000,
      cores = 4,
      chains = 4,
      seed = 123,
      init = 0,
      file_refit = "on_change",
      file = here::here("code",
                        "notebooks",
                        "models",
                        "2025-09-09_extra-survival-models",
                        "survival_climber_cut"))
```

``` r
survival_model_cc
```

     Family: weibull 
      Links: mu = log; shape = identity 
    Formula: time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~ 0 + forest_type + dbase_mean_sc + climber_cut + (0 + forest_type | genus_species) 
       Data: data (Number of observations: 4390) 
      Draws: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
             total post-warmup draws = 6000

    Multilevel Hyperparameters:
    ~genus_species (Number of levels: 15) 
                                              Estimate Est.Error l-95% CI u-95% CI
    sd(forest_typelogged)                         0.35      0.08     0.23     0.53
    sd(forest_typeprimary)                        0.62      0.17     0.36     1.00
    cor(forest_typelogged,forest_typeprimary)     0.81      0.15     0.40     0.99
                                              Rhat Bulk_ESS Tail_ESS
    sd(forest_typelogged)                     1.00     1533     2985
    sd(forest_typeprimary)                    1.00     2609     4098
    cor(forest_typelogged,forest_typeprimary) 1.00     2319     2726

    Regression Coefficients:
                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    forest_typelogged      1.10      0.10     0.90     1.30 1.00     1165     2118
    forest_typeprimary     1.74      0.19     1.37     2.12 1.00     1640     2840
    dbase_mean_sc          2.44      0.08     2.28     2.60 1.00     8599     4987
    climber_cut1           0.01      0.04    -0.07     0.10 1.00     9011     4526

    Further Distributional Parameters:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    shape     0.87      0.01     0.84     0.90 1.00     9069     4566

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
data_canopy <-
  readRDS(here::here("data", 
                     "derived", 
                     "data_survival_incl_understory.rds")) 
```

``` r
bform_canopy <-
  bf(
    time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~
      0  + canopy + dbase_mean_sc + 
      (0 + canopy | genus_species),
    family = brmsfamily("weibull")
  )
```

``` r
survival_model_cc <-
  brm(bform_canopy,
      data = data_canopy,
      prior = priors3,
      sample_prior = "no",
      iter = 3000,
      cores = 4,
      chains = 4,
      seed = 123,
      init = 0,
      file_refit = "on_change",
      file = here::here("code",
                        "notebooks",
                        "models",
                        "2025-09-09_extra-survival-models",
                        "survival_canopy"))
```

``` r
survival_model_cc
```

     Family: weibull 
      Links: mu = log; shape = identity 
    Formula: time_to_last_alive | cens(x = censor, y2 = time_to_dead) ~ 0 + canopy + dbase_mean_sc + (0 + canopy | genus_species) 
       Data: data_canopy (Number of observations: 4691) 
      Draws: 4 chains, each with iter = 3000; warmup = 1500; thin = 1;
             total post-warmup draws = 6000

    Multilevel Hyperparameters:
    ~genus_species (Number of levels: 15) 
                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    sd(canopyC)              0.33      0.07     0.22     0.49 1.00     1730
    sd(canopyG)              0.59      0.15     0.35     0.93 1.00     3179
    sd(canopyU)              0.89      0.20     0.59     1.39 1.00     2812
    cor(canopyC,canopyG)     0.76      0.17     0.32     0.97 1.00     3308
    cor(canopyC,canopyU)     0.68      0.18     0.24     0.92 1.00     2823
    cor(canopyG,canopyU)     0.63      0.22     0.09     0.94 1.00     1643
                         Tail_ESS
    sd(canopyC)              3060
    sd(canopyG)              4116
    sd(canopyU)              3057
    cor(canopyC,canopyG)     3746
    cor(canopyC,canopyU)     3811
    cor(canopyG,canopyU)     3860

    Regression Coefficients:
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    canopyC           1.08      0.09     0.90     1.27 1.00     1531     2521
    canopyG           1.71      0.18     1.36     2.08 1.00     2256     3269
    canopyU           1.42      0.24     0.95     1.91 1.00     2140     3217
    dbase_mean_sc     2.40      0.08     2.25     2.56 1.00     9880     4282

    Further Distributional Parameters:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    shape     0.89      0.01     0.86     0.92 1.00    10160     4291

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).
