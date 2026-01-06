# `summary(growth model)`
eleanorjackson
2026-01-06

``` r
library("tidyverse")
library("insight")
```

``` r
mod_gro <-
  readRDS(here::here("output", "models",
                     "growth_model_base_p3_allo.rds"))
```

``` r
summary(mod_gro)
```

     Family: lognormal 
      Links: mu = identity 
    Formula: dbase_mean ~ log(A) * exp(-exp(-(k * (years - delay)))) 
             A ~ 0 + forest_type + (0 + forest_type | genus_species) + (1 | plant_id)
             k ~ 0 + forest_type + (0 + forest_type | genus_species) + (1 | plant_id)
             delay ~ 0 + forest_type + (0 + forest_type | genus_species) + (1 | plant_id)
       Data: data_sample (Number of observations: 21642) 
      Draws: 4 chains, each with iter = 10000; warmup = 5000; thin = 1;
             total post-warmup draws = 20000

    Multilevel Hyperparameters:
    ~genus_species (Number of levels: 15) 
                                                          Estimate Est.Error
    sd(A_forest_typelogged)                                  34.70      6.74
    sd(A_forest_typeprimary)                                 20.33      5.55
    sd(k_forest_typelogged)                                   0.03      0.01
    sd(k_forest_typeprimary)                                  0.09      0.02
    sd(delay_forest_typelogged)                               0.72      0.17
    sd(delay_forest_typeprimary)                              0.28      0.17
    cor(A_forest_typelogged,A_forest_typeprimary)             0.63      0.19
    cor(k_forest_typelogged,k_forest_typeprimary)             0.51      0.23
    cor(delay_forest_typelogged,delay_forest_typeprimary)     0.21      0.43
                                                          l-95% CI u-95% CI Rhat
    sd(A_forest_typelogged)                                  23.94    50.33 1.00
    sd(A_forest_typeprimary)                                 10.82    32.70 1.01
    sd(k_forest_typelogged)                                   0.02     0.05 1.00
    sd(k_forest_typeprimary)                                  0.05     0.14 1.00
    sd(delay_forest_typelogged)                               0.47     1.13 1.00
    sd(delay_forest_typeprimary)                              0.02     0.66 1.01
    cor(A_forest_typelogged,A_forest_typeprimary)             0.17     0.92 1.01
    cor(k_forest_typelogged,k_forest_typeprimary)            -0.02     0.86 1.01
    cor(delay_forest_typelogged,delay_forest_typeprimary)    -0.72     0.91 1.00
                                                          Bulk_ESS Tail_ESS
    sd(A_forest_typelogged)                                   5897    12877
    sd(A_forest_typeprimary)                                   902     4712
    sd(k_forest_typelogged)                                   5213    10806
    sd(k_forest_typeprimary)                                 11803    14535
    sd(delay_forest_typelogged)                              13719    13805
    sd(delay_forest_typeprimary)                              1281     4050
    cor(A_forest_typelogged,A_forest_typeprimary)             3457     4787
    cor(k_forest_typelogged,k_forest_typeprimary)             5777    12693
    cor(delay_forest_typelogged,delay_forest_typeprimary)     7119     7636

    ~plant_id (Number of levels: 3069) 
                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(A_Intercept)        51.38      1.24    49.03    53.92 1.07       34      117
    sd(k_Intercept)         0.09      0.00     0.09     0.09 1.00     2306     6450
    sd(delay_Intercept)     1.56      0.04     1.49     1.64 1.01     4458    10435

    Regression Coefficients:
                             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    A_forest_typelogged        102.46      9.41    84.03   121.04 1.00     5261
    A_forest_typeprimary        57.07      6.37    44.89    70.09 1.01     2961
    k_forest_typelogged          0.14      0.01     0.12     0.16 1.00     7525
    k_forest_typeprimary         0.39      0.03     0.34     0.43 1.01     6262
    delay_forest_typelogged     -1.40      0.20    -1.79    -1.01 1.00     9006
    delay_forest_typeprimary    -0.54      0.13    -0.80    -0.29 1.00     4857
                             Tail_ESS
    A_forest_typelogged          8377
    A_forest_typeprimary        10237
    k_forest_typelogged          9803
    k_forest_typeprimary        13649
    delay_forest_typelogged     11873
    delay_forest_typeprimary     9403

    Further Distributional Parameters:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sigma     0.17      0.00     0.17     0.17 1.03      108     7447

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

Andrew Gelman, Ben Goodrich, Jonah Gabry & Aki Vehtari. (2018).
R-squared for Bayesian regression models, The American Statistician.
[10.1080/00031305.2018.1549100](https://doi.org/10.1080/00031305.2018.1549100)

> The usual definition of R2 (variance of the predicted values divided
> by the variance of the data) has a problem for Bayesian fits, as the
> numerator can be larger than the denominator. We propose an
> alternative definition similar to one that has appeared in the
> survival analysis literature: the variance of the predicted values
> divided by the variance of predicted values plus the expected variance
> of the errors.

``` r
brms::bayes_R2(mod_gro)
```

`Error: vector memory limit of 16.0 Gb reached, see mem.maxVSize()`

`VarCorr()`

> This function calculates the estimated standard deviations,
> correlations and covariances of the group-level terms in a multilevel
> model of class brmsfit. For linear models, the residual standard
> deviations, correlations and covariances are also returned.

> Returns: A list of lists (one per grouping factor), each with three
> elements: a matrix containing the standard deviations, an array
> containing the correlation matrix, and an array containing the
> covariance matrix with variances on the diagonal.

``` r
brms::VarCorr(mod_gro)
```

    $genus_species
    $genus_species$sd
                                Estimate   Est.Error        Q2.5       Q97.5
    A_forest_typelogged      34.69838726 6.738610492 23.93609370 50.33465037
    A_forest_typeprimary     20.33437977 5.552208404 10.81590511 32.70285335
    k_forest_typelogged       0.03292049 0.008043703  0.02078623  0.05207655
    k_forest_typeprimary      0.08804132 0.021865635  0.05491212  0.13961957
    delay_forest_typelogged   0.72497065 0.169545502  0.47091215  1.13224671
    delay_forest_typeprimary  0.28263484 0.171230341  0.01726654  0.65859025

    $genus_species$cor
    , , A_forest_typelogged

                              Estimate Est.Error      Q2.5     Q97.5
    A_forest_typelogged      1.0000000 0.0000000 1.0000000 1.0000000
    A_forest_typeprimary     0.6257707 0.1946418 0.1713292 0.9177598
    k_forest_typelogged      0.0000000 0.0000000 0.0000000 0.0000000
    k_forest_typeprimary     0.0000000 0.0000000 0.0000000 0.0000000
    delay_forest_typelogged  0.0000000 0.0000000 0.0000000 0.0000000
    delay_forest_typeprimary 0.0000000 0.0000000 0.0000000 0.0000000

    , , A_forest_typeprimary

                              Estimate Est.Error      Q2.5     Q97.5
    A_forest_typelogged      0.6257707 0.1946418 0.1713292 0.9177598
    A_forest_typeprimary     1.0000000 0.0000000 1.0000000 1.0000000
    k_forest_typelogged      0.0000000 0.0000000 0.0000000 0.0000000
    k_forest_typeprimary     0.0000000 0.0000000 0.0000000 0.0000000
    delay_forest_typelogged  0.0000000 0.0000000 0.0000000 0.0000000
    delay_forest_typeprimary 0.0000000 0.0000000 0.0000000 0.0000000

    , , k_forest_typelogged

                              Estimate Est.Error        Q2.5     Q97.5
    A_forest_typelogged      0.0000000 0.0000000  0.00000000 0.0000000
    A_forest_typeprimary     0.0000000 0.0000000  0.00000000 0.0000000
    k_forest_typelogged      1.0000000 0.0000000  1.00000000 1.0000000
    k_forest_typeprimary     0.5097355 0.2308388 -0.02346917 0.8566668
    delay_forest_typelogged  0.0000000 0.0000000  0.00000000 0.0000000
    delay_forest_typeprimary 0.0000000 0.0000000  0.00000000 0.0000000

    , , k_forest_typeprimary

                              Estimate Est.Error        Q2.5     Q97.5
    A_forest_typelogged      0.0000000 0.0000000  0.00000000 0.0000000
    A_forest_typeprimary     0.0000000 0.0000000  0.00000000 0.0000000
    k_forest_typelogged      0.5097355 0.2308388 -0.02346917 0.8566668
    k_forest_typeprimary     1.0000000 0.0000000  1.00000000 1.0000000
    delay_forest_typelogged  0.0000000 0.0000000  0.00000000 0.0000000
    delay_forest_typeprimary 0.0000000 0.0000000  0.00000000 0.0000000

    , , delay_forest_typelogged

                              Estimate Est.Error      Q2.5     Q97.5
    A_forest_typelogged      0.0000000 0.0000000  0.000000 0.0000000
    A_forest_typeprimary     0.0000000 0.0000000  0.000000 0.0000000
    k_forest_typelogged      0.0000000 0.0000000  0.000000 0.0000000
    k_forest_typeprimary     0.0000000 0.0000000  0.000000 0.0000000
    delay_forest_typelogged  1.0000000 0.0000000  1.000000 1.0000000
    delay_forest_typeprimary 0.2139694 0.4299506 -0.721463 0.9147424

    , , delay_forest_typeprimary

                              Estimate Est.Error      Q2.5     Q97.5
    A_forest_typelogged      0.0000000 0.0000000  0.000000 0.0000000
    A_forest_typeprimary     0.0000000 0.0000000  0.000000 0.0000000
    k_forest_typelogged      0.0000000 0.0000000  0.000000 0.0000000
    k_forest_typeprimary     0.0000000 0.0000000  0.000000 0.0000000
    delay_forest_typelogged  0.2139694 0.4299506 -0.721463 0.9147424
    delay_forest_typeprimary 1.0000000 0.0000000  1.000000 1.0000000


    $genus_species$cov
    , , A_forest_typelogged

                              Estimate Est.Error      Q2.5    Q97.5
    A_forest_typelogged      1249.3847  515.1353 572.93658 2533.577
    A_forest_typeprimary      451.9514  243.8625  96.38667 1043.114
    k_forest_typelogged         0.0000    0.0000   0.00000    0.000
    k_forest_typeprimary        0.0000    0.0000   0.00000    0.000
    delay_forest_typelogged     0.0000    0.0000   0.00000    0.000
    delay_forest_typeprimary    0.0000    0.0000   0.00000    0.000

    , , A_forest_typeprimary

                             Estimate Est.Error      Q2.5    Q97.5
    A_forest_typelogged      451.9514  243.8625  96.38667 1043.114
    A_forest_typeprimary     444.3125  248.2013 116.98380 1069.477
    k_forest_typelogged        0.0000    0.0000   0.00000    0.000
    k_forest_typeprimary       0.0000    0.0000   0.00000    0.000
    delay_forest_typelogged    0.0000    0.0000   0.00000    0.000
    delay_forest_typeprimary   0.0000    0.0000   0.00000    0.000

    , , k_forest_typelogged

                                Estimate    Est.Error          Q2.5       Q97.5
    A_forest_typelogged      0.000000000 0.0000000000  0.000000e+00 0.000000000
    A_forest_typeprimary     0.000000000 0.0000000000  0.000000e+00 0.000000000
    k_forest_typelogged      0.001148457 0.0006168543  4.320672e-04 0.002711967
    k_forest_typeprimary     0.001561701 0.0011532773 -5.846246e-05 0.004430133
    delay_forest_typelogged  0.000000000 0.0000000000  0.000000e+00 0.000000000
    delay_forest_typeprimary 0.000000000 0.0000000000  0.000000e+00 0.000000000

    , , k_forest_typeprimary

                                Estimate   Est.Error          Q2.5       Q97.5
    A_forest_typelogged      0.000000000 0.000000000  0.000000e+00 0.000000000
    A_forest_typeprimary     0.000000000 0.000000000  0.000000e+00 0.000000000
    k_forest_typelogged      0.001561701 0.001153277 -5.846246e-05 0.004430133
    k_forest_typeprimary     0.008229356 0.004440337  3.015341e-03 0.019493625
    delay_forest_typelogged  0.000000000 0.000000000  0.000000e+00 0.000000000
    delay_forest_typeprimary 0.000000000 0.000000000  0.000000e+00 0.000000000

    , , delay_forest_typelogged

                               Estimate Est.Error       Q2.5     Q97.5
    A_forest_typelogged      0.00000000 0.0000000  0.0000000 0.0000000
    A_forest_typeprimary     0.00000000 0.0000000  0.0000000 0.0000000
    k_forest_typelogged      0.00000000 0.0000000  0.0000000 0.0000000
    k_forest_typeprimary     0.00000000 0.0000000  0.0000000 0.0000000
    delay_forest_typelogged  0.55432669 0.2821399  0.2217583 1.2819826
    delay_forest_typeprimary 0.04697971 0.1061197 -0.1372951 0.2829894

    , , delay_forest_typeprimary

                               Estimate Est.Error          Q2.5     Q97.5
    A_forest_typelogged      0.00000000 0.0000000  0.0000000000 0.0000000
    A_forest_typeprimary     0.00000000 0.0000000  0.0000000000 0.0000000
    k_forest_typelogged      0.00000000 0.0000000  0.0000000000 0.0000000
    k_forest_typeprimary     0.00000000 0.0000000  0.0000000000 0.0000000
    delay_forest_typelogged  0.04697971 0.1061197 -0.1372951266 0.2829894
    delay_forest_typeprimary 0.10920082 0.1237871  0.0002981335 0.4337411



    $plant_id
    $plant_id$sd
                       Estimate   Est.Error        Q2.5       Q97.5
    A_Intercept     51.38195010 1.242726475 49.02963894 53.91725080
    k_Intercept      0.09003655 0.002335893  0.08553681  0.09474038
    delay_Intercept  1.56413892 0.039392538  1.48934098  1.64298237


    $residual__
    $residual__$sd
      Estimate   Est.Error      Q2.5     Q97.5
     0.1717318 0.001099372 0.1695774 0.1739045
