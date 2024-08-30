# Diameter conversion
eleanorjackson
2024-08-30

``` r
library("tidyverse")
library("here")
library("patchwork")
```

Some of the tree diameter measurements were taken at breast height
(1.3m), but others were taken at at base of the tree (5 cm from the
ground).

``` r
data <- 
  readRDS(here::here("data", "derived", "data_cleaned.rds"))
```

``` r
data %>% 
  ggplot(aes(x = dbase_mean)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~forest_type, scales = "free_y") +
  xlim(0, 50) +
  
  data %>% 
  ggplot(aes(x = dbh_mean)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~forest_type, scales = "free_y") +
  xlim(0, 50) +
  
  data %>% 
  ggplot(aes(x = height_apex)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~forest_type, scales = "free_y") +
  xlim(0, 500) +
  
  plot_layout(ncol = 1)
```

![](figures/2028-08-30_diameter-conversion/unnamed-chunk-2-1.png)

``` r
data %>% 
  filter(is.na(dbh_mean)) %>% 
  ggplot(aes(x = dbase_mean)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~forest_type, scales = "free_y") +
  xlim(0, 50) +
  
  data %>% 
  filter(is.na(dbase_mean)) %>% 
  ggplot(aes(x = dbh_mean)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~forest_type, scales = "free_y") +
  xlim(0, 50)  +
  
  plot_layout(ncol = 1) +
  plot_annotation(title = "Cases where either basal or BH diameter is NA")
```

![](figures/2028-08-30_diameter-conversion/unnamed-chunk-3-1.png)

From [Philipson *et al.* 2020](https://doi.org/10.1126/science.aay4490),
we can calculate diameter at breast height from our `dbase_mean`
measurements with this equation:

$$
diameter_{1.3m} = \frac{DBH_{POM}}{exp(-0.029(POM - 1.3))}
$$

They used this when the point of measurement (POM) had to be raised
above breast height (1.3m) due to buttress roots. I don’t know if this
applicable to our problem - our seedlings were too small for DBH to be
taken so the POM is below 1.3m.

Also.. I think this needs to be species specific.
