# TRY traits
eleanorjackson
2025-07-17

Which of our species are in the TRY database?

``` r
library("tidyverse")
library("tidybayes")
```

``` r
try_sp <- 
  read_tsv(here::here("data", "raw", "TryAccSpecies.txt")) %>% 
  mutate(genus_species = str_replace(AccSpeciesName, " ", "_")) 
```

``` r
our_sp <- readRDS(here::here("data", "derived", "data_cleaned.rds")) %>% 
  select(genus_species) %>% 
  distinct()

our_sp
```

    # A tibble: 15 × 1
       genus_species          
       <fct>                  
     1 Shorea_leprosula       
     2 Shorea_macroptera      
     3 Dryobalanops_lanceolata
     4 Shorea_gibbosa         
     5 Shorea_parvifolia      
     6 Shorea_faguetiana      
     7 Shorea_beccariana      
     8 Dipterocarpus_conformis
     9 Shorea_macrophylla     
    10 Parashorea_malaanonan  
    11 Shorea_argentifolia    
    12 Shorea_ovalis          
    13 Hopea_sangal           
    14 Parashorea_tomentella  
    15 Shorea_johorensis      

``` r
inner_join(our_sp, try_sp) %>% glimpse
```

    Rows: 14
    Columns: 8
    $ genus_species  <chr> "Shorea_leprosula", "Shorea_macroptera", "Dryobalanops_…
    $ AccSpeciesID   <dbl> 49981, 49988, 19393, 49960, 50006, 49952, 49933, 18749,…
    $ AccSpeciesName <chr> "Shorea leprosula", "Shorea macroptera", "Dryobalanops …
    $ ObsNum         <dbl> 235, 143, 81, 52, 102, 59, 101, 27, 62, 175, 58, 153, 4…
    $ ObsGRNum       <dbl> 22, 80, 31, 9, 56, 11, 37, NA, 6, 62, 8, 81, 1, 16
    $ MeasNum        <dbl> 415, 567, 399, 94, 530, 145, 565, 47, 155, 426, 156, 79…
    $ MeasGRNum      <dbl> 143, 450, 294, 32, 461, 72, 400, NA, 57, 282, 82, 676, …
    $ TraitNum       <dbl> 103, 64, 51, 26, 53, 46, 65, 11, 34, 51, 27, 54, 13, 21

One of our species is not in the TRY database: `Parashorea_tomentella`

``` r
inner_join(our_sp, try_sp) %>% 
  pull(AccSpeciesID) 
```

     [1] 49981 49988 19393 49960 50006 49952 49933 18749 49987 40272 49927 50001
    [13] 29483 49973

the `AccSpeciesID`’s we want:

49981, 49988, 19393, 49960, 50006, 49952, 49933, 18749, 49987, 40272,
49927, 50001, 29483, 49973

and the trait IDs:

- 4083 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) of
  total leaf area  
- 3086 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA)
  petiole, rhachis and midrib excluded
- 3115 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA):
  petiole excluded  
- 3116 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA):
  petiole included  
- 3117 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA):
  undefined if petiole is in- or excluded
- 4 Stem specific density (SSD, stem dry mass per stem fresh volume) or
  wood density
