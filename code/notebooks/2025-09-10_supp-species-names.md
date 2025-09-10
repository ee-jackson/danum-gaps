# Check taxonomy for the supp info
eleanorjackson
2025-09-10

``` r
library("tidyverse")
library("WorldFlora") 
```

``` r
sp_data <- 
  readRDS(
  here::here(
    "data",
    "derived",
    "data_cleaned.rds"
    )
  ) %>% 
  distinct(genus_species) %>% 
  mutate(genus_species = as.character(genus_species)) %>% 
  mutate(genus_species = str_replace(genus_species, "_", " ")) %>% 
  as.data.frame()
```

World Flora Online Taxonomic Backbone v.2024.12 Taxonomic
classification.
[zenodo.14538251](https://doi.org/10.5281/zenodo.14538251)

Download the latest version:

``` r
WFO.download(
  WFO.url =
    paste0(
      "https://files.worldfloraonline.org/files/WFO_Backbone/",
      "_WFOCompleteBackbone/WFO_Backbone.zip"
    ),
  save.dir = here::here("data", "raw")
)
```

``` r
wfo <- 
  WFO.match(spec.data = sp_data,
            WFO.file = here::here("data", "raw", "classification.csv"),
            spec.name = "genus_species")
```

``` r
# finds one unique matching name for each submitted name
wfo_sp <- 
  WFO.one(wfo, priority = "Accepted") %>% 
  select(genus_species.ORIG,
         Old.ID,
         taxonID,
         scientificName,
         scientificNameAuthorship,
         family,
         subfamily,
         tribe,
         genus,
         specificEpithet) %>% 
  remove_rownames()
```

``` r
knitr::kable(wfo_sp)
```

| genus_species.ORIG | Old.ID | taxonID | scientificName | scientificNameAuthorship | family | subfamily | tribe | genus | specificEpithet |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Shorea leprosula | wfo-0000500626 | wfo-1000050975 | Rubroshorea leprosula | (Miq.) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Rubroshorea | leprosula |
| Shorea macroptera | wfo-0000500609 | wfo-1000050964 | Rubroshorea macroptera | (Dyer) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Rubroshorea | macroptera |
| Dryobalanops lanceolata |  | wfo-0000657521 | Dryobalanops lanceolata | Burck | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Dryobalanops | lanceolata |
| Shorea gibbosa | wfo-0000500579 | wfo-1000051019 | Richetia gibbosa | (Brandis) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Richetia | gibbosa |
| Shorea parvifolia | wfo-1000040285 | wfo-1000050977 | Rubroshorea ovata | (Dyer ex Brandis) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Rubroshorea | ovata |
| Shorea faguetiana | wfo-0000500424 | wfo-1000051017 | Richetia faguetiana | (F.Heim) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Richetia | faguetiana |
| Shorea beccariana | wfo-0000496674 | wfo-1000050992 | Rubroshorea beccariana | (Burck) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Rubroshorea | beccariana |
| Dipterocarpus conformis |  | wfo-0000651311 | Dipterocarpus conformis | Slooten | Dipterocarpaceae | Dipterocarpoideae | Dipterocarpeae | Dipterocarpus | conformis |
| Shorea macrophylla | wfo-0000500608 | wfo-1000050993 | Rubroshorea macrophylla | (de Vriese) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Rubroshorea | macrophylla |
| Parashorea malaanonan |  | wfo-0000396696 | Parashorea malaanonan | (Blanco) Merr. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Parashorea | malaanonan |
| Shorea argentifolia | wfo-0000500383 | wfo-1000050969 | Rubroshorea argentifolia | (Symington) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Rubroshorea | argentifolia |
| Shorea ovalis | wfo-0000500547 | wfo-1000051055 | Rubroshorea ovalis | (Korth.) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Rubroshorea | ovalis |
| Hopea sangal |  | wfo-0000724645 | Hopea sangal | Korth. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Hopea | sangal |
| Parashorea tomentella |  | wfo-0000396691 | Parashorea tomentella | (Symington) Meijer | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Parashorea | tomentella |
| Shorea johorensis | wfo-0000500593 | wfo-1000050945 | Rubroshorea johorensis | (Foxw.) P.S.Ashton & J.Heck. | Dipterocarpaceae | Dipterocarpoideae | Shoreae | Rubroshorea | johorensis |

``` r
write_csv(wfo_sp,
        here::here(
          "data",
          "derived",
          "taxonomy.csv"),)
```
