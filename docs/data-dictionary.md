# data-dictionary

## Datasheets_Entry_31052024.xlsx
Raw data from the 2024 survey of the SBE. There are two tabs `1_64` and `65_124`, 
these correspond to the North and South blocks.

`Plot`, `Line` and `O_N` need to be concatenated to give a unique ID. 

If `Species` is `NA` it was a new seedling that died between initial planting and census in 2011.

|variable                           |definition                        |
|:----------------------------------|:---------------------------------|
|`Date` | date measurement taken |
|`Recordedby`| name of recorder |
|`Measuredby`| name of measurer |
|`Plot `| plot number |
|`Line`| line number |
|`Number`| same as `po` in `SBE_compiled_dataFull.csv` - planting point on each line |
|`O_N`| `O` (old) or `N` (new), was the seedling planted before the 1<sup>st</sup> or 2<sup>nd</sup> census? (O = Seedlings planted before 1<sup>st</sup> Census, N = Seedlings planted before 2<sup>nd</sup> census and after 1<sup>st</sup> census) |
|`Species`| Specific epithet, without genus. The 16 planted species have unique specific epithets. |
|`Survival`| 1 or 0 |
|`HgtOld`| Height measurement from the previous survey. Included to help fieldworks find individual trees. |
|`Height`| Only recorded if height is < 5m |
|`Diam1`| till be identical if dbh tape used, calliper up to 5 cm|
|`Diam2`| |
|`DBH1`| |
|`DBH2`| |
|`Comments`| - `Y` is used for bent and split stem seedlings <br/> - `X` is used for flat on the ground seedlings <br/> - `B` is used for seedlings with buttress <br/> - `R` is used for seedlings with ladder roots <br/> - `D` is used if there is damage at diameter at base  <br/> - `DB` is used if there is damage at DBH <br/> - `POMD=` is height of diameter measure if unable to get diameter at breast height <br/> - `POMB=` is height of DBH measure <br/> - `DX` is used if dead standing <br/> - `DY` is used if dead broken <br/> - `DZ` is used if dead missing <br/> - `patah` translates to broken |

## [SBE_compiled_dataFull.csv](https://zenodo.org/doi/10.5281/zenodo.10815814)
This dataset contains the data collected from a number of experiments that were conducted within the Sabah Biodiversity Experiment (SBE) project site located within the Malua Forest Reserve in Sabah, Malaysia between 2002 to 2020. The data collected are measurements taken from 16 species of Dipterocarpaceae seedlings that were planted within the project site. These measurements largely comprise of the diameters at base and breast height of the main stems (in centimeters), the height of the main stem (in centimeters), densiometer readings to determine canopy openness, and the level of herbivory experienced by seedlings.

|variable                           |definition                        |
|:----------------------------------|:---------------------------------|
|`pl` | Plot number |
|`li`| Line number within each plot |
|`po`| Planting point on each line |
|`location`| Plot location /  seedling tag |
|`plantingdate`| Date of planting |
|`surveydate`| Date of survey |
|`genus`| Genus of the seedling being measured |
|`species`| Species of the seedling being measured |
|`genus_species`| The genus and species of the seedling combined |
|`SpeciesMix`| Type of species mixture (refer to planting design booklet below) |
|`survival`| Did the seedling survive when measurements were being taken? Y or N  |
|`O.N`| Was the seedling planted before the 1<sup>st</sup> or 2<sup>nd</sup> census? (O = Seedlings planted before 1<sup>st</sup> Census, N = Seedlings planted before 2nd census and after 1<sup>st</sup> census) |
|`data_origin`| `full_measurement`, `intensive` or `climber`. The experiment or study that the measurement originated from |
|`sample`| The i<sup>th</sup> sample being measured for a given study  |
|`insectdamage`| Level of leaf damage inflicted by herbivorous insects |
|`mammaldamage`| Level of leaf damage inflicted by herbivorous mammals |
|`treefalldamage`| Type of shoot damage inflicted by falling trees or branches |
|`heightapex`| Measure to the tip of leader shoot (to nearest cm) |
|`diam1`| Measure diameter of stem base at 5cm from the ground (to nearest cm) |
|`diam2`| Same as `diam1` but at right-angle to the first measurement |
|`dbh1`| Measure diameter of stem at 130cm from the ground (to nearest cm) |
|`dbh2`| As with `dbh1`, but at right-angle to the first measurement |
|`DN`| Hemispherical densiometer measurements taken facing North |
|`DE`| Hemispherical densiometer measurement taken facing East |
|`DS`| Hemispherical densiometer measurement taken facing South |
|`DW`| Hemispherical densiometer measurement taken facing West |












