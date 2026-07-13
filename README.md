# danum-gaps

This repository contains the [research compendium](https://research-compendium.science) 
for our in-prep manuscript: 
Dynamic seedling growth and survival trajectories in logged and old-growth tropical forest over two decades.

Contact: eleanor.elizabeth.j@gmail.com

## Article abstract

1. Enrichment planting is a common method of restoration for selectively logged forests, particularly in Southeast Asia, where depleted populations of commercially valuable dipterocarp species are supplemented with nursery-grown seedlings. However, the success of these interventions has been mixed and studies often report poor seedling survival. It is possible that the logged forest presents a lower quality environment for dipterocarp seedlings, leading to recruitment failure, but rarely has the growth and survival of enrichment-planted seedlings been compared to seedlings planted in an old growth forest. 
2. We compare the growth and survival of over 3,000 dipterocarp seedlings of 15 species planted in logged forest versus canopy gaps in unlogged old-growth forest over a 20-year period using non-linear models. 
3. Survival probability was initially lower for seedlings in the logged forest, but converged with increasing size, such that large seedlings in both forest types had similar probabilities of survival after 20 years. 
4. Growth trajectories differed markedly between forest types: seedlings in old-growth forest canopy gaps reached maximum relative growth rates 3.5 years earlier and approximately 6% year-1 higher than seedlings in the logged forest, which exhibited slower early growth and delayed growth onset. However, after 20 years, seedling size was similar between the two forest types, as growth rates in the old-growth forest declined to levels lower than those observed in the logged forest.  
5. More open conditions, including maintenance of planting lines in the logged forest may have created a relatively stable light environment with reduced competition relative to canopy gaps in the old-growth forest, potentially explaining the documented patterns of growth and survival. 
6. Our results demonstrate that short-term assessments risk misrepresenting the longer-term outcomes of restoration interventions and highlight the importance of size-dependent and trajectory-based approaches for understanding seedling performance in modified tropical forests. Although survival probability and growth rates were initially lower in the logged forest, surviving seedlings did not experience a persistently elevated risk of mortality and reached sizes comparable to seedlings in the old-growth forest canopy gaps. Findings from our study system do not support concerns about severe seedling recruitment failure in logged forests over the longer-term.


## Contents:

### [`code/`](code/)
The [`code/`](code/) directory contains these subdirectories:

[`notebooks/`](code/notebooks/) contains Quarto files that were used for exploratory analysis and note-taking. 
Notebooks are not intended to be reproducible but the `.md` files can be viewed as rendered html (with output) on GitHub.

[`scripts/`](code/scripts/) contains action scripts, i.e. all the code for cleaning, combining, and analysing the data. All paths in the scripts are relative to the root directory (where the `.Rproj` file lives). Each `.R` script has a summary at the top of what it does. The scripts are numbered in the order in which they would typically be run.

### `data/`
The original data is stored in the `data/raw/` subdirectory. Any data that is produced using code is stored in `data/derived/`. Data are archived separately, and `scripts/` files contain code to download the data.

### [`output/`](output/)
The [`output/`](output/) directory contains the subdirectories [`figures/`](output/figures/) and [`results/`](output/results/), which contain the figures used in the paper and other output from analyses, respectively.

### [`docs/`](docs/)
The [`docs/`](docs/) directory contains the [data dictionary](docs/data-dictionary.md) (i.e. metadata), [protocols](docs/protocols.md) and any other relevant documents.

## Usage
To reproduce results and figures from this project in the [RStudio IDE](https://posit.co/download/rstudio-desktop/), 
first open the `.Rproj` file and call `renv::restore()` to restore the project's R package library. 
Then, run the `.R` scripts in [`code/scripts/`](code/scripts/) in the order in which they are labelled.
The scripts will download the required data files to reproduce the project.

## License
Code is under a [MIT license](LICENSE.md)
