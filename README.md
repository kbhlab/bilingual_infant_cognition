**Repository specifications - Dal Ben et al. (2020)**

*author*: Hilary Killam and Rodrigo Dal Ben

*date*: September, 03, 2020

**Article**: Dal Ben, R., Killam, H., Iliaei, S. P., & Byers-Heinlein, K. (under review). Bilingualism affects infant cognition: Insights from new and open data.

This repository contain data, scripts, and analyses from two new datasets (Studies 1 & 3) and three reanalyzed open datasets (Studies 2a, 2b, 2c).

The reanalyzed datasets come from: 

*Study 2a*: 

D'Souza et al. (2020)

- article: https://doi.org/10.1098/rsos.180191 

- OSF: https://osf.io/53gh2/

- Dryad: https://doi.org/10.5061/dryad.3n5tb2rc6 

*Studies 2b & 2c*: 

Kalashnikova et al. (2020):

- article: https://doi.org/10.1111/desc.13011
- corrigendum: https://doi.org/10.1111/desc.13139

- OSF: https://osf.io/k3e9z/

## Reproducibility

This project uses the renv package to create a snapshot of the R and package versions used in preparing the analyses. To use renv, please first run the very first chunk in the file `analyses.Rmd` script to restore the saved renv lockfile. You will need to have rtools installed and on the PATH to install some of the packages. If this fails and you are not able to install or update all packages, please run the code `renv::deactivate()` and see the file `R_and_package_versions_for_reproducibility.txt` to manually download the packages required. This file lists all the package versions used in the project. Comment out the first chunk in the `analyses.Rmd` file and begin with chunk 2 instead.

### Directory structure:

The repository is organized into `/input`, `/output`, and `/scripts` subdirectories: 

* `/input`: contains the raw data for Studies 1 and 3 (our own newly collected data) and **step-by-step** guides to retrieve the raw data from D'Souza et al. (2020; [OSF](https://osf.io/53gh2/), [Dryad](https://doi.org/10.5061/dryad.3n5tb2rc6)) and Kalashnikova et al. (2020; [OSF](https://osf.io/k3e9z/)). The folder also contains  `.txt` codebooks for each raw dataset.

* `/output`: contains two subdirectories, `/data` and `/table_figure`
  
  * `/data`: contains tidy data for all studies (1, 2a, 2b, 2c, and 3). Each study has at least two datasets: *filtered* and *unfiltered*. Studies 1 and 3 have additional files (i.e., `exclusions_1_3.rda`, `data_tobii_cleaned_1_3.rda`, etc.)
  
  * `/table_figure`: contains analysis tables and figures for all Studies (1, 2a, 2b, 2c, and 3).

* `/scripts`: contains three wrangling scripts (`.r`) to transform the raw data (`/input` & OSF) into tidy data (`/output/data`): one for Studies 1 and 3, another for 2a, and another for 2b and 2c. It also contains the `analyses.Rmd` script to generate descriptive statistics, inferential statistics, and visualizations for all Studies.

### Feedback and suggestions: 
<kbh.coordinator@concordia.ca>, <dalbenwork@gmail.com>, <k.byers@concordia.ca>
