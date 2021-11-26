---
title: "Step-by-step: raw data from Study 1c and 1d - Kalashnikova et al. (2020)"
author: "Hilary Killam and Rodrigo Dal Ben"
date: "September, 03, 2020"
output: html_document
---

**Article**: Dal Ben, R., Killam, H., Iliaei, S. P., & Byers-Heinlein, K. (under review). Bilingualism affects infant cognition: Evidence from new and reanalyzed data.

**OSF**: https://osf.io/bz8jn/

Step-by-step instructions to retrieve the raw data from Kalashnikova et al. (2020).

Reference for Kalashnikova et al. (2020) materials:

- article: https://doi.org/10.1111/desc.13011
- corrigendum: https://doi.org/10.1111/desc.13139

- OSF: https://osf.io/k3e9z/

### Summary: 

Tidy datasets with sample-by-sample data and demographic information (available at the `/output/data` directory) were created from Kalashnikova et al.'s (2020) raw datasets available on their OSF (https://osf.io/k3e9z/) using the `wrangling_2b_2c.R` script (see `/scripts`).

### Raw data: step-by-step

The **Version 1** of the following raw files, available on Kalashnikova et al. (2020) OSF repository (https://osf.io/k3e9z/) as of **2020/04/14**, are necessary:

* [Metadata.xlsx](https://osf.io/6mfbu/)
* [auditory_left_FA.csv](https://osf.io/zq6s2/)
* [auditory_right_FA.csv](https://osf.io/uwb9z/)
* [visual_left_FA.csv](https://osf.io/yk2qg/)
* [visual_right_FA.csv](https://osf.io/fqsdh/)

### Tidy data

After downloading these files, add them to the `/input` subdirectory and run the `wrangling_2b_2c.R` script. This should generate four tidy datasets, available in the `/output/data` subdirectory:

* `data_full_trials_filtered_2b.Rda`: containing 41 participants included in the final analysis (main article);
	
* `data_full_trials_unfiltered_2b.Rda`: containing 70 participants included in the additional analysis (supplementary material);
	
* `data_full_trials_filtered_2c.Rda`: containing 44 participants included in the final analysis (main article);
	
* `data_full_trials_unfiltered_2c.Rda`: containing 67 participants included in the additional analysis (supplementary material);
	

### Feedback and suggestions: 
<kbh.coordinator@concordia.ca>, <dalbenwork@gmail.com>