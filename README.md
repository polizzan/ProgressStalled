[![doi](https://img.shields.io/badge/DOI-10.17605/OSF.IO/H4AG3-blue)][doi]

[doi]: https://doi.org/10.17605/OSF.IO/H4AG3

# Progress Stalled? The Uncertain Future of Mortality in High-Income Countries

## Purpose of This Repository
The repository **ProgressStalled** was created to enable the replication of findings reported in:

*Progress Stalled? The Uncertain Future of Mortality in High-Income Countries*,

hereafter *our manuscript*.

## Repository Structure
The repository **ProgressStalled** contains two folders:

### 1. data
This folder stores all data files necessary to replicate our findings. 

We use age-group-, sex-, year-, and country-specific death and exposure counts, as well as estimates of life expectancy at birth by sex, year, and country, as provided in the [Human Mortality Database](https://mortality.org) (HMD). HMD data are stored in the three .zip files *deaths_d20240601*, *exposures_d20240601*, and *e0_per_d20240601*. These .zip files do **not** need to be unzipped before running the analysis file in the **scripts** folder (see below). 

HMD data are distributed under a [Creative Commons Attribution 4.0 International License][cc-by]. Please note that our reported findings are based on HMD data downloaded on 01 June 2024 and that data distributed by HMD may have been updated or revised in the meantime.

### 2. scripts
This folder contains the analysis file necessary to replicate our findings. 

The file `00_analysis.R` installs all `R` packages necessary to replicate our findings. It loads the HMD data and creates tables and plots of rates of mortality improvement and life expectancy at birth: Table 1, Figure 1, and Figure 2 in the main manuscript, as well as Figures A1 to A4 and Table T1 in the Appendix. The file `00_analysis.R` saves the tables in *.xlsx* format and saves the figures in *.svg* format in an automatically generated folder called **output**.  

## How to Use This Repository
In order to run the `R` code provided in the repository **ProgressStalled**, please proceed in the following order:

1. Download the repository from `github`. If applicable, unzip the downloaded folder and place it in a location convenient for you. 
2. Double click on the file `ProgressStalled.Rproj`. This should open `RStudio` on your machine.  
3. Within `RStudio`, click on `File`/`Open File...` and select the analysis file `00_analysis.R` located in the **scripts** folder.
4. You should now be able to run our code without adjusting any directories.

## License
This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
