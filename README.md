# 2017aqrptR
Data and R code for graphs and analysis in 2017 Annual Air Quality Report for San Luis Obispo County.

## Details
The 2017 AQ Report is a product of the [San Luis Obispo County Air Pollution Control District](http://www.slocleanair.org/) and will be available on the District website [here](http://www.slocleanair.org/library/air-quality-reports.php) once finalized.

## Data Sources
Almost all data used in the report were downloaded from EPA's [Air Quality System (AQS)](https://www.epa.gov/aqs). These raw data downloads are provided as `.txt` files. Data from California Department of Parks and Recreation's "S1" meteorology tower were obtained from their data management system, and is provided in the `.csv` files begining `vdv_`. The data in Tables 3 and 4 are from AQS AMP440 and AMP450 reports; these are provided as `.pdf` files.

## Analyses and Figures
Scripts for reproducing the analyses and figures in the report are provided as `.R` files. `AQSloader.R` contains a single function for loading certain types files spit out by AQS. Most of the other scripts make use of this function. Otherwise, the different scripts are independent, i.e. `ozone.R` depends on having sourced `AQSloader.R` but not on having run any of the other scripts. 

## Dependencies
The following packages will be needed: `openair`, `dplyr`, `reshape2`, `crch`, `EnvStats`, and `RColorBrewer`. All are available on [CRAN](https://cran.r-project.org/).

### Session Info:
```
R version 3.4.4 (2018-03-15)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] crch_1.0-1     nlme_3.1-131.1 EnvStats_2.3.0 reshape2_1.4.3 bindrcpp_0.2.2 dplyr_0.7.4   
[7] openair_2.3-0 

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.16        pillar_1.2.1        compiler_3.4.4      RColorBrewer_1.1-2  plyr_1.8.4         
 [6] bindr_0.1.1         scoringRules_0.9.4  tools_3.4.4         digest_0.6.15       evaluate_0.10.1    
[11] lubridate_1.7.2     tibble_1.4.2        lattice_0.20-35     mgcv_1.8-23         pkgconfig_2.0.1    
[16] rlang_0.2.0         Matrix_1.2-12       cli_1.0.0           rstudioapi_0.7      mapproj_1.2.6      
[21] yaml_2.1.18         hexbin_1.27.2       knitr_1.20          stringr_1.3.0       cluster_2.0.6      
[26] maps_3.3.0          rprojroot_1.3-2     grid_3.4.4          glue_1.2.0          R6_2.2.2           
[31] rmarkdown_1.9       Formula_1.2-2       latticeExtra_0.6-28 purrr_0.2.4         tidyr_0.8.0        
[36] magrittr_1.5        backports_1.1.2     htmltools_0.3.6     MASS_7.3-49         rsconnect_0.8.8    
[41] assertthat_0.2.0    sandwich_2.4-0      utf8_1.1.3          stringi_1.1.7       crayon_1.3.4       
[46] zoo_1.8-1      
```