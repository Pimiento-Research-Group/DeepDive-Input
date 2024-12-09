<!-- badges: start -->
[![R-CMD-check](https://github.com/DeepDive-project/DeepDiveR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DeepDive-project/DeepDiveR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/DeepDive-project/DeepDiveR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/DeepDive-project/DeepDiveR?branch=main)
<!-- badges: end -->

# DeepDiveR
DeepDiveR is an r package to prepare input for running DeepDive in Python. 

You can install DeepDiveR directly in an R console by using the devtools library as below. Make sure you have R version 4.4.1 or above installed on your computer:

```
library(devtools)
devtools::install_github("DeepDive-project/DeepDiveR")
library(DeepDiveR)
```

The functions in DeepDiveR assume input data to be organised by columns with a "Taxon", discrete "Area", "MinAge", "MaxAge" and "Locality" identifier for each fossil occurrence. 
Please carry out any extra cleaning steps, such as removal of duplicate occurrences, prior to use of the data preparation function.

You can read more about following functions using ?function_name_here.

1. Data preparation: prep_dd_input
Generates a .csv file containing information about the time bins, counts of localities per time bin per region and counts of occurrences per taxa through time in each region. If using age   replicates these are also saved in the same file. 

2. Create config file
create_config generates a .ini file of settings for analyses that will be executed in step 3. Setting modules to false will remove stages of analyses from the pipeline, by default the full pipeline will be run. For arguments see ?create_config.

Settings not included in the arguments for create_config can be updated using:
set_value(attribute_name = "parameter_you_want_to_set", value="value_here", module="module_where_parameter_is_stored", config)

To make areas appear through time you can provide age ranges as below:
area_ages <- rbind(c(max(bins), max(bins)),  # where each row represents a discrete sampling region
                   c(50, 40)))  
                   
Areas can also be made to disappear using label="end" in the following:
areas_matrix(area_ages, n_areas = length(unique(dat$Area)), config)
which adds ages to the config.

The config is saved using:
config$write(paste(path_dat, "config.ini", sep="/"))


4. Read the config file in python and launch analyses
