# Functions for organising data in the correct format for deepdive
# Run functions in data_pipeline.R
#library(dplyr)
#library(data.table)
#library(tidyr)

# create_folders is possibly redundant with the new structure
create_folders <- function(name){
  dir.create(paste0(name, "empirical_data"))
  folder_names <- c("Species_occurrences", "Genus_occurrences", "Locality", "Info")
  for (i in 1:length(folder_names)){
    dir.create(paste0(name, "empirical_data/", folder_names[i]))
  }
}
