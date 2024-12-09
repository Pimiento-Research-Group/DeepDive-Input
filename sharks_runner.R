library(DeepDiveR)
library(readxl) 
library(tidyverse)
library(here)



# load necessary data -----------------------------------------------------

# area ages
dat_area <- read_xlsx(here("data","ocean_basin_age.xlsx"))

# number of extant species
dat_extant <- read_rds(here("data", 
                            "extant_species.rds")) %>% 
  mutate(taxon = str_to_lower(taxon))

# root ages of order
dat_root <- read_xlsx(here("data","root_ages_order.xlsx")) %>% 
  mutate(order = str_to_lower(order)) 


# load species data -------------------------------------------------------

# all species
dat_species <- read_xlsx(here("data","fins_filtered_species.xlsx")) %>% 
  rename("species" = "accepted_name")

# split into superorders
# selachii
dat_sela_species <- filter(dat_species, 
                         superorder %in% c("Galeomorphii", 
                                           "Squalomorphii"))
# batoidea
dat_bat_species <- filter(dat_species, superorder == "Batoidea")

# and into orders
# Carcharhiniformes
dat_carch_species <- filter(dat_species, order == "Carcharhiniformes")
# Hexanchiformes
dat_hexa_species <- filter(dat_species, order == "Hexanchiformes")
# Lamniformes
dat_lamni_species <- filter(dat_species, order == "Lamniformes")
# Myliobatiformes
dat_mylio_species <- filter(dat_species, order == "Myliobatiformes")
# Orectolobiformes
dat_orec_species <- filter(dat_species, order == "Orectolobiformes")
# Rajiformes
dat_raji_species <- filter(dat_species, order == "Rajiformes")
# Rhinopristiformes
dat_rhino_species <- filter(dat_species, order == "Rhinopristiformes")
# Squaliformes
dat_squali_species <- filter(dat_species, order == "Squaliformes")


# load genus data ---------------------------------------------------------

# genus level
dat_genus <- read_xlsx(here("data","fins_filtered_genus.xlsx"))

# split into superorders
# selachii
dat_sela_genus <- filter(dat_genus, 
                         superorder %in% c("Galeomorphii", 
                                           "Squalomorphii"))
# batoidea
dat_bat_genus <- filter(dat_genus, superorder == "Batoidea")

# and into orders
# Carcharhiniformes
dat_carch_genus <- filter(dat_genus, order == "Carcharhiniformes")
# Hexanchiformes
dat_hexa_genus <- filter(dat_genus, order == "Hexanchiformes")
# Lamniformes
dat_lamni_genus <- filter(dat_genus, order == "Lamniformes")
# Myliobatiformes
dat_mylio_genus <- filter(dat_genus, order == "Myliobatiformes")
# Orectolobiformes
dat_orec_genus <- filter(dat_genus, order == "Orectolobiformes")
# Rajiformes
dat_raji_genus <- filter(dat_genus, order == "Rajiformes")
# Rhinopristiformes
dat_rhino_genus <- filter(dat_genus, order == "Rhinopristiformes")
# Squaliformes
dat_squali_genus <- filter(dat_genus, order == "Squaliformes")


# prepare function --------------------------------------------------------


# set up function
prepare_data <- function(data_input, 
                         taxon_level, 
                         subgroup) {
  
  present_diversity <- dat_extant %>% 
    filter(taxon == subgroup) %>% 
    pull(paste("n", 
               taxon_level,
               "upd", 
               sep = "_"))
  
  dat <- data_input %>% 
    select(Taxon = taxon_level,
           Area = paleoocean, 
           MinAge = min_ma, 
           MaxAge = max_ma, 
           Locality = locality_id) %>% 
    mutate(across(contains("Age"), as.integer)) %>% 
    distinct() %>% 
    arrange(Area)
  
  
  # stages
  bins <- sort(c(max(dat$MaxAge), 
                 c(139.800,  132.600, 129.400, 125, 113.000, 100.500, 93.900, 
                   89.800, 86.300, 83.600, 72.100, 66.000, 61.600, 59.200, 56.000, 
                   47.800, 41.200, 37.710, 33.900, 27.820, 23.030, 20.440, 15.970, 
                   13.820, 11.630, 7.246, 5.333, 2.580), 
                 min(dat$MinAge)), 
                 decreasing = TRUE)
  
  
  dd_dataset <- here("data", taxon_level, 
                     paste0("deepdive_input_", 
                            subgroup, "_",
                            taxon_level, ".csv"))
  
  # Prepare input file for deepdive, set output_file name to save
  prep_dd_input(dat = dat, bins = bins, r = 100, 
                age_m = "random_by_loc", 
                output_file = dd_dataset)
  
  # define area ages
  area_ages_start <- dat %>% 
    distinct(Area) %>% 
    left_join(dat_area) %>% 
    select(-c(Area, End)) %>% 
    mutate(start = if_else(Start < 145, Start + 5, Start), 
           end = Start - 5) %>% 
    select(-Start) %>% 
    as.matrix.data.frame()
  
  area_ages_end <- dat %>% 
    distinct(Area) %>% 
    left_join(dat_area) %>% 
    select(-c(Area, Start)) %>% 
    mutate(end = if_else(End > 0, End - 5, End), 
           start = End) %>% 
    select(start, end) %>% 
    as.matrix.data.frame()
  
  # define root ages
  if(subgroup %in% c("myliobatiformes", "squaliformes")) {
    root_inp <- dat_root %>% 
      filter(order == subgroup) %>% 
      pivot_longer(cols = c(max_root, min_root)) %>% 
      pull(value)
    }
  
  
  # set up config
  config <- create_config(
    simulate = T,  model_training = T,
    empirical_predictions = T,
    path_wd = paste0("/home/matmat/data/deepdive/", 
                     subgroup, "_", taxon_level),  
    bins = bins,
    sim_name ="sharks",
    n_areas = length(unique(dat$Area)),
    simulations_file = "simulations_sharks", 
    add_test = T, 
    autotune = T,
    models_file = "trained_models_sharks", 
    present_diversity = present_diversity,  
    empirical_input_file = paste0("deepdive_input_", 
                                  subgroup, "_", 
                                  taxon_level, ".csv"), 
    output_file = "results"
  )
  
  # adjust settings
  # area
  areas_matrix(area_ages_start, n_areas = length(unique(dat$Area)), config, label="start")
  areas_matrix(area_ages_end, n_areas = length(unique(dat$Area)), config, label="end")
  
  # general
  set_value(attribute_name = "include_present_diversity", value=TRUE, module="general", config)  
  set_value(attribute_name = "calibrate_diversity", value=TRUE, module="general", config)
  # simulations
  set_value(attribute_name = "n_CPUS", value=70, module="simulations", config) 
  set_value(attribute_name = "n_training_simulations", value=format(100000, scientific = FALSE), module="simulations", config)
  set_value(attribute_name = "n_test_simulations", value=1000, module="simulations", config)
  
  if(subgroup %in% c("myliobatiformes", "squaliformes")) {
    set_value(attribute_name = "root_r", value=root_inp, module="simulations", config)  
  }
  
  set_value(attribute_name = "s_species", value=c(10, 100), module="simulations", config)
  set_value(attribute_name = "total_sp", value=c(1000, 25000), module="simulations", config)  
  set_value(attribute_name = "min_extant_sp", value=c(0, 1000), module="simulations", config)  
  set_value(attribute_name = "rangeL", value=c(0.01, 0.1), module="simulations", config)
  set_value(attribute_name = "rangeM", value=c(0.01, 0.1), module="simulations", config)
  set_value(attribute_name = "area_variance", value=c(0.01), module="simulations", config)
  set_value(attribute_name = "sp_mean", value=c(0.3, 1), module="simulations", config)
  set_value(attribute_name = "maximum_localities_per_bin", value=c(600), module="simulations", config)
  set_value(attribute_name = "pr_extant_clade", value=1, module="simulations", config)
  # empirical_predictions
  set_value(attribute_name = "scaling", value="1-mean", module="empirical_predictions", config)
  set_value(attribute_name = "present_diversity", value=present_diversity, module="empirical_predictions", config)
  set_value(attribute_name = "taxon_level", value=str_to_title(taxon_level), module="empirical_predictions", config)
  
  # write config
  config$write(here("data",
                    taxon_level,
                    paste0(subgroup, "_",
                           taxon_level, ".ini")))
  
}



# apply on species level --------------------------------------------------

# for species level
prepare_data(dat_species, "species", "all")

# superorder
prepare_data(dat_sela_species, "species", "selachii")
prepare_data(dat_bat_species, "species", "batoidea")


# order
prepare_data(dat_carch_species, "species", "carcharhiniformes")
prepare_data(dat_hexa_species, "species", "hexanchiformes")
prepare_data(dat_lamni_species, "species", "lamniformes")
prepare_data(dat_mylio_species, "species", "myliobatiformes")
prepare_data(dat_orec_species, "species", "orectolobiformes")
prepare_data(dat_raji_species, "species", "rajiformes")
prepare_data(dat_rhino_species, "species", "rhinopristiformes")
prepare_data(dat_squali_species, "species", "squaliformes")


# apply on genus level ----------------------------------------------------


# for genus level
prepare_data(dat_genus, "genus", "all")

# superorder
prepare_data(dat_sela_genus, "genus", "selachii")
prepare_data(dat_bat_genus, "genus", "batoidea")


# order
prepare_data(dat_carch_genus, "genus", "carcharhiniformes")
prepare_data(dat_hexa_genus, "genus", "hexanchiformes")
prepare_data(dat_lamni_genus, "genus", "lamniformes")
prepare_data(dat_mylio_genus, "genus", "myliobatiformes")
prepare_data(dat_orec_genus, "genus", "orectolobiformes")
prepare_data(dat_raji_genus, "genus", "rajiformes")
prepare_data(dat_rhino_genus, "genus", "rhinopristiformes")
prepare_data(dat_squali_genus, "genus", "squaliformes")



