library(devtools)
load_all(".")
library(DeepDiveR)
library(readxl)  # if your occurrence data is in an xl file
library(data.table)  # needed to assign localities from coordinates

# load an occurrence table here
# your_dat <- read.csv("your_file_path")
path_dat <- "C:/Users/CooperR/Documents/carnivora_analysis"
dat <- read_xlsx(paste0(path_dat, "/carnivora_data_cleaned.xlsx"), sheet="Records")

dat <- dat[-which(dat$Maximum_Age=="Unknown"),]

# Organise your data in the expected format
# In this case some 2496 of 15882 records are not identified to the species 
# level, so here we will use genera
dat <- data.frame(dat$Genus, dat$Continent, 
                  as.numeric(dat$Minimum_Age), as.numeric(dat$Maximum_Age), 
                  dat$Latitude, dat$Longitude)
colnames(dat) <- c('Genus', 'Area', 'MinAge', 'MaxAge', 
                   'Latitude', 'Longitude')

# assign localities
setDT(dat)[, Locality := .GRP, by =.(Latitude, Longitude, MinAge, MaxAge)]

dat <- dat[!duplicated(dat), ]  # remove duplicated rows


# Specify a vector of time bins
bins <- c(max(dat$MaxAge), 65, 64, 63, 61.6, 60, 59.2, 58.13333, 57.06667, 56, 54.975, 53.95, 
          52.925, 51.9, 50.875, 49.85, 48.825, 47.8, 46.85714, 45.91429, 
          44.97143, 44.02857, 43.08571, 42.14286, 41.2, 40.03667, 38.87333, 
          37.71, 36.7575, 35.805, 34.8525, 33.9, 32.88667, 31.87333, 30.86, 
          29.84667, 28.83333, 27.82, 26.862, 25.904, 24.946, 23.988, 23.03, 
          22.16667, 21.30333, 20.44, 19.3225, 18.205, 17.0875, 15.97, 14.895, 
          13.82, 12.725, 11.63, 10.534, 9.438, 8.342, 7.246, 6.2895, 5.333, 
          4.4665, 3.6, 2.58, 0) # 1.8, 0.774, 0.129, 0.0117, 0)

dd_dataset <- paste(path_dat, "deepdive_input.csv", sep="/")

# Prepare input file for deepdive, set output_file name to save
prep_dd_input(dat = dat, bins = bins, r = 100, 
              age_m = "random_by_loc", taxon_level = "Genus", 
              output_file = dd_dataset)

# Check the number of extant taxa
summary_dat <- read_xlsx(paste0(path_dat, "/carnivora_data_cleaned.xlsx"), sheet="Species summary")
summary_dat <- summary_dat[-which(summary_dat$Minimum_age=="Unknown"),]
summary_dat <- summary_dat[-which(summary_dat$Status == "Extinct"),]
summary_dat <- data.frame(summary_dat$Genus, summary_dat$Status)
colnames(summary_dat) <- c("Genus", "Status")
summary_dat <- summary_dat[!duplicated(summary_dat), ]
length(summary_dat$Status == "Extant")


# Just the simulations
#config <- create_config(
#  simulate = T,  model_training = F, test_sim = F, empirical_predictions = F,
#  wd = paste0(getwd()),  
#  time_bins = bins,
#  sim_name="try1",
#  n_areas = length(unique(dat$Area)),
#  simulations_file = paste0(path_dat, "simulations_1"), 
#  add_test = F
#)
#config$write(paste0(path_dat,"try1.ini"))


# Just simulations, with test batch of simulations with the same settings as training data
#config <- create_config(
#  simulate = T,  model_training = F, test_sim = F, empirical_predictions = F,
#  wd = getwd(),  
#  time_bins = bins,
#  sim_name="try1.2",
#  n_areas = length(unique(dat$Area)),
#  simulations_file = paste0(path_dat, "simulations_2"), 
#  add_test = T
#)
#config$write(paste0(path_dat, "try1.2.ini"))


# Just the model_training
#config <- create_config(
#  simulate = F,  model_training = T, test_sim = F, empirical_predictions = F, 
#  wd = getwd(),
#  time_bins = bins,
#  simulations_file = paste0(path_dat, "simulations_1"), 
#  models_file = paste0(path_dat, "trained_models_1"), 
#  feature_file = paste0(path_dat, "simulations_1/try1_20240208_training_features.npy") , 
#  label_file = paste0(path_dat, "simulations_1/try1_20240208_training_labels.npy")
#)
#config$write(paste0(path_dat, "try2.ini"))


# Just the predictions
#config <- create_config(
#  simulate = F,  model_training = F, test_sim = F, empirical_predictions = T, 
#  wd = getwd(),
#  models_file = paste0(path_dat, "trained_models_1"), 
#  time_bins = bins,
#  include_present_diversity = T,
#  present_diversity = 300,
#  empirical_input_file = dd_dataset
#)
#config$write(paste0(path_dat, "try3.ini"))


# Generate config for the carnivores
# To do everything in one
#config <- create_config(
#  simulate = T,  model_training = T, empirical_predictions = T,
#  wd = path_dat,  
#  time_bins = bins,
#  sim_name="carnivora",
#  n_areas = length(unique(dat$Area)),
#  simulations_file = "simulations_carnivora", 
#  add_test = T, 
#  models_file = "trained_models_carnivora", 
#  include_present_diversity = FALSE,
#  present_diversity = 137,
#  taxonomic_level = "Genus",
#  empirical_input_file = dd_dataset
#)

# simulate training and test sets
config <- create_config(
  simulate = T,  model_training = F, empirical_predictions = F,
  wd = path_dat,  
  time_bins = bins,
  sim_name="carnivora",
  n_areas = length(unique(dat$Area)),
  simulations_file = "simulations_carnivora", 
  add_test = T, 
  include_present_diversity = FALSE,
  calibrate_diversity = FALSE,
  autotune=TRUE
)

# edit number of living taxa, present_diversity is 137
set_value(attribute_name = "extant_sp", value=c(13, 1300), module="simulations", config)

# edit total number of simulated taxa (minimum and maximum) 
set_value(attribute_name = "total_sp", value=c(618, 2000), module="simulations", config)

# edit carrying capacity in equilibrium simulations
set_value(attribute_name="dd_K", value=c(13, 1300), module="simulations", config)

#set_value(attribute_name = "p_mass_extinction", value = 0.001, module="simulations", config)
#set_value(attribute_name = "p_mass_speciation", value = 0.001, module="simulations", config)
#set_value(attribute_name ="p_constant_bd", value = 0.001, module="simulations", config)
#set_value(attribute_name = "p_equilibrium", value = 0.1, module="simulations", config)


# From the data, find when areas must have been occupied by (MaxAge for oldest
# fossil sampled per continent, in this case).
area_tables <- split(dat, f = dat$Area)  # Split data by area
Africa <- max(area_tables$Africa$MaxAge)
Asia <- max(area_tables$Asia$MaxAge)
Europe <- max(area_tables$Europe$MaxAge)
N_America <- max(area_tables$NorthAmerica$MaxAge)
S_America <- max(area_tables$SouthAmerica$MaxAge)


## SUGGESTION FOR THE ABOVE
area_ages <- rbind(c(max(bins), max(bins)),  # North America - these have narrower, 0.2ma range for the oldest
                   c(max(bins), max(bins)),  # Asia - these have very coarse ages at the oldest end, around 5my  ### VIA BERING LAND BRIDGE
                   c(61.6, 59.2),  # Africa - via arabian peninsula?
                   c(59.2, 56),  # Europe - why is there a temporal delay before they are sampled in Europe? barrier to dispersal we can time?
                   c(11.608, 7.3))  # South America - GABI, when is the oldest this could realistically happen? 


# if entered as area_ages = NULL, also provide bins as an argument.
# areas disappearing instead of connecting can be made via adding the argument
# label = "end"
areas_matrix(area_ages, n_areas = length(unique(dat$Area)), config)

config$write(paste(path_dat, "carnivora.ini", sep="/"))


##### Modern div, do nothing
config <- create_config(
  simulate = F,  model_training = T, empirical_predictions = T,
  wd = path_dat,  
  time_bins = bins,
  sim_name="carnivora",
  n_areas = length(unique(dat$Area)),
  autotune = T,
  simulations_file = "simulations_carnivora", 
  add_test = T, 
  models_file = "trained_models_carnivora", 
  include_present_diversity = FALSE,
  taxonomic_level = "Genus",
  empirical_input_file = dd_dataset
)

config$write(paste(path_dat, "carnivora_no_modern.ini", sep="/"))

##### Modern div, extra feature
config <- create_config(
  simulate = F,  model_training = T, empirical_predictions = T,
  wd = path_dat,  
  time_bins = bins,
  sim_name="carnivora",
  n_areas = length(unique(dat$Area)),
  autotune = T,
  simulations_file = "simulations_carnivora", 
  add_test = T, 
  models_file = "trained_models_carnivora", 
  include_present_diversity = TRUE,
  present_diversity=137, 
  taxonomic_level = "Genus",
  empirical_input_file = dd_dataset
)

config$write(paste(path_dat, "carnivora_mod_feature.ini", sep="/"))

##### Modern div, calibrate
config <- create_config(
  simulate = F,  model_training = T, empirical_predictions = T,
  wd = path_dat,  
  time_bins = bins,
  sim_name="carnivora",
  n_areas = length(unique(dat$Area)),
  autotune = T,
  simulations_file = "simulations_carnivora", 
  add_test = T, 
  models_file = "trained_models_carnivora", 
  include_present_diversity = TRUE,
  present_diversity=137, 
  calibrate_diversity = TRUE,
  taxonomic_level = "Genus",
  empirical_input_file = dd_dataset
)

config$write(paste(path_dat, "carnivora_mod_calibrate.ini", sep="/"))
