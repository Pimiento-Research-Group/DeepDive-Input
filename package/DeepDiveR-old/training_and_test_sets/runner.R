# Runner to generate:
# - A general training set with 0.5 probability extant
# - Test set with everything recently extinct.
# - Test set with everything extant.
library(devtools)

# Set the session wd to the location of DeepDiveR
DeepDiveR_path <- ""
setwd("DeepDiveR_path")

load_all(".")
library(DeepDiveR)
library(readxl)  # if your occurrence data is in an xl file
library(data.table)

path_wd <- "/Users/CooperR/Documents/GitHub/DeepDiveR/training_and_test_sets"

# Specify a vector of time bins
bins <- c(66, 65, 64, 63, 61.6, 60, 59.2, 58.13333, 57.06667, 56, 54.975, 53.95, 
          52.925, 51.9, 50.875, 49.85, 48.825, 47.8, 46.85714, 45.91429, 
          44.97143, 44.02857, 43.08571, 42.14286, 41.2, 40.03667, 38.87333, 
          37.71, 36.7575, 35.805, 34.8525, 33.9, 32.88667, 31.87333, 30.86, 
          29.84667, 28.83333, 27.82, 26.862, 25.904, 24.946, 23.988, 23.03, 
          22.16667, 21.30333, 20.44, 19.3225, 18.205, 17.0875, 15.97, 14.895, 
          13.82, 12.725, 11.63, 10.534, 9.438, 8.342, 7.246, 6.2895, 5.333, 
          4.4665, 3.6, 2.58, 1.8, 0.774, 0.129, 0.0117, 0)


# To make a general training set and train a model
config <- create_config(
  simulate = T,  model_training = T, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="general",
  n_areas = 5,
  simulations_file = "simulations_general", 
  add_test = TRUE, 
  autotune = FALSE,
  models_file = "trained_models_general", 
  include_present_diversity = TRUE,
  calibrate_diversity = FALSE
)

set_value(attribute_name = "pr_extant_clade", value=0.5, module="simulations", config)
config$write(paste(path_wd, "general.ini", sep="/")) # write config


# Test set extant - unconstrained
config <- create_config(
  simulate = T,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extant",
  n_areas = 5,
  simulations_file = "simulations_extant_u", 
  add_test = FALSE, 
  autotune = TRUE,
  include_present_diversity = FALSE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=1, module="simulations", config)
config$write(paste(path_wd, "extant_unconstrained.ini", sep="/")) # write config


# Test set extant - conditioned
config <- create_config(
  simulate = TRUE,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extant",
  n_areas = 5,
  simulations_file = "simulations_extant_con", 
  add_test = FALSE, 
  autotune = TRUE,
  include_present_diversity = TRUE,
  present_diversity = 100,  # extant species-level diversity
  calibrate_diversity = FALSE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=1, module="simulations", config)
config$write(paste(path_wd, "extant_condition.ini", sep="/")) # write config


# Test set extant - calibrate
config <- create_config(
  simulate = TRUE,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extant",
  n_areas = 5,
  simulations_file = "simulations_extant_cal", 
  add_test = FALSE, 
  autotune = TRUE,
  include_present_diversity = TRUE,
  present_diversity = 100,  # extant species-level diversity
  calibrate_diversity = TRUE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=1, module="simulations", config)
config$write(paste(path_wd, "extant_calibrate.ini", sep="/")) # write config


# Test set extinct - unconstrained
config <- create_config(
  simulate = TRUE,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extinct",
  n_areas = 5,
  simulations_file = "simulations_extinct_u", 
  add_test = FALSE, 
  autotune = TRUE,
  include_present_diversity = FALSE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=0, module="simulations", config)
config$write(paste(path_wd, "extinct_unconstrained.ini", sep="/")) # write config


# Test set extant - conditioned
config <- create_config(
  simulate = TRUE,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extinct",
  n_areas = 5,
  simulations_file = "simulations_extinct_con", 
  add_test = FALSE, 
  autotune = TRUE,
  include_present_diversity = TRUE,
  present_diversity = 0,  # extant species-level diversity
  calibrate_diversity = FALSE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=0, module="simulations", config)
config$write(paste(path_wd, "exinct_condition.ini", sep="/")) # write config


# Test set extant - calibrate
config <- create_config(
  simulate = TRUE,  model_training = F, empirical_predictions = F,
  path_wd = path_wd,  
  time_bins = bins,
  sim_name="extinct",
  n_areas = 5,
  simulations_file = "simulations_extinct_cal", 
  add_test = FALSE, 
  autotune = TRUE,
  include_present_diversity = TRUE,
  present_diversity = 0,  # extant species-level diversity
  calibrate_diversity = TRUE
)

set_value(attribute_name = "n_training_simulations", value=100, module="simulations", config)
set_value(attribute_name = "pr_extant_clade", value=0, module="simulations", config)
config$write(paste(path_wd, "extinct_calibrate.ini", sep="/")) # write config

