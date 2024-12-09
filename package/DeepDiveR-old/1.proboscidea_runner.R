library(devtools)
load_all(".")
library(DeepDiveR)

# load an occurrence table here
# your_dat <- read.csv("your_file_path")
path_dat <- "R/test_empirical_data/elephant_analysis/"
dat <- read_xlsx(paste0(path_dat, "/Proboscidea_occurrences_def_Nov.2020.xlsx"))


# Organise your data in the expected format
dat <- data.frame(dat$species_complete_corrected, dat$species_complete_corrected,
                  dat$continent, dat$MIN_AGE_homog, dat$MAX_AGE_homog, dat$NAME)
colnames(dat) <- c('Complete_name', 'Genus_species', 'Area', 'MinAge', 'MaxAge', 
                   'Locality')

dat <- dat %>% separate(Genus_species, c('Genus', 'Species'), sep=" ")

# re-code geography
setDT(dat)[, Locality := .GRP, by = Locality]
dat$Area <- str_replace_all(dat$Area, c("Northern Africa" = "Africa",
                                        "Eastern Africa" = "Africa",
                                        "Western Africa" = "Africa",
                                        "Southern Africa" = "Africa",
                                        "Middle Africa" = "Africa",
                                        "Eastern Asia" = "Asia", 
                                        "South-Eastern Asia" = "Asia",
                                        "Western Asia" = "Asia",
                                        "Southern Asia" = "Asia",
                                        "South-Asia" = "Asia",
                                        "Central Asia" = "Asia",
                                        "Northern Europe" = "Europe", 
                                        "Eastern Europe" = "Europe", 
                                        "Western Europe" = "Europe", 
                                        "Southern Europe" = "Europe",
                                        "Northern America" = "North America",
                                        "Central America" = "North America"))

dat <- dat[!duplicated(dat), ]

# Specify a vector of time bins
bins <- c(66, 65, 64, 63, 61.6, 60, 59.2, 58.13333, 57.06667, 56, 54.975, 53.95, 
          52.925, 51.9, 50.875, 49.85, 48.825, 47.8, 46.85714, 45.91429, 
          44.97143, 44.02857, 43.08571, 42.14286, 41.2, 40.03667, 38.87333, 
          37.71, 36.7575, 35.805, 34.8525, 33.9, 32.88667, 31.87333, 30.86, 
          29.84667, 28.83333, 27.82, 26.862, 25.904, 24.946, 23.988, 23.03, 
          22.16667, 21.30333, 20.44, 19.3225, 18.205, 17.0875, 15.97, 14.895, 
          13.82, 12.725, 11.63, 10.534, 9.438, 8.342, 7.246, 6.2895, 5.333, 
          4.4665, 3.6, 2.58, 1.8, 0.774, 0.129, 0.0117, 0.0082, 0.0042, 0)

# Prepare input file for deepdive, set output_file name to save
prep_dd_input(dat = dat, bins = bins, r = 100, 
              age_m = "random_by_loc", taxon_level = "Species", 
              output_file = paste0(path_dat, ""))


# Create config file with settings to run analysis
#create_config(simulations=T, n_CPUS=20, p_mass_extinction=0.01,
#              model_training=T, test=T, empirical_predictions=T)


# Just the simulations
config <- create_config(
  simulate = T,  model_training = F, test_sim = F, empirical_predictions = F,
  wd = paste0(getwd(), path_dat),  
  time_bins = bins,
  sim_name="try1",
  n_areas = length(unique(dat$Area)),
  simulations_file = "simulations_1", 
  add_test = FALSE
)
config$write("/Users/CooperR/Documents/GitHub/DeepDiveR/try1.ini")


# Just simulations, with test batch of simulations with the same settings as training data
config <- create_config(
  simulate = T,  model_training = F, test_sim = F, empirical_predictions = F,
  wd = getwd(),  
  time_bins = bins,
  sim_name="try1",
  n_areas = length(unique(dat$Area)),
  simulations_file = "simulations_2", 
  add_test = FALSE
)
config$write("/Users/CooperR/Documents/GitHub/DeepDiveR/try1.2.ini")



# Just the model_training
config <- create_config(
  simulate = FALSE,  model_training = TRUE, test_sim = FALSE,
  empirical_predictions = FALSE, wd = "/Users/CooperR/Documents/GitHub/deep_dive",
  simulations_file = "simulations", models_file = "trained_models", 
  feature_file = "/Users/CooperR/Documents/GitHub/deep_dive/simulations/try1_20240119_training_features.npy", 
  label_file = "/Users/CooperR/Documents/GitHub/deep_dive/simulations/try1_20240119_training_labels.npy"
)
config$write("/Users/CooperR/Documents/GitHub/DeepDiveR/try2.ini")


# Just the predictions
config <- create_config(
  simulate = FALSE,  model_training = FALSE, test_sim = FALSE,
  empirical_predictions = TRUE, wd = "/Users/CooperR/Documents/GitHub/deep_dive",
  models_file = "trained_models", time_bins = c(0, 5, 10, 15, 20),
  empirical_input_file = "/Users/CooperR/Documents/GitHub/DeepDiveR/R/test_empirical_data"
)

config$write("/Users/CooperR/Documents/GitHub/DeepDiveR/try3.ini")



config <- create_config(simulate = T, model_training = T, 
                        test_sim = T, empirical_predictions = FALSE, 
                        time_bins = c(0, 5, 10, 15, 20), 
                        n_areas = 5, simulations_file = "simulations", 
                        # feature_files = paste('sim_features20220617.npy', collapse=""),
                        # label_files = 'sim_labels20220617.npy',
                        # feature_files_test = paste('test_sim_features20220617.npy', collapse=""),
                        label_files_test = 'test_sim_labels20220617.npy',
                        rnn_names = paste('rnn20220617', collapse=""), 
                        empirical_input_file = NULL,
                        max_age = -66, min_age = -0)

# use this syntax to edit entries in the config.ini file
config$set(option="n_areas", value="6", section="simulations")

# move this so the user instead will call this in the run line.
config$write("config.ini")

