library(tidyverse)
library(here)
library(readxl)
library(janitor)

dat_extant <- read_xlsx(here("data", 
                             "Lookup_Taxonomy(extant).xlsx")) %>% 
  clean_names() %>% 
  mutate(genus = word(species), 
         .before = family) %>% 
  select(-contains("synon")) %>% 
  pivot_longer(cols = c(order, superorder), 
               names_to = "taxon_group", 
               values_to = "taxon")

dat_altern <- read_xlsx(here("data", 
                             "Lookup_Taxonomy_LW.xlsx")) %>% 
  clean_names() %>% 
  mutate(genus = word(species), 
         .before = family) %>% 
  select(-contains("synon")) %>% 
  pivot_longer(cols = c(order, superorder), 
               names_to = "taxon_group", 
               values_to = "taxon")


# set up function
get_recent_taxa <- function(dataset = c(dat_extant, dat_altern), 
                            taxon_group = c("order", 
                                            "superorder"), 
                            taxon_level = c(species, genus), 
                            taxon) {
  
  filter(dataset, 
         taxon_group == taxon_group, 
         taxon == !!taxon) %>% 
    distinct(!!enquo(taxon_level)) %>% 
    nrow()
}

dat_input <- tibble(taxon_group = c(rep("superorder", 2),  
                                  rep("order", 8)), 
                  taxon = c("Selachii", "Batoidea", 
                            "Carcharhiniformes", 
                            "Hexanchiformes", 
                            "Lamniformes", 
                            "Myliobatiformes", 
                            "Orectolobiformes", 
                            "Rajiformes", 
                            "Rhinopristiformes", 
                            "Squaliformes"))

# combine
dat_ext <- dat_input %>% 
  mutate(n_species_ori = map2_int(.x = taxon_group, 
                           .y = taxon, 
                           .f = ~ get_recent_taxa(dat_extant, 
                                                  taxon_group = .x, 
                                                  taxon = .y, 
                                                  taxon_level = species)), 
         n_species_upd = map2_int(.x = taxon_group, 
                                  .y = taxon, 
                                  .f = ~ get_recent_taxa(dat_altern, 
                                                         taxon_group = .x, 
                                                         taxon = .y, 
                                                         taxon_level = species)), 
         n_genus_ori = map2_int(.x = taxon_group, 
                                  .y = taxon, 
                                  .f = ~ get_recent_taxa(dat_extant, 
                                                         taxon_group = .x, 
                                                         taxon = .y, 
                                                         taxon_level = genus)), 
         n_genus_upd = map2_int(.x = taxon_group, 
                                  .y = taxon, 
                                  .f = ~ get_recent_taxa(dat_altern, 
                                                         taxon_group = .x, 
                                                         taxon = .y, 
                                                         taxon_level = genus))) %>% 
  add_row(tibble(taxon_group = "all", 
                 taxon = "all", 
                 n_species_ori = 1100, 
                 n_species_upd = 1100, 
                 n_genus_ori = 192, 
                 n_genus_upd = 205)) %>% 
  mutate(diff_species = n_species_ori - n_species_upd, 
         diff_genus = n_genus_ori - n_genus_upd) 

dat_ext %>% 
  write_rds(here("data", 
                 "extant_species.rds"))
