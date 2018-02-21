## Pull a random subset of authors for manual checking of affiliation ##

#install.packages('pacman') # Install pacman package
pacman::p_load(tidyverse) # Load packages

#### Take a random sample from author affiliations dataframe for validation ####
val <- c('CCC','KJF','JPD','RPM','NWK','MEL','AIK') # List of validator IDs
set.seed(1) # Set seed to replicate same validation draw each time

subset <- read_csv('./output_data/author_affiliations.csv') %>% # Load author affiliations
  select(-Keyword, -AffiliationGroup) %>% # Omit algorithm-assigned affiliations
  sample_frac(0.05, replace = FALSE) %>% # Subsample 5% of authors without replacement
  mutate(Validator = rep(val, length = nrow(.))) %>% # Assign validator ID at random
  arrange(Validator) %>% # Sort rows by validator ID
  mutate(ManualKeyword = '', ManualAffiliation = '') # Add empty column for validator to assign keyword & affiliation

write_csv(subset, './output_data/validation/validation.csv') # Export as csv to validation subfolder
