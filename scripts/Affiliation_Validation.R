## Pull a random subset of authors for manual checking of affiliation ##

#install.packages('pacman') # Install pacman package
pacman::p_load(tidyverse) # Load packages

#### Take a random sample from author affiliations dataframe for validation ####
val <- c('CCC','KJF','JPD','RPM','NKW','MEL','AIK') # List of validator IDs
set.seed(2) # Set seed to replicate same validation draw each time

subset <- read_csv('./output_data/author_affiliations.csv') %>% # Load author affiliations
  select(-Keyword, -AffiliationGroup) %>% # Omit algorithm-assigned affiliations
  sample_n((length(val) * 150), replace= FALSE) %>% # Sample 150 authors per validator without replacement
  mutate(SearchString = paste(Author, OriginalAffiliation, sep = ' ')) %>% # Concatenate author and affiliation
  mutate(Validator = rep(val, length = nrow(.))) %>% # Assign validator ID at random
  arrange(Validator) %>% # Sort rows by validator ID
  mutate(ManualKeyword = '', ManualAffiliation = '') # Add empty column for validator to assign keyword & affiliation
  
write_csv(subset, './output_data/validation/validation.csv') # Export as csv to validation subfolder

#### Manually check all of the CS authors 
CS_subset <- read_csv('./output_data/author_affiliations.csv') %>% # Load author affiliations
  filter(AffiliationGroup == "CS") %>% # Just pick out the CS authors 
  select(-Keyword, -AffiliationGroup) %>% # Omit algorithm-assigned affiliations
  mutate(SearchString = paste(Author, ListedAffiliation, sep = ' ')) %>% # Concatenate author and affiliation
  mutate(ManualKeyword = '', ManualAffiliation = '') # Add empty column for validator to assign keyword & affiliation


#### Compare completed validation to automated author affiliation ####
# Read in author affiliations based on Author_Categorization script
author_affiliations <- read_csv('./output_data/author_affiliations.csv')

# Pull individual validation files from validation folder
validation_path <- "output_data/validation"

manual_validation <- dir(validation_path, pattern = "*.csv") %>% 
  map_df(~ read_csv(file.path(validation_path, .))) %>%  
  filter(ManualAffiliation != "NA") %>% 
  arrange(Paper_ID) %>%
  mutate(ID_affil = paste0(.$Paper_ID, '_', .$Author_ID, '_', .$Affiliation_ID)) 

validated_affiliations = author_affiliations %>%
  arrange(Paper_ID) %>%
  mutate(ID_affil = paste0(.$Paper_ID, '_', .$Author_ID, '_', .$Affiliation_ID)) %>%
  filter(ID_affil %in% manual_validation$ID_affil)
