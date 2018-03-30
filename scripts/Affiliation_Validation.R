## Pull a random subset of authors for manual checking of affiliation ##

# Load packages and algorithm-assigned author affiliations ####
#install.packages('pacman') # Install pacman package
pacman::p_load(tidyverse) # Load packages

# Read in author affiliations based on Author_Categorization script
author_affiliations <- read_csv('./output_data/author_affiliations.csv')

#### Take a random sample from author affiliations dataframe for validation ####
val <- c('CCC','KJF','JPD','RPM','NKW','MEL','AIK') # List of validator IDs
set.seed(2) # Set seed to replicate same validation draw each time

subset <- author_affiliations %>% # Load author affiliations
  select(-Keyword, -AffiliationGroup) %>% # Omit algorithm-assigned affiliations
  sample_n((length(val) * 150), replace= FALSE) %>% # Sample 150 authors per validator without replacement
  mutate(SearchString = paste(Author, ListedAffiliation, sep = ' '), # Concatenate author and affiliation
         Validator = rep(val, length = nrow(.))) %>% # Assign validator ID at random
  arrange(Validator) %>% # Sort rows by validator ID
  mutate(ManualKeyword = '', ManualAffiliation = '') # Add empty column for validator to assign keyword & affiliation
  
write_csv(subset, './output_data/validation/validation.csv') # Export to validation subfolder

#### Pull algorithm-assigned CS authors for manual checking ####
CS_subset <- author_affiliations %>% # Load author affiliations
  filter(AffiliationGroup == "CS") %>% # Just pick out the CS authors 
  select(-Keyword, -AffiliationGroup) %>% # Omit algorithm-assigned affiliations
  mutate(SearchString = paste(Author, ListedAffiliation, sep = ' ')) %>% # Concatenate author and affiliation
  mutate(ManualKeyword = '', ManualAffiliation = '') # Add empty column for validator to assign keyword & affiliation

write_csv(CS_subset, './output_data/validation/CSvalidation.csv') # Export computer science authors as csv to validation subfolder

#### Compare completed validation to automated author affiliation ####
# Pull individual validation files from validation folder
validation_path <- "output_data/validation/individual_validation" # Set path of individual validation csv's

manual_validation <- dir(validation_path, pattern = "*.csv") %>% #
  map_df(~ read_csv(file.path(validation_path, .))) %>% # Load individual csv's
  filter(ManualAffiliation != "NA") %>% # Exclude rows within each csv that we're assigned to that validator
  rename(ListedAffiliation = OriginalAffiliation) %>% # Update column name to match change in Categorization script
  mutate(ManualGroup = recode(ManualAffiliation, "Architecture" = "SS", "Chemistry" = "PS", # Rename factors to match categorization groups
                              "Computer Science" = "CS", "Earth Science" = "ES","Education" = "SS",
                              "Engineering" = "EG", "Environmental Biology" = "ES",
                              "Humanities" = "SS", "Interdisciplinary Computing" = "CS",
                              "InterdisciplinaryEnvironmental" = "ES", "Life Science" = "ES",
                              "Math" = "MA", "Physics" = "PS", "Social Science" = "SS")) %>%
  select(-(Affiliation_ID:Year), -Author_ID, -ManualAffiliation)

# Select focal rows for matching from algorithm-assigned author affiliations
assigned_affiliations <- author_affiliations %>%
  select(Paper_ID, Keyword:Author)

# Match validation and categorization by Paper_ID, Author, and Affiliation
validation_check <- right_join(assigned_affiliations, manual_validation, 
                              by = c("Paper_ID", "ListedAffiliation", "Author")) %>%
  mutate(Match = ifelse(AffiliationGroup == ManualGroup, "YES", "NO")) # Add indicator of Affiliation match

# Summarize number of mis-matched affiliations between categorization script and manual validation
mismatches <- validation_check %>%
  filter(Match == "NO") %>%
  group_by(ManualGroup) %>%
  summarise(N_Mismatches = n())

# Calculate total number of mismatches of validation entries
mismatch_metrics <- mismatches %>%
  mutate(Total_Mismatches = sum(N_Mismatches),
         Prop_Mismatches = round(Total_Mismatches/ nrow(validation_check),2)) %>%
  filter(ManualGroup != "UNKNOWN") %>%
  mutate(Non_UNKNOWN_Mismatches = sum(N_Mismatches),
         Prop_Non_UNKNOWN_Mismatches = round(Non_UNKNOWN_Mismatches / nrow(validation_check),2)) %>%
  distinct(Total_Mismatches, Prop_Mismatches, Non_UNKNOWN_Mismatches, Prop_Non_UNKNOWN_Mismatches)
