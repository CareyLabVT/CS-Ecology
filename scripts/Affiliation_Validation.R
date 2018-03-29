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

all_affildata = read_csv('./output_data/author_affiliations.csv')
for (t in 1:length(val)) {
  if (val[t] != 'JPD') {
  currFile = paste0('./output_data/validation/validation_', val[t], '.csv')
  curr_data = read_csv(currFile) %>%
              filter(Validator == val[t])
  if (t == 1) {
    all_valdata = curr_data
  } else {
    all_valdata = all_valdata %>%
                  bind_rows(curr_data)
  }
  }
}

all_valdata = all_valdata %>%
              arrange(Paper_ID) %>%
              mutate(ID_affil = paste0(all_valdata$Paper_ID, '_', all_valdata$Author_ID, '_', all_valdata$Affiliation_ID))

all_affildata = all_affildata %>%
                arrange(Paper_ID) %>%
                mutate(ID_affil = paste0(all_affildata$Paper_ID, '_', all_affildata$Author_ID, '_', all_affildata$Affiliation_ID)) %>%
                filter(ID_affil %in% all_valdata$ID_affil)
