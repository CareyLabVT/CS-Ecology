# PSUEDOCODE!!
# - Install packages
# - Read in Ecology_FullRecords.CSV 
# - omit columns we don’t need, but keep the numeric identifier already in the full records CSV as each paper’s ID
# - read in long version of keywords CSV [this needs to be checked against Nicole’s original script, now on github] that has two columns: Keywords and Affiliations
# - use the for loop to assign disciplinary affiliations to each paper_ID (*not* the lumped categories of CS, MA, etc. but the level of Chemistry, Physics, etc)
# -within the for loop, create the author affiliation data frame. Importantly, this data frame also needs to be in long format, with four columns: “Paper_ID” (this is its number), 
#   “Affiliation_ID” (this is another numeric identifier (1, 2, 3…) for each affiliation listed in the order they are listed on the paper, even if they are repeats and they are later
#    going to be excluded), Keyword, and Affiliation (with the two latter columns matching the keywords CSV columns). The Keyword is the character string from the affiliation that gets 
#    used to classify the affiliation a certain way, with the Affiliation the disciplinary affiliation.
# 
# AIK (14 Feb 18) - I suggest adding a Year column to this long format dataframe - I use that a lot
#
# (because there are three authors, and the listed keywords are the words in their affiliation that assigns them the Environmental Biology category from the keywords CSV)
# 
# -now we clean up the data frame: omit repeat affiliations within the same paper, omit government entities, omit papers that don’t have any affiliations, etc.
# -export this data frame as an “AuthorAffiliationDatabase.CSV”
# -now we look at each paper for our collaboration analysis- create a new for loop that goes through the data frame and assigns the lumped categories (CS, MA, etc.) [note: didn’t we get rid of ‘interdisciplinary computing’?] 
# - create a new data frame “InterdisciplinaryCollaboration.CSV” that looks similar to what Arianna created in the github output file, but with columns: “Paper_ID”, “MA”, “CS”, etc. and with the values in the categories the number of authors with those affiliations. There should also be ‘Total authors’ and ‘Total affiliations’ columns in this CSV so we have an easy gut-check about papers not having as many affiliations as authors (because we want each author per paper to have at least one affiliation). From this final data frame, we can then do the plotting (but in the separate plotting R script).

#### Load necessary packages #### 
pacman::p_load(tidyverse)

#### Read in raw data from Web of Science #### 
raw = read.csv("./raw_data/Ecology_FullRecords.csv") %>% # Load Web of Science entries
  select(-(ACK:EI), -(SC:WC)) # Omit columns without unique identifying data
raw = raw[, which(names(raw) %in% c("paper_ID", "AUTHOR", "Affiliation1", "YEAR"))]

#### Read in categorization keywords and reshape from long to wide ####
keywords <- read_csv('./raw_data/keyword_categories.csv', trim_ws = FALSE) %>% # load keywords long format
  group_by(category) %>% mutate(id=1:n()) %>% # group by category
  spread(category, keyword) %>% select(-id) # reshape keywords from long to wide
# This loop translates [comma], which I used as a placeholder, to a real comma 
for (i in 1:ncol(keywords)) {
  counter = 1
  while (!is.na(keywords[counter,i])) {
    keywords[counter,i] = gsub("\\[comma\\]", ",",as.character(keywords[counter,i]))
    counter = counter +1 
  }
}

##### Lump categories into broad groups #####
groups = c("CS", "MA", "EG", "PS", "SS", "ES") # Define groups by abbreviation

matchUp <- data.frame() %>% bind_rows(list(        # Assign categories to groups
  CS = c("InterdisciplinaryComputing", "ComputerScience", NA, NA),
  MA = c("Math", NA, NA, NA), 
  EG = c("Engineering", NA, NA, NA), 
  PS = c("Chemistry", "Physics", NA, NA),
  SS = c("Education", "Humanities", "Architecture", "SocialScience"), 
  ES = c("EarthScience", "EnvironmentalBiology", "LifeScience", NA)))

#### Create the author database ####
# Data frame for papers with their respective authors
#papersShover = c(0, 0, 0, 0, 0, 0)
#papersShoverPaps = c(rep(0, length(groups) + 2))
affiliationDataFrame = c(0, 0, 0, 0, 0)

for (jj in 2:length(raw$Affiliation1)) {
  #thisRow = c(rep(0,length(groups)))
  paperNum = raw$paper_ID[jj]
  currYear = as.numeric(as.character(raw$YEAR[jj]))
  affiliations = unlist(strsplit(as.character(raw$Affiliation1[jj]), "\\[.*?\\]"))
  if (length(affiliations) == 1) {
    authors = c(unlist(strsplit(as.character(raw$AUTHOR[jj]), ";")))
    if (length(authors) > 1) {
      affiliations = c(unlist(strsplit(as.character(affiliations), ";")))
    }
  }
  affiliations = affiliations[(affiliations != "")] # remove empty entries
  if (length(affiliations) > 0) {
    for (i in 1:length(affiliations)) {
      thisRow = c(rep(0, 5))
      authors = gsub("\\].*?;", "", raw$Affiliation1[jj]) # remove the affiliations from the Affiliation1 list
      authorsBreakdown = unlist(strsplit(authors, "\\[")) # split author by affiliation
      authorsBreakdown = authorsBreakdown[-(authorsBreakdown == "")] # remove empty entries
      authorsBreakdown[length(authorsBreakdown)] = # remove final untrimmed affiliation
        (unlist(strsplit(authorsBreakdown[length(authorsBreakdown)], "\\]")))[1]
      numAuthors = length(unlist(strsplit(authorsBreakdown[i], ";"))) # split author within present affil
      
      removeAffil = (length(grep(paste(keywords$Remove, collapse = "|"), affiliations[i],
                                 fixed = FALSE, ignore.case = TRUE)) > 0) # flag for whether to proceed
      if (!removeAffil) {
        for (z in 1:length(groups)) {
          innerCategories = c(as.character(na.exclude(matchUp[,z]))) # grab subcategories
          colnums = which(grepl(paste0(innerCategories, collapse = "|"), colnames(keywords))) # grab columns of keyword matrix to analyze
          words = ""
          for (j in 1:length(colnums)) {
            words = c(words, as.character(na.exclude(pull(keywords[,colnums[j]])))) # combine all relevant
            # keywords 
          }
          if (length(grep(paste0(as.character(words[2:length(words)]), collapse = "|"), affiliations[i]
                          , fixed = FALSE, ignore.case = TRUE)) > 0) { # check whether affilName matches conglomerate
            # keyword group
            thisRow[1] = as.numeric(as.character(raw$paper_ID[jj]))
            thisRow[2] = z
            thisRow[3] = as.numeric(as.character(raw$YEAR[jj]))
            #thisRow[4] # fill in Keyword and Affiliation
            print(paste0("Affiliation: ", affiliations[i], " was assigned to |", groups[z], "|"))
            break
          }
        }
      }
      if (thisRow[1] != 0) {
        affiliationDataFrame = rbind(affiliationDataFrame, thisRow)
      }
    }
  }
}
affiliationDataFrame = affiliationDataFrame[2:nrow(affiliationDataFrame),]
colnames(affiliationDataFrame) = c("Paper_ID", "Affiliation_ID", "Year", "Keyword", "Affiliation")