#' Load WebofScience records pulled for Ecology journal articles and
#' assign authors to disciplinary categories based on "Affiliation" columns
#'
#' Written by AIK, last edits by KJF on 5 Feb 18

# install.packages('pacman')
pacman::p_load(tidyverse)

## Define functions used later in the script ####

#' Determines # authors per paper for time series analysis
#'
#' @param authorsTogether The dataframe with all the authors
#' @param repeatOffenders people who are repeated
#' @param ltnrow Number of rows in Pull record
authorsPerPaper <- function(autTog,repeatOffenders,ltnrow) {
  autPerP = matrix(0, nrow = ltnrow, ncol = 2)
  for (j in 1:nrow(autTog)) {
    firstPaperNum = as.numeric(as.character(autTog$FirstPaperNum[j]))
    currYear = as.numeric(as.character(autTog$Year[j]))
    autPerP[firstPaperNum,1] = 
      autPerP[firstPaperNum,1] + 1
    autPerP[firstPaperNum,2] = currYear
  }
  for (j in 1:nrow(repeatOffenders)) {
    firstPaperNum = as.numeric(as.character(repeatOffenders$FirstPaperNum[j]))
    currYear = as.numeric(as.character(repeatOffenders$Year[j]))
    autPerP[firstPaperNum,1] = 
      autPerP[firstPaperNum,1] + 1
    autPerP[firstPaperNum,2] = currYear
  }
  autPerPold = autPerP; ctr = 1
  for (g in 1:nrow(autPerPold)) {
    if (autPerPold[g,1] == 0) {
      autPerP = autPerP[-ctr,]
      ctr = ctr - 1
    }
    ctr = ctr + 1
  }
  return(autPerP)
}

## Read in raw data from Web of Science #### 
raw = read.csv("./raw_data/Ecology_FullRecords.csv") %>% # Load Web of Science entries
  select(-(ACK:EI), -(SC:WC)) # Omit columns without unique identifying data

## Read in categorization keywords and reshape from long to wide ####
keywords <- read_csv('./raw_data/keyword_categories.csv', trim_ws = FALSE) %>% #, strip.white = FALSE) %>% # load keywords long format
  group_by(category) %>% mutate(id=1:n()) %>% # group my category
  spread(category, keyword) %>% select(-id) # reshape keywords from long to wide
for (i in 1:ncol(keywords)) {
  counter = 1
  while (!is.na(keywords[counter,i])) {
   keywords[counter,i] = gsub("\\[comma\\]", ",",as.character(keywords[counter,i]))
   counter = counter +1 
  }
}
# Kait : to use this formatting I'll have to change my scheme somehow

##### Lump categories into broad groups #####
groups = c("CS", "MA", "EG", "PS", "SS", "ES") # Define groups by abbreviation

matchUp <- data.frame() %>% bind_rows(list(        # Assign categories to groups
  CS = c("InterdisciplinaryComputing", "ComputerScience", NA, NA),
  MA = c("Math", NA, NA, NA), 
  EG = c("Engineering", NA, NA, NA), 
  PS = c("Chemistry", "Physics", NA, NA),
  SS = c("Education", "Humanities", "Architecture", "SocialScience"), 
  ES = c("EarthScience", "EnvironmentalBiology", "LifeScience", NA)))

## Create the author database ####
# Data frame for papers with their respective authors
papersShover = c(0, 0, 0, 0, 0, 0)
papersShoverPaps = c(rep(0, length(groups) + 2))

# Data frame for authors found by category in each year
lowYear = min(na.exclude(as.numeric(as.character(raw$YEAR))))
highYear = max(na.exclude(as.numeric(as.character(raw$YEAR))))
yearSaver = matrix(0, ncol = length(groups), nrow = highYear - lowYear + 1)
colnames(yearSaver) = c(1:length(groups)); row.names(yearSaver) = c(lowYear:highYear)

patterning = c("\\[",'\\]') # allows us to split authors with affiliations according to
# WebOfScience formatting


for (jj in 2:length(raw$Affiliation1)) {
  thisRow = c(rep(0,length(groups)))
  paperNum = raw$paper_ID[jj]
  currYear = as.numeric(as.character(raw$YEAR[jj]))
  affiliations = unlist(strsplit(as.character(raw$Affiliation1[jj]), "\\[.*?\\]"))
  affiliations = affiliations[-(affiliations == "")]
  if (length(affiliations) > 0) {
    for (i in 1:length(affiliations)) {
      authors = gsub("\\].*?;", "", affiliations) # remove the affiliations from the Affiliation1 list
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
            thisRow[z] = thisRow[z] + 1
            break
          }
        }
      }
    }
  }
  papersShoverPaps = rbind(papersShoverPaps, c(paperNum, currYear, thisRow))
}

colnames(papersShoverPaps) = c("paperID", "Year", groups)

## Save author database as a dataframe, export as .csv ####
categorizations <- as.data.frame(papersShoverPaps)  # Transform affiliation matrix to dataframe
write_csv(categorizations, './output_data/author_affiliations.csv') # Export author affiliations as .csv