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
keywords <- read_csv('./raw_data/category_keywords.csv') %>% # load keywords long format
  group_by(category) %>% mutate(id=1:n()) %>% # group my category
  spread(category, keyword) %>% select(-id) # reshape keywords from long to wide

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
  if (!is.na(raw$Affiliation1[jj]) && raw$Affiliation1[jj] != "") {
    humptyDumpty = (c(unlist(strsplit(as.character(raw$Affiliation1[jj]), ""))))
    paperNum = raw$paper_ID[jj]
    affilNames = c(0)
    thisRow = c(rep(0,length(groups)))
    for (g in 1:length(humptyDumpty)) {
      if (humptyDumpty[g] == '[' || g == 1) {
        ## This should be its own function - of course before any sharing if done
        goagain = TRUE; g1 = g; goOnceAgain = FALSE; counterOnceAgain = 1
        while (goagain || goOnceAgain) {
          d = g1 + 2; authorName = c(humptyDumpty[g1 + 1]) # start reforming author name
          goagain = FALSE; goOnceAgain = FALSE; skip = FALSE;
          
          if ((length(grep(patterning[1], raw$Affiliation1[jj])) == 0 ||
               (goOnceAgain))) {
            authorName = as.character(raw$AUTHOR[jj]); d = d - 2;
            authors = c(unlist(strsplit(as.character(raw$AUTHOR[jj]), ";")))
            if (length(authors) > 1 && counterOnceAgain <= length(authors)) {
              authorName = authors[counterOnceAgain]
              affils = c(unlist(strsplit(as.character(raw$Affiliation1[jj]), ";")))
              if (length(affils) >= counterOnceAgain) {
                affilName = affils[counterOnceAgain]
              } else {
                affilName = affils[length(affils)]
              }
              if (counterOnceAgain + 1 <= length(authors)) {
                counterOnceAgain = counterOnceAgain + 1
                goOnceAgain = TRUE
              }
              skip = TRUE
            }
          } else {
            while (humptyDumpty[d] != ']') {
              if (humptyDumpty[d] == ';') {
                goagain = TRUE
                starting = d + 1
              } else if (!goagain) {
                authorName = c(authorName, humptyDumpty[d]); 
              }
              d = d + 1; 
            }
            authorName = paste(authorName, collapse = "") # push author name back together
          }
          if (!skip) {
            if (humptyDumpty[d] != '[') {
              affilName = c(humptyDumpty[d]);
            } else {
              affilName = c(humptyDumpty[d + 1]); d = d + 1
            }
            while (!is.na(humptyDumpty[d]) && humptyDumpty[d] != '[' && d != length(humptyDumpty)
                   && humptyDumpty[d] != ';') {
              affilName = c(affilName, humptyDumpty[d]); d = d + 1;
            }
            affilName = paste(affilName, collapse = "") # push affiliation back together
          }
          
          if (length(grep(paste(keywords$Remove, collapse = "|"), affilName,
                          fixed = FALSE, ignore.case = TRUE)) > 0) {
            affilType = 0
            affilCat = 0
          } else {
            for (z in 1:length(groups)) { 
              innerCategories = c(as.character(na.exclude(matchUp[,z]))) # grab subcategories
              colnums = which(grepl(paste0(innerCategories, collapse = "|"), colnames(keywords))) # grab columns of keyword matrix to analyze
              words = ""
              for (j in 1:length(colnums)) {
                words = c(words, as.character(na.exclude(keywords[,colnums[j]]))) # combine all relevant
                                                                                         # keywords 
              }
              if (length(grep(paste0(as.character(words[2:length(words)]), collapse = "|"), affilName
                              , fixed = FALSE, ignore.case = TRUE)) > 0) {
                affilType = z
                affilCat = groups[z] # select the appropriate affiliation
                break
              }
              else {
                affilType = 0
                affilCat = 0
              }
            }
          }
          print(affilName)
          currYear = as.numeric(as.character(raw$YEAR[jj]))
          papersShover = rbind(papersShover, c(authorName, currYear, affilName, affilCat, affilType, paperNum))
          # authorName, currYear, affilName, affilCat, affilType, paperNum
          insert = TRUE
          for (j in 1:length(affilNames)) {
            if (affilNames[j] == affilName) {
              insert = FALSE
            }
          }
          if (insert) {
            thisRow[affilType] = thisRow[affilType] + 1
            yearSaver[currYear - lowYear + 1, affilType] = yearSaver[currYear - lowYear + 1, affilType] + 1
          }
          affilNames = c(affilNames, affilName)
          if (goagain) {
            g1 = starting
          }
        }
      }
    }
    papersShoverPaps = rbind(papersShoverPaps, c(paperNum, currYear, thisRow))
  }
}
colnames(papersShoverPaps) = c("paperID", "Year", groups)
## Save author database as a dataframe, export as .csv ####
categorizations <- as.data.frame(as.table(papersShoverPaps)) #### <--- I don't think this output is right yet
                                                             # - (AIK: yes, better way to output needed if we want to change it)
write_csv(categorizations, './output_data/author_affiliations.csv')