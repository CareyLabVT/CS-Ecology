#' Load Web of Science records pulled for Ecology journal articles and
#' assign authors to disciplinary categories based on their institutional affiliations 
#'
#' Written by AIK based on original script by NKW, and edited by KJF and CCC
#' Last edits by AIK on 21 February 2018

#### Install and load packages #### 
pacman::p_load(tidyverse)
library(stringr) # for me this doesn't load with tinyverse for some reason

#### Read in raw data from Web of Science #### 
raw = read.csv("./raw_data/Ecology_FullRecords.csv") %>% # Load Web of Science entries
  select("paper_ID", "AUTHOR", "Affiliation1", "Affiliation2", "YEAR") # Retain only these columns

#### Read in categorization keywords and reshape from long to wide ####
keywords <- read_csv('./raw_data/keyword_categories.csv', trim_ws = FALSE) %>% # load keywords long format
  group_by(category) %>% mutate(id=1:n()) %>% # group by category
  spread(category, keyword) %>% select(-id) # reshape keywords from long to wide

for (i in 1:ncol(keywords)) { # This loop translates [comma], which is used as a placeholder, to a real comma 
  counter = 1
  while (!is.na(keywords[counter,i])) {
    keywords[counter,i] = gsub("\\[comma\\]", ",",as.character(keywords[counter,i]))
    counter = counter +1 
  }
}

##### Lump categories into broad groups #####
groups = c("CS", "MA", "EG", "PS", "SS", "ES") # Define groups by abbreviation

matchUp <- data.frame() %>% bind_rows(list(  # Assign discipline categories to groups in a dataframe that has four rows
  CS = c("InterdisciplinaryComputing", "ComputerScience", NA, NA),
  MA = c("Math", NA, NA, NA), 
  EG = c("Engineering", NA, NA, NA), 
  PS = c("Chemistry", "Physics", NA, NA),
  SS = c("Education", "Humanities", "Architecture", "SocialScience"), 
  ES = c("EarthScience", "EnvironmentalBiology", "LifeScience", "InterdisciplinaryEnvironmental")))

#### Create the author database ####
affiliationDataFrame = c(0, 0, 0, 0, 0, 0, 0) # Initialize dataframe to be populated 
  #in loop to have columns with c("Paper_ID", "Affiliation_ID", "Year", "Keyword", "AffiliationGroup", "OriginalAffiliation", "Author")
rownamecounter = 1
options(warn = -1) # turning off warnings for inner concatenation 
for (jj in 2:length(raw$Affiliation1)) {
  paperNum = raw$paper_ID[jj] # unique paper ID
  currYear = as.numeric(as.character(raw$YEAR[jj])) #year in which paper was published
  authorsAndAffils = c(0, 0)
  affiliations = unlist(strsplit(as.character(raw$Affiliation1[jj]), "\\[.*?\\]")) 
  # create a character string the length of the total number of authors on each paper,
  # with each value in the string containing the complete affiliation for each author
  authors = gsub("\\].*?;", "", raw$Affiliation1[jj]) # collect the authors from the Affiliation1 list
  
  #affiliations = affiliations[(affiliations != "")] # remove empty entries
  goHere = FALSE
  if (length(affiliations) == 1) {
    #if (length(authors) > 1) {
    goHere = TRUE
    affiliations = c(unlist(strsplit(as.character(affiliations), ";"))) # separates each affiliation with a semicolon
  }
  affiliations = affiliations[(affiliations != "")] # remove empty entries
  if (goHere || (trimws(authors) == trimws(affiliations) && length(affiliations) > 0 )) {#(length(authors) == 0 && length(affiliations)>0) {
    authors = c(unlist(strsplit(as.character(raw$AUTHOR[jj]), ";")))
    if (length(affiliations) == 1) { # create a matrix with the first column = authors and the second column = their affiliation
      for (j in 1:length(authors)) {
        authorsAndAffils = rbind(authorsAndAffils, t(c(authors[j], affiliations[1])))
      }
    } else if (length(authors) == length(affiliations)) { 
      for (j in 1:length(authors)) {
        authorsAndAffils = rbind(authorsAndAffils, t(c(authors[j], affiliations[j])))
      }
    } else {
      for (j in 1:length(authors)) {
        if (j < length(affiliations)) {
          authorsAndAffils = invisible(suppressWarnings(rbind(authorsAndAffils, t(c(authors[j], affiliations[j])))))
        } else {
          authorsAndAffils = rbind(authorsAndAffils, t(c(authors[j], affiliations[length(affiliations)])))
        }
      }
    }
  } else if (length(affiliations) > 0) {
    authors = unlist(strsplit(authors, "\\[")) # split author list by their affiliations 
    authors = authors[-(authors == "")] # remove empty entries
    authors[length(authors)] = # remove final untrimmed affiliation for last author
      (unlist(strsplit(authors[length(authors)], "\\]")))[1]
    if (length(authors) > 0) {
      for (j in 1:length(authors)) {
        strippedAuthors = unlist(strsplit(authors[j], ";"))
        strippedAuthors = strippedAuthors[(trimws(strippedAuthors) != "")] # remove empty entries
        for (t in 1:length(strippedAuthors)) {
          affiliationsTemp = c(unlist(strsplit(as.character(affiliations[j]), ";")))
          affiliationsTemp = affiliations[(affiliations != "")] # remove empty entries
          currAuthor = gsub("Univ.*", "", trimws(strippedAuthors[t]))
          if (length(affiliationsTemp) == 1) {
            authorsAndAffils = rbind(authorsAndAffils, t(c(trimws(strippedAuthors[t]), affiliations[j])))
            # create a matrix with the first column = authors and the second column = their affiliation
          } else if (length(affiliationsTemp) > 0) {
            for (qq in 1:length(affiliationsTemp)) {
              authorsAndAffils = rbind(authorsAndAffils, t(c(trimws(strippedAuthors[t]), affiliationsTemp[qq])))
            }
          }
        }
      }
    }
  } else {
    affiliations = unlist(strsplit(as.character(raw$Affiliation2[jj]), "\\(reprint author),.*?")) 
    authorsAndAffils = c(trimws(affiliations[2]), trimws(affiliations[1]))
  }
  authorsAndAffils = data.frame(authorsAndAffils) # convert matrix to dataframe
  authorsAndAffils = authorsAndAffils[2:nrow(authorsAndAffils),] # remove empty first row
  
  if (!is.null(nrow(authorsAndAffils)) && nrow(authorsAndAffils) > 0) { # now we match each affiliation with the keywords
    colnames(authorsAndAffils) = c("Author", "Affiliation") # add column names
    paperAffilNumber = 1
    for (i in 1:nrow(authorsAndAffils)) {
      thisRow = c(rep(0, 7))
      keyword_matched = str_extract(tolower(as.character(authorsAndAffils[i,2])), 
                                    paste(keywords$Remove, collapse = "|"))
      if (!is.na(keyword_matched)) {
        if (i > 1 && trimws(oldRow[6]) != trimws(as.character(authorsAndAffils[i,2]))) {
          paperAffilNumber = paperAffilNumber + 1
        }
        thisRow[1] = as.numeric(as.character(raw$paper_ID[jj]))
        thisRow[2] = paperAffilNumber #paper ID number
        thisRow[3] = as.numeric(as.character(raw$YEAR[jj])) # year of paper
        thisRow[4] = keyword_matched # identifies the words in the affiliation that matched the disciplinary keywords
        thisRow[5] = "Remove"
        thisRow[6] = as.character(authorsAndAffils[i,2]) # original affiliation from paper
        thisRow[7] = as.character(authorsAndAffils[i,1]) # author's name
        print(paste0("Affiliation: ", affiliations[i], " was assigned to |", groups[z], "|"))
      } else {
        for (z in 1:length(groups)) {
          innerCategories = c(as.character(na.exclude(matchUp[,z]))) # identify each discipline within the disciplinary groups to match keywords
          colnums = which(grepl(paste0(innerCategories, collapse = "|"), colnames(keywords))) # identify columns of keyword dataframe to analyze
          words = ""
          for (j in 1:length(colnums)) {
            words = c(words, as.character(na.exclude(pull(keywords[,colnums[j]])))) # combine all relevant
            # keywords 
          }
          keyword_matched = str_extract(tolower(as.character(authorsAndAffils[i,2])), 
                                        paste0(as.character(words[2:length(words)]), collapse = "|"))
          if (!is.na(keyword_matched)) {
            if (i > 1 && trimws(oldRow[6]) != trimws(as.character(authorsAndAffils[i,2]))) {
              paperAffilNumber = paperAffilNumber + 1
            }
            thisRow[1] = as.numeric(as.character(raw$paper_ID[jj]))
            thisRow[2] = paperAffilNumber
            thisRow[3] = as.numeric(as.character(raw$YEAR[jj]))
            thisRow[4] = keyword_matched
            thisRow[5] = groups[z]
            thisRow[6] = as.character(authorsAndAffils[i,2])
            thisRow[7] = as.character(authorsAndAffils[i,1])
            print(paste0("Affiliation: ", as.character(authorsAndAffils[i,2]), " was assigned to |", groups[z], "|"))
            break
          }
          if (z == length(groups)) {
            thisRow[1] = as.numeric(as.character(raw$paper_ID[jj]))
            thisRow[2] = paperAffilNumber
            thisRow[3] = as.numeric(as.character(raw$YEAR[jj]))
            thisRow[4] = "NoMatch"
            thisRow[5] = "Unmatched"
            thisRow[6] = as.character(authorsAndAffils[i,2])
            thisRow[7] = as.character(authorsAndAffils[i,1])
            print(paste0("Affiliation: ", as.character(authorsAndAffils[i,2]), " was not assigned"))
          }
        }
      }
      if (!is.na(thisRow[1]) && thisRow[1] != 0) {
        toAdd = data.frame(thisRow); colnames(toAdd) = rownamecounter
        invisible(suppressWarnings((affiliationDataFrame = rbind(affiliationDataFrame, t(toAdd)))))
        rownamecounter = rownamecounter + 1
      }
      oldRow = thisRow
    }
  }
} # suppressing warning because loop concatenates multiple instances of the same paper IDs and treats it as a dataframe
options(warn = 0)
affiliationDataFrame = as.data.frame(affiliationDataFrame[2:nrow(affiliationDataFrame),])
colnames(affiliationDataFrame) = c("Paper_ID", "Affiliation_ID", "Year", "Keyword", "AffiliationGroup", "OriginalAffiliation", "Author")

withoutUnmatched = affiliationDataFrame[affiliationDataFrame$Keyword != "NoMatch",]
unmatchedEntries = affiliationDataFrame[affiliationDataFrame$Keyword == "NoMatch",]
write_csv(withoutUnmatched,"./output_data/author_affiliations.csv")
write_csv(unmatchedEntries,"./output_data/unmatched_authors.csv")
write_csv(affiliationDataFrame,"./output_data/matched_unmatched.csv")
