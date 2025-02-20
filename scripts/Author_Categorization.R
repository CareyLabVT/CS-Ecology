#' Load Web of Science records pulled for Ecology journal articles and
#' assign authors to disciplinary categories based on their institutional affiliations 
#' Written by AIK based on original script by NKW, and edited by KJF and CCC

#### Install and load packages #### 
#install.packages('pacman')
pacman::p_load(tidyverse, stringr)

#### Read in raw data from Web of Science #### 
raw = read.csv("./raw_data/Ecology_FullRecords.csv") %>% # Load Web of Science entries
  select(paper_ID, AUTHOR2, Affiliation1, Affiliation2, YEAR, DOI) %>%  # Retain only these columns
  rename(AUTHOR = AUTHOR2) %>%
  filter(YEAR < 2017)

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
affiliationDataFrame = c(0, 0, 0, 0, 0, 0, 0, 0, 0) # Initialize dataframe to be populated 
  #in loop to have columns with c("Paper_ID", "DOI", Affiliation_ID", "Year", 
  #"Keyword", "AffiliationGroup", "ListedAffiliation", "Author", "Author_ID")
authorDB = c(0)
rownamecounter = 1
options(warn = -1) # turning off warnings for inner concatenation 
for (jj in 2:length(raw$Affiliation1)) {
  paperNum = raw$paper_ID[jj] # unique paper ID
  DOI = as.character(raw$DOI[jj]) # DOI pulled in Web of Science 
  currYear = as.numeric(as.character(raw$YEAR[jj])) #year in which paper was published
  authorsAndAffils = c(0, 0)
  affiliations = unlist(strsplit(as.character(raw$Affiliation1[jj]), "\\[.*?\\]")) 
  # create a character string the length of the total number of authors on each paper,
  # with each value in the string containing the complete affiliation for each author
  authors = gsub("\\].*?;", "", raw$Affiliation1[jj]) # collect the authors from the Affiliation1 list
  
  affiliations = affiliations[(affiliations != "")] # remove empty entries
  goHere = FALSE
  if (length(affiliations) == 1 && length(grep('\\[', authors)) == 0) {
    goHere = TRUE
    affiliations = c(unlist(strsplit(as.character(affiliations), ";"))) # separates each affiliation with a semicolon
  }
  affiliations = affiliations[(affiliations != "")] # remove empty entries
  if (goHere || (trimws(authors) == trimws(affiliations) && length(affiliations) > 0 ))
      {
    authors = trimws(c(unlist(strsplit(as.character(raw$AUTHOR[jj]), ";"))))
    if (length(affiliations) == 1) { # create a matrix with the first column = authors and the second column = their affiliation
      for (j in 1:length(authors)) {
        authorsAndAffils = rbind(authorsAndAffils, t(c(authors[j], affiliations[1])))
      }
    } else if (length(authors) == length(affiliations)) { 
      for (j in 1:length(authors)) {
        authorsAndAffils = rbind(authorsAndAffils, t(c(authors[j], affiliations[j])))
      }
    } else if (length(affiliations) == 2 && (substr(affiliations[1], 1, 10) == substr(affiliations[2], 1, 10))) { 
      for (j in 1:length(authors)) { 
        authorsAndAffils = rbind(authorsAndAffils, t(c(authors[j], trimws(affiliations[1]))))
        authorsAndAffils = rbind(authorsAndAffils, t(c(authors[j], trimws(affiliations[2]))))
      }
    } else {
      giveAll = FALSE
      if (length(grep("\\(reprint author),.*?", raw$Affiliation2[jj]))  == 0) {
        affiliations = trimws(affiliations)
        giveAll = TRUE
      } else {
        affiliationRe = trimws(unlist(strsplit(as.character(raw$Affiliation2[jj]), "\\(reprint author),.*?")) )
        authorsAndAffils = rbind(authorsAndAffils, t(c(trimws(affiliationRe[1]), trimws(affiliationRe[2]))))
        authors = authors[authors != affiliationRe[1]]
        affiliations = trimws(affiliations[(trimws(affiliations) != trimws(affiliationRe[2])) &&
                                           (substr(affiliations, 1, 10) != substr(affiliationRe[2], 1, 10))])
      }
      if (length(authors) > 0) {
        for (j in 1:length(authors)) {
          if (length(authors) == length(affiliations)) {
            authorsAndAffils = rbind(authorsAndAffils, t(c(authors[j], affiliations[j])))
          } else if (giveAll && ((length(affiliations) == 1) || 
                     (length(affiliations) == 2 && (substr(affiliations[1], 1, 10) == substr(affiliations[2], 1, 10))))) { 

            for (zz in 1:length(affiliations)) {
              authorsAndAffils = rbind(authorsAndAffils, t(c(authors[j], affiliations[zz])))
            }
          }
        }
      }
    }
  } else if (length(affiliations) > 0) {
    authors = unlist(strsplit(as.character(raw$Affiliation1[jj]), "\\["))
    for (q in 1:length(authors)) {
      authors[q] = gsub("\\].*\\[", "", authors[q])
      authors[q] = gsub("\\].*", "", authors[q])
      authors[q] = gsub("\\[", "", authors[q])
    }
    authors = authors[-(authors == "")] # remove empty entries
    if (length(authors) > 0) {
      for (j in 1:length(authors)) {
        strippedAuthors = unlist(strsplit(authors[j], ";"))
        strippedAuthors = strippedAuthors[(trimws(strippedAuthors) != "")] # remove empty entries
        for (t in 1:length(strippedAuthors)) {
          affiliationsTemp = c(unlist(strsplit(as.character(affiliations[j]), ";")))
          affiliationsTemp = trimws(affiliationsTemp[(affiliationsTemp != "") & (affiliationsTemp != " ")]) # remove empty entries
          currAuthor = gsub("Univ.*", "", trimws(strippedAuthors[t]))
          if (length(affiliationsTemp) == 1) {
            authorsAndAffils = rbind(authorsAndAffils, t(c(trimws(strippedAuthors[t]), affiliationsTemp)))
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
    affiliations = trimws(unlist(strsplit(as.character(raw$Affiliation2[jj]), "\\(reprint author),.*?")))
    authorsAndAffils = t(c(trimws(affiliations[1]), trimws(affiliations[2])))
  }
  authors = trimws(c(unlist(strsplit(as.character(raw$AUTHOR[jj]), ";"))))
  for (k in 1:length(authors)) {
    if (is.null(ncol(authorsAndAffils)) || (length(authors[k]) > 0 &&
        length(grep(unlist(strsplit(authors[k], ","))[1], paste0(authorsAndAffils[,1], collapse = "|"))) == 0)) {
      authorsAndAffils = rbind(authorsAndAffils, t(c(trimws(authors[k]), "1University1"))) 
    }
  }
  authorsAndAffils = data.frame(authorsAndAffils) # convert matrix to dataframe
  if (!is.null(ncol(authorsAndAffils)) && length((which(is.na(authorsAndAffils[,2])))) > 0) {
    authorsAndAffils = authorsAndAffils[-(which(is.na(authorsAndAffils[,2]))),]
  } else if (!is.null(ncol(authorsAndAffils))) {
    authorsAndAffils = authorsAndAffils[-(which((authorsAndAffils[,2]) == 0)),]
  }
  
  if (!is.null(nrow(authorsAndAffils)) && nrow(authorsAndAffils) > 0) { # now we match each affiliation with the keywords
    colnames(authorsAndAffils) = c("Author", "Affiliation") # add column names
    paperAffilNumber = 1
    for (i in 1:nrow(authorsAndAffils)) {
      thisRow = c(rep(0, 9))
      keyword_matched = str_extract(tolower(as.character(authorsAndAffils[i,2])), 
                                    paste(keywords$Remove, collapse = "|"))
      if (!is.na(keyword_matched)) {
        if (i > 1 && trimws(oldRow[6]) != trimws(as.character(authorsAndAffils[i,2]))) {
          paperAffilNumber = paperAffilNumber + 1
        }
        author_matched = str_extract(tolower(trimws(as.character(authorsAndAffils[i,1]))), 
                                      paste(tolower(authorDB), collapse = "|"))
        thisRow[6] = "Remove"
        thisRow[1] = as.numeric(as.character(raw$paper_ID[jj]))
        thisRow[2] = DOI # paper DOI
        thisRow[3] = paperAffilNumber #paper ID number
        thisRow[4] = as.numeric(as.character(raw$YEAR[jj])) # year of paper
        thisRow[5] = keyword_matched # identifies the words in the affiliation that matched the disciplinary keywords
        thisRow[7] = as.character(authorsAndAffils[i,2]) # original affiliation from paper
        thisRow[8] = as.character(authorsAndAffils[i,1]) # author's name
        if (is.na(author_matched) || length(which(tolower(authorDB) == tolower(trimws(as.character(authorsAndAffils[i,1]))))) == 0) {
          authorDB = c(authorDB, trimws(as.character(authorsAndAffils[i,1])))
          thisRow[9] = length(authorDB) - 1
        } else {
          thisRow[9] = which(tolower(authorDB) == tolower(trimws(as.character(authorsAndAffils[i,1])))) - 1
        }
        print(paste0("Affiliation: ", as.character(authorsAndAffils[i,2]), " was assigned to |Remove|"))
      } else {
        for (z in 1:length(groups)) {
          innerCategories = c(as.character(na.exclude(matchUp[,z]))) # identify each discipline within the disciplinary groups to match keywords
          colnums = which(grepl(paste0(innerCategories, collapse = "|"), colnames(keywords))) # identify columns of keyword dataframe to analyze
          words = ""
          for (j in 1:length(colnums)) {
            words = c(words, as.character(na.exclude(pull(keywords[,colnums[j]])))) # combine all relevant keywords 
          }
          keyword_matched = str_extract(tolower(as.character(authorsAndAffils[i,2])), 
                                        paste0(as.character(words[2:length(words)]), collapse = "|"))
          if (!is.na(keyword_matched)) {
            if (i > 1 && trimws(oldRow[6]) != trimws(as.character(authorsAndAffils[i,2]))) {
              paperAffilNumber = paperAffilNumber + 1
            }
            author_matched = str_extract(tolower(trimws(as.character(authorsAndAffils[i,1]))), 
                                         paste(tolower(authorDB), collapse = "|"))
            thisRow[1] = as.numeric(as.character(raw$paper_ID[jj]))
            thisRow[2] = DOI
            thisRow[3] = paperAffilNumber
            thisRow[4] = as.numeric(as.character(raw$YEAR[jj]))
            thisRow[5] = keyword_matched
            thisRow[6] = groups[z]
            thisRow[7] = as.character(authorsAndAffils[i,2])
            thisRow[8] = as.character(authorsAndAffils[i,1])
            if (is.na(author_matched) || length(which(tolower(authorDB) == tolower(trimws(as.character(authorsAndAffils[i,1]))))) == 0) {
              authorDB = c(authorDB, trimws(as.character(authorsAndAffils[i,1])))
              thisRow[9] = length(authorDB) - 1
            } else {
              thisRow[9] = which(tolower(authorDB) == tolower(trimws(as.character(authorsAndAffils[i,1])))) - 1
            }
            print(paste0("Affiliation: ", as.character(authorsAndAffils[i,2]), " was assigned to |", groups[z], "|"))
            break
          }
          if (z == length(groups)) {
            author_matched = str_extract(tolower(trimws(as.character(authorsAndAffils[i,1]))), 
                                         paste(tolower(authorDB), collapse = "|"))
            thisRow[1] = as.numeric(as.character(raw$paper_ID[jj]))
            thisRow[2] = DOI
            thisRow[3] = paperAffilNumber
            thisRow[4] = as.numeric(as.character(raw$YEAR[jj]))
            thisRow[5] = "NoMatch"
            thisRow[6] = "Unmatched"
            thisRow[7] = as.character(authorsAndAffils[i,2])
            thisRow[8] = as.character(authorsAndAffils[i,1])
            if (is.na(author_matched) || length(which(tolower(authorDB) == tolower(trimws(as.character(authorsAndAffils[i,1]))))) == 0) {
              authorDB = c(authorDB, trimws(as.character(authorsAndAffils[i,1])))
              thisRow[9] = length(authorDB) - 1
            } else {
              thisRow[9] = which(tolower(authorDB) == tolower(trimws(as.character(authorsAndAffils[i,1])))) - 1
            }
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
colnames(affiliationDataFrame) = c("Paper_ID", "DOI", "Affiliation_ID", "Year", 
                                   "Keyword", "AffiliationGroup", "ListedAffiliation", "Author", "Author_ID")

#write_csv(withoutUnmatched,"./output_data/author_affiliations.csv")
