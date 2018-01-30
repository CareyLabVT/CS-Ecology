# Load WebofScience records pulled for Ecology journal articles and
# assign authors to disciplinary categories based on "Affiliation" columns
#
# Written by AIK, last edits on 30 Jan 2018 by KJF

# install.packages('pacman')
pacman::p_load(tidyverse)

## Define functions used later in the script ####
#'
#' Checks whether the current author is in the database; if so generates a new 
#' row for that affiliation. 
#' 
#' @param authorsTogether The full file for this group
#' @param authorName The author to search for 
#' @param affilName The author's affiliation
#' @param paperNum The paper index to search for
#' @param currYear The year to be checked
#' @return A new dataframe with the current author added
#' 
checkIfInDB <- function (authorsTogether, authorName, affilName, paperNum, currYear) {
  addIt = TRUE
  for (k in 1:nrow(authorsTogether)) {
    if (authorsTogether[k,1] == authorName) {
      authorsTogether[k,7] = as.numeric(authorsTogether[k,7]) + 1
      addIt = FALSE
      break;
    }
  }
  if (addIt) {
    authorsTogether = rbind(authorsTogether, c(authorName, currYear, affilName, 0, 0, paperNum, 1))
  }
  return (authorsTogether)
}

#'
#' Constructs a dataframe of collaboration from the result
#' returned from disciplinesRepresented 
#' 
#' @param discRep The dataframe returned from disciplinesRepresented
#' @param categories The categories possible
#' @return A dataframe of three columns - 1st, descriptive link,
#'         second, pseudo-hash function key, 3rd count
coordinateLinks <- function(discRep, categories) {
  descripts = rbind(c(rep(length(categories)* length(categories), 0)))
  leftcol = c(rep(0, length(descripts)))
  rightcol = c(rep(0, length(descripts)))
  keys = c(rep(0, length(descripts)))
  counts = c(rep(0, length(descripts)))
  collaborators = c(rep(0, length(descripts)))
  counter = 1
  for (j in 1:length(categories)) {
    if (j + 1 < length(categories)) { 
      for (l in (j + 1):length(categories)) {
        descripts[counter] = paste0(categories[j], '_', categories[l])
        leftcol[counter] = categories[j]
        rightcol[counter] = categories[l]
        keys[counter] = (j / l) + (j * l) + (j + l)
        counter = counter + 1
      }
    }
  }
  
  counts = c(rep(0, length(descripts)))
  collaborators = c(rep(0, length(descripts)))
  for (t in 1:nrow(discRep)) {
    for (q in 2:ncol(discRep)) {
      currCat = q - 1 
      if (((q + 1) < ncol(discRep)) && discRep[t, q] != 0) {
        for (g in (q + 1):ncol(discRep)) {
          if (discRep[t, g] != 0) {
            currKey = (currCat / (g - 1)) + (currCat * (g - 1)) + (currCat + (g - 1))
            magicVal = (keys == currKey)
            collaborators[magicVal] = collaborators[magicVal] + (as.numeric(as.character(discRep[t, g])) 
                                                                 + as.numeric(as.character(discRep[t, q])))
            if (!is.na(counts[magicVal])) {
              counts[magicVal] = counts[magicVal] + 1
            }
          }
        }
      }
    }
  }
  return (cbind(descripts, leftcol, rightcol, keys, collaborators, counts))
}

#' Determines # authors per paper for time series analysis
#'
#' @param autTog The dataframe with all the authors
#' @param repeatOffenders people who are repeated
#' @param ltnrow Number of rows in Pull record
#' @param categories The disciplines being grouped into
#' @return How many disciplines are present on each paper in a large 
#' dataframe
disciplinesRepresented <- function(autTog,repeatOffenders,ltnrow,categories) {
  autPerP = matrix(0, nrow = ltnrow, ncol = length(categories) + 1)
  
  colnames(autTog) = c("Author", "Year", "Department", "Discipline", "DiscCode", "FirstPaperNum", "AuthorCount")
  for (j in 1:nrow(autTog)) {
    firstPaperNum = as.numeric(as.character(autTog$FirstPaperNum[j]))
    currYear = as.numeric(as.character(autTog$Year[j]))
    discCode = as.numeric(as.character(autTog$DiscCode[j]))
    autPerP[firstPaperNum,(1 + discCode)] = 
      autPerP[firstPaperNum,(1 + discCode)] + 1
    autPerP[firstPaperNum,1] = currYear
  }
  for (j in 1:nrow(repeatOffenders)) {
    firstPaperNum = as.numeric(as.character(repeatOffenders$FirstPaperNum[j]))
    currYear = as.numeric(as.character(repeatOffenders$Year[j]))
    discCode = as.numeric(as.character(autTog$DiscCode[j]))
    autPerP[firstPaperNum,(1 + discCode)] = 
      autPerP[firstPaperNum,(1 + discCode)] + 1
    autPerP[firstPaperNum,1] = currYear
  }
  autPerPold = autPerP; ctr = 1
  for (g in 1:nrow(autPerPold)) {
    if (sum(autPerPold[g,(2:(1+length(categories)))]) == 0) {
      autPerP = autPerP[-ctr,]
      ctr = ctr - 1
    }
    ctr = ctr + 1
  }
  return(autPerP)
}

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

## Read in categorization keywords and define categories as factors ####
keywords <- read_csv('./raw_data/category_keywords.csv', col_types = list(
  category = col_factor(c("Remove", "Architecture", "Chemistry", "Computer Science", 
"Earth Science", "Education", "Engineering", "Environmental Biology", "Humanities", 
"Interdisciplinary Computing", "Life Science", "Math", "Physics", "Social Science"))))

##### Categorizes the located affiliations <--- can we reformat use of keywords to use csv entries instead of these lists?#####
axKeyword = c("British Antarctic Survey", "USGS", "Geological Survey", "Kennedy Space", "Fish and Wildlife",
              "FIW", "NOAA", "National Oceanic and", "Consult", "Geol Survey", "Capita", "capital", "capitol",
              "Save Elephants", "USDA", "NASA")
EarthSciencekeywords = c("GEO ", "Geol", "Earth", "GEOL", "EARTH", "Meteorol", "Spatial", "SPATIAL",
                         "Energy", "ENERGY", "CLIMATE", "Climate", "Hydrol", "HYDROL", "Minerals", "MINERALS",
                         "Polar", "POLAR", "Space", "SPACE", "ATMOSPHER", "Atmosfera", "Clima",  "LOW TEMP",
                         "Parsons Lab",  "Ft Collins Sci", "Busgen Inst",  "Macaulay Inst", "Buesgen Inst",
                         "Svalbard UNIS", "Ctr Svalbard",  "Terra Jaume", "Ore Deposits", "Isotope", "Min",
                         "Nordiques", "Ambientales", "Inst Arctic", "Radiocarbon", "Radioisotope", "Hydrophys",
                         "QUATERNARY", "REMOTE SENSING", "PALAEONTOL", "CTR ETUD NORD", "Oceanog")
InterdisciplinaryComputingkeywords = c("Informat", "Informa", "Bioinform", "Data Sci", "Inform Tech", "Info Tech",
                                       "Ecoinfo", "Computat Land", "Computat Ecol", "Theory & Modeling",
                                       "Bioinf", "Univ Politecn Catalunya, Complex Syst Res Grp", "Modelling Sci")
# now includes ecoinformat plus things like math & informat 
ComputerSciencekeywords = c("comp sci", "Informat & Enabling Technol", "Enabling Tech", "Comp Sci", "COMP SCI",  
                            "Computat Sci", "COMPUTAT SCI", "MADALGO", "informat ", "informat,", "informatics",
                            "Computat", "Complejidad", "Eugene Lawler", "Dworkin Lab", "Artificial Intelligence",
                            "Artificial Intel", "Art Intell") # removed "Planck Inst" why does it keep coming back
Mathkeywords = c("math ", "Math ", "MATH ", "Mathemat", 
                 "Mathematics", "Mathemati", "Mathematics", 
                 "Stat ", "stat,", "STATISTICS",  "Santa Fe Inst", 
                 "Nevanlinna Inst",  "DEPT MATEMAT")
Physicskeywords = c("Phys", "PHYS", "Langmuir", "LANGMUIR",  "Gleb Wataghin", "Reg Bariloche",
                    "Inst Balseiro", "Ctr Atom Bariloche",  "Nucl", "NUCL", "isotrace", "theor", "Inst adv") # removed "Planck Inst"
Engineeringkeywords = c("Engn ", "ENGN,", "Control Sci", "Engr", "Coll Engn & Architecture")
LifeSciencekeywords = c(unique(toupper(c("Communicable", "Pharm", "INFECT", "Infect", "Hlth", "HLTH", "Food", "FOOD",
                                         "NEUROBIOL", "Neurobiol", "Bacteriol",  "MICROBIOL", "BACTERIOL",  "Microbiol",
                                         "EPIDEMIOL", "Epidemiol", "Serv Alta Tecno",  "Aarhus Inst", "Aronoff Lab",
                                         "Charles Perkins", "Vet",  "Genom", "Enabling Technol",  "MED", "NUTR",  "PARASITOL",
                                         "Radiol",   "Neurogenet", "David Clark Labs",  "Dis Control", "Salud", "Med",  "NUCATS",
                                         "Pathol", "Canc", "Helmholtz Ctr",  "Karolinska Inst", "Gulbenkian Inst",  "Inst Vogelforsch",
                                         "Neurosci",  "Liggins Inst", "Haartman Inst", "Virol", "HUCH Lab", "Tech St Jerome",
                                         "Inst Butantan", "Forens", "Helmintol", "Salut", "Microorganisms", "Stress Adaptat",
                                         "Outbreak Anal", "Genet", "Verhaltensphysiol", "Parsitol", "INEUCI", "Inst Venezolano",
                                         "BABRAHAM INST", "NEISON LABS", "VERO BEACH LAB", "PATHOL", "REG HOSP",  "RADIAT",
                                         "NELSON LABS", "CORSON LAB", "CTR LOUIS EMBERGER",   "OPHTHALMOL", "PHARM", "BARNES LAB",
                                         "USDA", "Med", "Physiol"))))
Humanitieskeywords = c(unique(toupper(c("English", "ENGLISH", "Latin",  "LATIN", "LIBERAL ARTS",  "BUNTING INST", "LITERATURE",
                                        "BUNTING INST", "ARCHEOL",  "Arqueol"))))
Chemistrykeywords = c("Chem", "Chemistry", "Atomic", "Balcones")
EnvironmentalBiologykeywords = c(unique(toupper(c("Bio", "Bot", "Evol", "Univ Politecn Valencia, IAM", 
                                                  "Eco", "Ent", "Orn", "Zoo", "Plant", "Life", "Genet",
                                                  "Limn", "GENET", "Species", "SPECIES", "Aquat", "Vida", "VIDA", "Nat", "Trop", "Populat", "Paleo",
                                                  "Freshwater", "LIMN", "Bird", "IMMUNOECOL", "Nematol", "ANIM", "MARINE", "Ocean", "Reef", "ESTUAR",
                                                  "OCEAN", "REEF", "Oceanog", "Env", "Fish", "Management", "Forest", "WATERSHED", "Rivers", "Land", "Soil", "ICTIOL", "Water",
                                                  "CROP", "RANGE", "Hort", "Smithsonian", "Lake", "Aridas", "Grassland", "Appalachian Lab",
                                                  "Moreau Lab", "Alaska Sci Ctr", "Wetland", "TechnoSuruga Lab Co Ltd", "Field Sci",
                                                  "Florestais", "Ethol", "Lorenz",  "Foret", "Conservat", "Terradynam",
                                                  "LINCGlobal", "DNA Barcoding", "Volcani",  "Wild",  "Conservat",
                                                  "Fram",  "James Hutton", "Percy Fitzpatrick","BARNES LAB",
                                                  "Clodomiro Picado", "Forets", "CORAIL", "Recanati Kaplan", "Edward Grey",
                                                  "Farlington Ringing", "INDIE", "INSTAAR", "Leetown Sci",
                                                  "Arthur Rylah", "Florida Integrated", "Desert", "Mt Studies",
                                                  "LINC Global", "NSW Dept Primary Ind", "Brackenridge Field",
                                                  "Avancats Blanes", "Christchurch Sci Ctr", "Silvicultura", "Dendro",
                                                  "St Anthony Falls Lab", "Cryoptogamy",
                                                  "Ciencies Mar", "Herpetol", "Avanzados Blanes",
                                                  "Horn Point Lab", "Vegetal", "Lab Costero Calfuco", "Aquaculture", "Ag", "Alfred Wegener",
                                                  "Vogelwarte", "Mediterr",  "Dendrocronol", "Insect Sci",
                                                  "Phytol", "Rech Halieut", "Ichtyol", "Erken Lab", "Pesquisa Amazonia", "Louis Calder",
                                                  "Russell Labs", "ambient", "Hoffmann Inst", "Complutense",
                                                  "Ralph M Parsons",  "Tree Ring Lab",  "Ctr Wood",
                                                  "LINCGlobal", "Okol", "Verteb",  "Desert", "Meereskunde",
                                                  "Streamside Studies", "ARS CEREAL RUST", "coastal",
                                                  "BLACK MT LABS",  "PRAIRIE SCI", "COWEETA", "COMMUNITY DYNAM",
                                                  "ARBEITSGRP VERHALTENSFORSCH", "INSECT", "CONSERVAT", "AVIAN",
                                                  "SHRUB SCI LAB", "BELLE W BARUCH", "BOREAL INST", "EKOL",
                                                  "PESTICIDE LAB", "ASKO", "GREELEY MEM LAB", "BUCKHOUT",
                                                  "BOYCE THOMPSON INST", "Pasture Sci", "MARITIME", "Harbor", "sea"))))
SocialSciencekeywords = c(unique(toupper(c("Culture", "CULTURE", "POLITi", "PSYCHIAT", "Psychiat", "GOVT", "Govt",
                                           "ANTHROPOL", "Anthropol", "SOCIAL", "Social", "Law ", "LAW ", "Woodrow Wilson Sch",
                                           "Pondichery", "Saami Studies", "Ctr Noumea", "Sociol",
                                           "COMMERCE",  "BUSINESS"))))
Educationkeywords = c("Educ") #, "CURRICULUM") # resulted in poor matches
Architecturekeywords = c("Architecture")

# Categories ####
categories = c("EarthScience", "InterdisciplinaryComputing",
               "ComputerScience", "Math", "Education", "Engineering",
               "Humanities", "EnvironmentalBiology", "Chemistry", "LifeScience", 
               "Physics", "SocialScience", "Architecture")

categories = c("CS", "MA", "EG", "PS", "SS", "ES")

## Create the author database ####
authorCount = 0
smallerAuthorCount = 0
authorsTogether = c(0,0,0,0,0,0,0)
authorsTogetherLeaders = c(0,0,0,0,0,0,0)
papersShover = c(0, 0, 0, 0, 0, 0)
papersShoverPaps = c(rep(0, length(categories) + 2))
repeatOffenders = c(0,0,0,0,0,0)
lowYear = min(na.exclude(as.numeric(as.character(raw$YEAR))))
highYear = max(na.exclude(as.numeric(as.character(raw$YEAR))))
yearSaver = matrix(0, ncol = length(categories), nrow = highYear - lowYear + 1)
colnames(yearSaver) = c(1:length(categories)); row.names(yearSaver) = c(lowYear:highYear)
started = FALSE; origLen = 1; # allows us to take num rows of original zeroed vector 
patterning = c("\\[",'\\]') # allows us to split authors with affiliations according to
# WebOfScience formatting

for (jj in 2:length(raw$Affiliation1)) {
  if (!is.na(raw$Affiliation1[jj]) && raw$Affiliation1[jj] != "") {
    humptyDumpty = (c(unlist(strsplit(as.character(raw$Affiliation1[jj]), ""))))
    paperNum = raw$paper_ID[jj]
    affilNames = c(0)
    thisRow = c(rep(0,length(categories)))
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
          
          if (length(grep(paste(eval(as.name('axKeyword')), collapse = "|"), affilName,
                          fixed = FALSE, ignore.case = TRUE)) > 0) {
            affilType = 0
            affilCat = 0
          } else {
            for (z in 1:length(categories)) {# This needs to be moved below keywords
              if (length(grep(paste(eval(as.name(paste0(categories[z], "keywords"))), collapse = "|"),
                              affilName, fixed = FALSE, ignore.case = TRUE)) > 0) {
                affilType = z
                affilCat = categories[z] # select the appropriate affiliation
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

## Save author database as a dataframe, export as .csv ####
categories <- as.data.frame(as.table(papersShoverPaps)) #### <--- I don't think this output is right yet
write_csv(categories, './output_data/author_affiliations.csv')