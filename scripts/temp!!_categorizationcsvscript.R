#' Converts my original setup to columns of keywords CSV setup 
#' 
#' Written by Arianna Krinos, last edits on 3 Feb 2018

keywords_messing = read.csv("./raw_data/keyword_categories.csv")

## EXAMPLE #### 
# index = 1
# for (i in 1:length(tolower(EarthSciencekeywords))) {
#   if (!any(keywords_messing$EarthScience == tolower(EarthSciencekeywords[i]))) {
#     if (index > nrow(keywords_messing)) {
#       keywords_messing = rbind(keywords_messing, c(rep(NA, ncol(keywords_messing))))
#     }
#     keywords_messing$EarthScience[index] = tolower(EarthSciencekeywords[i])
#     index = index + 1
#   }
# }
categories = c("ax", "EarthScience", "InterdisciplinaryComputing",
               "ComputerScience", "Math", "Education", "Engineering",
               "Humanities", "EnvironmentalBiology", "Chemistry", "LifeScience", 
               "Physics", "SocialScience", "Architecture")

keywords_messing = c(rep(NA, length(categories)))
keywords_messing = rbind(keywords_messing, keywords_messing)
for (j in 1:length(categories)) {
  index = 3
  currCat = categories[j]
  colCtr = j
  for (i in 1:length(tolower(eval(as.name(paste0(currCat, "keywords")))))) {
    #if (colCtr > length(keywords_messing) || colCtr > ncol(keywords_messing)) {
    #  keywords_messing = cbind(keywords_messing, (c(rep(NA, nrow(keywords_messing)))))
    #  colnames(keywords_messing)[ncol(keywords_messing)] = currCat
    #}
    if (all(is.na(keywords_messing[,colCtr])) ||
        (!any(na.exclude(keywords_messing[,colCtr]) == tolower(eval(as.name(paste0(currCat, "keywords")))[i])) &&
        !any(grepl(paste0(na.exclude(keywords_messing[,colCtr]), collapse = "|"), tolower(eval(as.name(paste0(currCat, "keywords")))[i]))))) {
        #&& !any(grepl(tolower(eval(as.name(paste0(currCat, "keywords")))[i]), (na.exclude(keywords_messing[,colCtr])))))) {
      if (index > nrow(keywords_messing)) {
        keywords_messing = rbind(keywords_messing, c(rep(NA, ncol(keywords_messing))))
      }
      keywords_messing[index, colCtr] = tolower(eval(as.name(paste0(currCat, "keywords")))[i])
        #gsub(" ", "", tolower(eval(as.name(paste0(currCat, "keywords")))[i])) # if you want to remove spaces
      index = index + 1
    }
  }
}

keywords_messing = keywords_messing[3:nrow(keywords_messing),]

colnames(keywords_messing) = categories
colnames(keywords_messing)[1] = "Remove"
keywords_messing = data.frame(keywords_messing)

keywords_messing_2 = c(0,0)
for (g in 1:(nrow(keywords_messing))) {
  for (f in 1:ncol(keywords_messing)) {
    if (!is.na(as.character(keywords_messing[g,f]))) {
      keywords_messing_2 = rbind(keywords_messing_2, c(colnames(keywords_messing)[f], as.character(keywords_messing[g,f])))
    }
  }
}
keywords_messing_2 = keywords_messing_2[2:nrow(keywords_messing_2),]
colnames(keywords_messing_2) = c("category", "keyword")
keywords_messing_2[,2] = gsub(",", "[comma]", keywords_messing_2[,2])
write.csv(keywords_messing_2, "./raw_data/keyword_categories.csv", row.names = FALSE, quote = FALSE)


# NOTES - 
# Removed "theor" from env bio - prevents CS affiliations for modeling centers 
# changed "phys" to "phys " to avoid getting so many physiol - still going to get things like phys anthropol 
keywords_messing_3 = as.data.frame(keywords_messing_2)
#write.table(keywords_messing_2, file = "./raw_data/keyword_categories2.csv", sep = "\t", row.names = FALSE, col.names = c("category", "keyword"))
axkeywords = c("British Antarctic Survey", "USGS", "Geological Survey", "Kennedy Space", "Fish and Wildlife",
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
Physicskeywords = c("Phys ", "PHYS ", "Langmuir", "LANGMUIR",  "Gleb Wataghin", "Reg Bariloche",
                    "Inst Balseiro", "Ctr Atom Bariloche",  "Nucl", "NUCL", "isotrace", "Inst adv") # removed "Planck Inst"
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
