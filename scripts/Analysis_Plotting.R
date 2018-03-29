#' Use categorized author affiliations to estimate and plot collaboration rates
#' ("author_affiliations.csv", output from "Author_Categorization.R" script)
#' Written by AIK and KJF, last updated 29 March 2018

#### Load packages and data file ####
# install.packages('pacman') 
pacman::p_load(tidyverse) # Install and load libraries

authors <- read_csv('./output_data/author_affiliations.csv') # Load author categorizations

#### Analysis of Env Biologist Collaborations Over Time #### 

# Select for focal years
# Retain only unique Affiliation groups for a given paper 
affil_hist = authors %>%
             filter(Year >= 1969 & Year <= 2016) %>%
             group_by(Year, AffiliationGroup) %>%
             distinct(AffiliationGroup, Paper_ID)

# This keeps track of all the CS authors
affil_CS = affil_hist %>%
           filter(AffiliationGroup == "CS")

# Spread the data out into a table format with columns as affils 
affil_combos = affil_hist %>%
               count(Paper_ID) %>% #, Year, AffiliationGroup) %>%
               spread(key = AffiliationGroup, value = n)
  
# Dataframe of just nonzero CS-ES collaborations counted by year
yearly_counts = affil_combos %>% 
                group_by(Year) %>%
                filter(CS == 1) %>%
                filter(ES == 1) %>%
                summarize(ct = n())

# Counts of CS and then CS with other disciplines 
tallied_CS = affil_combos %>%
  group_by(Year) %>%
  count(CS) %>%
  filter(CS == 1) %>%
  rename(sumCS = n)

tallied_CSES = affil_combos %>%
          count(CS, ES) %>%
          filter(CS == 1 & ES == 1) %>%
          rename(CSES = n)
tallied_CSEG = affil_combos %>%
          count(CS, EG) %>%
          filter(CS == 1 & EG == 1) %>%
          rename(CSEG = n)
tallied_CSMA = affil_combos %>%
          count(CS, MA) %>%
          filter(CS == 1 & MA == 1) %>%
          rename(CSMA = n)
tallied_CSPS = affil_combos %>%
          count(CS, PS) %>%
          filter(CS == 1 & PS == 1) %>%
          rename(CSPS = n)
tallied_CSSS = affil_combos %>%
          count(CS, SS) %>%
          filter(CS == 1 & SS == 1) %>%
          rename(CSSS = n)

# Pulls together CS with all other disciplines 
CSandothers = tallied_CSES %>%
              full_join(tallied_CSEG) %>%
              full_join(tallied_CSMA) %>%
              full_join(tallied_CSPS) %>%
              full_join(tallied_CSSS) %>%
              select(c(-CS, -ES, -EG, -MA, -PS, -SS))

# Same process for Environmental Biologists collaborating with other disciplines 
tallied_ESCS = affil_combos %>%
  group_by(Year) %>%
  count(ES, CS) %>%
  filter(ES == 1 & CS == 1) %>%
  rename(ESCS = n)
tallied_ESEG = affil_combos %>%
  count(ES, EG) %>%
  filter(ES == 1 & EG == 1) %>%
  rename(ESEG = n)
tallied_ESMA = affil_combos %>%
  count(ES, MA) %>%
  filter(ES == 1 & MA == 1) %>%
  rename(ESMA = n)
tallied_ESPS = affil_combos %>%
  count(ES, PS) %>%
  filter(ES == 1 & PS == 1) %>%
  rename(ESPS = n)
tallied_ESSS = affil_combos %>%
  count(ES, SS) %>%
  filter(ES == 1 & SS == 1) %>%
  rename(ESSS = n)

ESandothers = tallied_ESCS %>%
  full_join(tallied_ESEG) %>%
  full_join(tallied_ESMA) %>%
  full_join(tallied_ESPS) %>%
  full_join(tallied_ESSS) %>%
  select(c(-CS, -ES, -EG, -MA, -PS, -SS)) %>%
  mutate(Sum = sum(ESEG, ESMA, ESPS, ESSS, ESCS, na.rm = TRUE))

gatheredES = ESandothers %>%
             gather(key = ESMatch, value = count,
                      ESCS, ESEG, ESMA, ESPS, ESSS, -Sum) %>%
             mutate(relfreq = count / Sum)

colorsnew = c("red", gray.colors(4, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)) # grayscale for non-CS, red for CS
#colorsnew[5] = "white" # can change individual colors 
names = unique(gatheredES$ESMatch)
colsinds = colorsnew[which(gatheredES$ESMatch == names)]

# Create histogram of environmental biologists working with other disciplines 
t = ggplot(gatheredES, aes(x = Year, y = relfreq, fill = ESMatch))
t+ geom_col(show.legend = TRUE) + ylab('Relative frequency per year') + xlab('Year') + theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + scale_fill_manual("Legend", values = colorsnew) + 
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18), legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), legend.title.align = 0.5)