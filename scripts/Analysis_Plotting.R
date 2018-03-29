#' Use categorized author affiliations to estimate and plot collaboration rates
#' ("author_affiliations.csv", output from "Author_Categorization.R" script)
#' Written by AIK and KJF, last updated 29 March 2018

#### Load packages ####
# install.packages('pacman') 
pacman::p_load(tidyverse) # Install and load libraries

#### Analysis of Collaborations Over Time #### 
affiliation_groups <- read_csv('./output_data/author_affiliations.csv') %>% # Load author categorizations
  filter(Year >= 1969 & Year <= 2016) %>% # Select for focal years
  group_by(Year, AffiliationGroup) %>%
  count(AffiliationGroup, Paper_ID) %>% # Count authors per affiliation for each paper
  rename(Author_Count = n)

# PaperIDs that include a CS author; count of CS authors for those papers
CS_authored_papers <- affiliation_groups %>%
           filter(AffiliationGroup == "CS")

CS_papers_by_year <- CS_authored_papers %>%
  group_by(Year) %>%
  summarize(Annual_CS_Papers = n_distinct(Paper_ID))

# Authors per affiliation for each paper
affiliation_combos = affiliation_groups %>%
  spread(key = AffiliationGroup, value = Author_Count)

# Count number of papers per year with collaboration between CS and other disciplines 
CS_collaborations <- left_join((affiliation_combos %>%    # Comp Sci and Env Sci
                           filter(CS >= 1 & ES >= 1) %>% 
                           summarize(CSES = n())), 
                        (affiliation_combos %>%           # Comp Sci and Eng
                           filter(CS >= 1 & EG >= 1) %>% 
                           summarize(CSEG = n()))) %>%
  left_join(., (affiliation_combos %>%                    # Comp Sci and Math
                  filter(CS >= 1 & MA >=1) %>% 
                  summarize(CSMA = n()))) %>%
  left_join(., (affiliation_combos %>%                    # Comp Sci and Physics
                  filter(CS >= 1 & PS >=1) %>% 
                  summarize(CSPS = n()))) %>%
  left_join(., (affiliation_combos %>%                    # Comp Sci and Social Sci
                  filter(CS >= 1 & SS >=1) %>% 
                  summarize(CSSS = n())))

# Same process for Environmental Biologists collaborating with other disciplines 
tallied_ESCS = affiliation_combos %>%
  group_by(Year) %>%
  count(ES, CS) %>%
  filter(ES == 1 & CS == 1) %>%
  rename(ESCS = n)
tallied_ESEG = affiliation_combos %>%
  count(ES, EG) %>%
  filter(ES == 1 & EG == 1) %>%
  rename(ESEG = n)
tallied_ESMA = affiliation_combos %>%
  count(ES, MA) %>%
  filter(ES == 1 & MA == 1) %>%
  rename(ESMA = n)
tallied_ESPS = affiliation_combos %>%
  count(ES, PS) %>%
  filter(ES == 1 & PS == 1) %>%
  rename(ESPS = n)
tallied_ESSS = affiliation_combos %>%
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