#' Use categorized author affiliations to estimate and plot collaboration rates
#' ("author_affiliations.csv", output from "Author_Categorization.R" script)
#' Written by AIK and KJF, last updated 29 March 2018

# Load packages ####
# install.packages('pacman') 
pacman::p_load(tidyverse) # Install and load libraries

# Analysis of Collaborations Over Time #### 
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
affiliation_combos <- affiliation_groups %>%
  spread(key = AffiliationGroup, value = Author_Count)

# Count number of papers per year with collaboration between CS and other disciplines ####
CS_collaborations <- full_join((affiliation_combos %>%    # Comp Sci and Env Sci
                           filter(CS >= 1 & ES >= 1) %>% 
                           summarize(CSES = n())), 
                        (affiliation_combos %>%           # Comp Sci and Eng
                           filter(CS >= 1 & EG >= 1) %>% 
                           summarize(CSEG = n()))) %>%
  full_join(., (affiliation_combos %>%                    # Comp Sci and Math
                  filter(CS >= 1 & MA >=1) %>% 
                  summarize(CSMA = n()))) %>%
  full_join(., (affiliation_combos %>%                    # Comp Sci and Physics
                  filter(CS >= 1 & PS >=1) %>% 
                  summarize(CSPS = n()))) %>%
  full_join(., (affiliation_combos %>%                    # Comp Sci and Social Sci
                  filter(CS >= 1 & SS >=1) %>% 
                  summarize(CSSS = n())))

# Count number of papers per year with collaboration between Env. Sci and other disciplines ####
ES_collaborations <- full_join((affiliation_combos %>%    # Env Sci and Comp Sci
                                  filter(ES >= 1 & CS >= 1) %>% 
                                  summarize(ESCS = n())), 
                               (affiliation_combos %>%    # Env Sci and Eng
                                  filter(ES >= 1 & EG >= 1) %>% 
                                  summarize(ESEG = n()))) %>%
  full_join(., (affiliation_combos %>%                    # Env Sci and Math
                  filter(ES >= 1 & MA >=1) %>% 
                  summarize(ESMA = n()))) %>%
  full_join(., (affiliation_combos %>%                    # Env Sci and Physics
                  filter(ES >= 1 & PS >=1) %>% 
                  summarize(ESPS = n()))) %>%
  full_join(., (affiliation_combos %>%                    # Env Sci and Social Sci
                  filter(ES >= 1 & SS >=1) %>% 
                  summarize(ESSS = n()))) %>%
  mutate(Total_Collab_Papers = rowSums(.[2:6], na.rm=T))

gatheredES <- ES_collaborations %>%
             gather(key = Collaboration, value = Num_Papers, ESCS:ESSS, -Total_Collab_Papers) %>%
             mutate(RelativeFreq = Num_Papers / Total_Collab_Papers)

# Plot frequencies of collaborations among groups ####
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