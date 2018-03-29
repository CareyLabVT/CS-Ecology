#' Use categorized author affiliations to estimate and plot collaboration rates
#' ("author_affiliations.csv", output from "Author_Categorization.R" script)
#' Written by AIK and KJF, last updated 29 March 2018

# Load packages ####
# install.packages('pacman') 
pacman::p_load(tidyverse, cowplot) # Install and load libraries

# Load author categorization and count affiliations per paper #### 
affiliation_groups <- read_csv('./output_data/author_affiliations.csv') %>% # Load author categorizations
  filter(Year >= 1969 & Year <= 2016) %>% # Select for focal years
  group_by(Year, AffiliationGroup) %>%
  count(AffiliationGroup, Paper_ID) %>% # Count authors per affiliation for each paper
  rename(Author_Count = n)

# Calculate papers per year: total papers and papers with a CS author ####
# Total papers per year: 1972-2016
Papers_per_year <- affiliation_groups %>%
  group_by(Year) %>%
  summarize(Total_Annual_Papers = n_distinct(Paper_ID))

# PaperIDs that include a CS author; count of CS authors for those papers
CS_authored_papers <- affiliation_groups %>%
           filter(AffiliationGroup == "CS")

CS_papers_by_year <- CS_authored_papers %>%
  group_by(Year) %>%
  summarize(Annual_CS_Papers = n_distinct(Paper_ID))

# Group each paper by author affiliations
# Authors per affiliation for each paper
affiliation_combos <- affiliation_groups %>%
  spread(key = AffiliationGroup, value = Author_Count)

# Count papers per year with collaboration between Comp Sci and other disciplines ####
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
                  summarize(CSSS = n()))) %>%
  mutate(Total_Collab_Papers = rowSums(.[2:6], na.rm=T)) # Sum of collaborative papers per year

# Count papers per year with collaboration between Env Sci and other disciplines ####
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
  mutate(Total_Collab_Papers = rowSums(.[2:6], na.rm=T)) %>% # Sum of collaborative papers per year
  full_join(., Papers_per_year) 

ES_collab_frequency <- ES_collaborations %>%
             gather(key = Collaboration, value = Num_Collab_Papers, ESCS:ESSS, 
                    -(Total_Collab_Papers:Total_Annual_Papers)) %>%
             mutate(RelativeFreq = Num_Collab_Papers / Total_Collab_Papers,
                    OverallFreq = Num_Collab_Papers / Total_Annual_Papers,
                    Collaboration = factor(Collaboration,
                                           levels=c("ESMA", "ESEG", "ESPS", "ESSS", "ESCS"), 
                                           labels=c("Math", "Engineering", "Physical Science",
                                                    "Social Science", "Computer Science")))


# Plot setup ####
mytheme <- theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 axis.text=element_text(size=16, colour = "black"), axis.title=element_text(size=18, colour = "black"), 
                 legend.text = element_text(size = 14), legend.title = element_text(size = 16), legend.title.align = 0.5)

colors = c("gray20", "gray35", "gray60", "gray75", "red") # Red for CS collaborations

# Plot total papers and collaborative papers per year ####
prop_collab <- left_join(ES_collaborations, CS_papers_by_year) %>% 
  select(Year, Total_Collab_Papers, Total_Annual_Papers, Annual_CS_Papers) %>%
  gather(Metric, Value, Total_Collab_Papers:Annual_CS_Papers) %>%
  mutate(Metric = factor(Metric, levels= c("Total_Annual_Papers", "Total_Collab_Papers", "Annual_CS_Papers"), 
                                           labels = c("Total", "Total Collaborative", 
                                                      "Computer Science Collaborative")))

# Paper Figure 1A
a <- ggplot(prop_collab, aes(x = Year, y = Value, col = Metric)) + mytheme + 
  geom_line(lwd = 1.05) + 
  geom_point(size = 2) +
  #geom_smooth() +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0,400, 50)) +
  scale_x_continuous(limits = c(1973, 2016), breaks=seq(1975,2015,5)) + 
  scale_colour_manual(values = c('black', 'gray50', 'red')) +
  labs(y = expression(paste("Papers published in", italic("Ecology")))) +
  theme(legend.position = c(0,1), legend.justification = c(0,1), legend.title = element_blank()) 

ggplot(ES_collaborations, aes(x = Year, y =  round((Total_Collab_Papers / Total_Annual_Papers), 2))) + mytheme + 
  geom_point(size = 2) +
  geom_smooth(color= 'black') +
  scale_y_continuous(limits = c(0, 0.25)) +
  scale_x_continuous() + 
  labs(y = expression(paste("Proportion of papers published in", italic("Ecology"))), title = "Collaborative Papers")

# Plot frequencies of collaborations among groups ####
# Frequency of collaborations among all papers: Figure 1B
b <- ggplot(ES_collab_frequency, aes(x = Year, y = RelativeFreq, fill = Collaboration)) + mytheme + 
  geom_col() + 
  labs(x = "Year", y = "Proportion of collaborative papers") + 
  scale_x_continuous(expand = c(0,0), limits = c(1973, 2016), breaks=seq(1975,2015,5)) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual("Discipline", values = colors) 

# Frequency within Collaborative Papers: Env Sci and other disciplines 
ggplot(ES_collab_frequency, aes(x = Year, y = OverallFreq, fill = Collaboration)) + mytheme + 
  geom_col() + 
  labs(x = "Year", y = "Overall frequency") + 
  scale_x_continuous(expand = c(0,0), limits = c(1973, 2016), breaks=seq(1975,2015,5)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,0.25)) + 
  scale_fill_manual("Discipline", values = colors) 


# Figure 1 panel: ####
plot_grid(a, b, nrow = 1, labels = c('A', 'B'), rel_widths = c(1, 1.4))
