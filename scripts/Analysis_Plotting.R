# Use assigned author affiliations to estimate and plot collaboration rates
# ("author_affiliations.csv", output from "Author_Categorization.R" script)

##### Load packages and data ####
# install.packages('pacman') 
pacman::p_load(tidyverse, cowplot) # Install and load libraries
author_affiliations <- read_csv('./output_data/author_affiliations.csv')

raw <-  read.csv("./raw_data/Ecology_FullRecords.csv", na.strings = c("", "NA")) %>% # Load Web of Science entries
  select(paper_ID, AUTHOR2, Affiliation1, Affiliation2, YEAR, DOI) %>%  # Retain only these columns
  rename(AUTHOR = AUTHOR2)

##### Calculate author and paper metrics ####
original_papers <- raw %>% # Load Web of Science records
  filter(!is.na(Affiliation1) | !is.na(Affiliation2)) %>% # Omit papers with no affiliation information
  filter(YEAR < 2017) # Omit 2017 papers (1972 - 2016 only)

# Number of papers from Web of Science that can be linked to affiliations
print(n_distinct(original_papers$paper_ID)) # ("total articles analyzed" in results)

papers_retained <- author_affiliations %>% 
  filter(AffiliationGroup != "Remove")   # Omit gov't or industry authors

# Number of papers with complete, non-"Remove" author affiliations
print(n_distinct(papers_retained$Paper_ID)) # (Number included in affiliation analysis)

# Total authors with academic affiliations
print(n_distinct(papers_retained$Author)) 

# Count affiliations per paper per year
affiliation_groups <- papers_retained %>% 
  group_by(Year, AffiliationGroup) %>%
  count(AffiliationGroup, Paper_ID) %>% # Count authors per affiliation for each paper
  rename(Author_Count = n) %>%
  select(Paper_ID, Year, AffiliationGroup, Author_Count)

# Estimate mean number of authors per paper in 1st and last decade of analysis
Authors_over_time <- affiliation_groups %>%
  group_by(Paper_ID, Year) %>%
  summarize(Authors_per_paper = sum(Author_Count)) %>%
  mutate(Decade = ifelse(Year < 1983, "Start", 
                     ifelse(Year > 2005, "End", NA))) %>%
  group_by(Decade) %>%
  summarize(Mean_Author_Count = round(mean(Authors_per_paper, na.rm=T),1), 
            SD = round(sd(Authors_per_paper, na.rm=T),1), 
            Papers = n_distinct(Paper_ID),
            SE = round(SD / sqrt(Papers)),2)

# Total papers per year: 1972-2016
Papers_per_year <- affiliation_groups %>%
  group_by(Year) %>%
  summarize(Total_Annual_Papers = n_distinct(Paper_ID))

# PaperIDs that include a CS author; count of CS authors for those papers
CS_authored_papers <- papers_retained %>%
  filter(AffiliationGroup == 'CS')

# Number of papers with a CS-affiliated author
print(n_distinct(CS_authored_papers$Paper_ID)) 

# Number of CS-affiliated authors
CS_authored_papers %>% summarize(n())

# Number of distinct CS-affiliated authors
print(n_distinct(CS_authored_papers$Author_ID)) 

# Number of CS-affiliated papers by year
CS_papers_by_year <- CS_authored_papers %>%
  group_by(Year) %>%
  summarize(Annual_CS_Papers = n_distinct(Paper_ID))

# Group each paper by author affiliations ####
# Authors per affiliation group for each paper
affiliation_combos <- affiliation_groups %>%
  spread(key = AffiliationGroup, value = Author_Count)

# Count papers per year with collaboration between Comp Sci and ES ####
CS_collaborations <- affiliation_combos %>% # Comp Sci and Env Sci
  filter(CS >= 1 & ES >= 1) %>% summarize(CSES = n())

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
  full_join(., (affiliation_combos %>%                    # Env Sci and Social Sci
                  filter(ES >= 1) %>% 
                  summarize(ESES = n()))) %>%
  arrange(Year)%>%
  mutate(Total_Collab_Papers = rowSums(.[2:6], na.rm=T),
         NonCS_Collab_Papers = Total_Collab_Papers - ESCS) %>% # Sum of collaborative papers per year
  full_join(., Papers_per_year) %>% # Add column of total papers per year
  mutate(ES_Only_Papers = Total_Annual_Papers - Total_Collab_Papers)

# Sum ES collaborations with each other category 
collaboration_proportions <- ES_collaborations %>%
  summarise_all(sum, na.rm=T) %>%
  mutate(ESMA_prop = round(ESMA / Total_Annual_Papers *100,1),
         ESSS_prop = round(ESSS / Total_Annual_Papers*100,1),
         ESPS_prop = round(ESPS / Total_Annual_Papers*100,1), 
         ESEG_prop = round(ESEG / Total_Annual_Papers*100,1))

# Estimate proportion of papers that are collaborative between ES and anyone non-ES 
Interdisc_collab <- ES_collaborations %>%
  group_by(Year) %>%
  #mutate(Percent_collab = Total_Collab_Papers / Total_Annual_Papers) %>%
  mutate(Decade = ifelse(Year < 1983, "Start", 
                         ifelse(Year > 2005, "End", NA))) %>%
  group_by(Decade) %>%
  summarize(Mean_Collab_Papers = round(mean(Total_Collab_Papers, na.rm=T),1),
            Mean_Total_Papers = round(mean(Total_Annual_Papers, na.rm=T),1),
            Prop_Collab = round((Mean_Collab_Papers / Mean_Total_Papers * 100),1))
  

ES_collab_frequency <- ES_collaborations %>%
             gather(key = Collaboration, value = Num_Collab_Papers, ESCS:ESSS, 
                    -(Total_Collab_Papers:ES_Only_Papers)) %>%
             mutate(RelativeFreq = Num_Collab_Papers / Total_Collab_Papers,
                    OverallFreq = Num_Collab_Papers / Total_Annual_Papers,
                    Collaboration = factor(Collaboration,
                                           levels=c("ESMA", "ESEG", "ESPS", "ESSS", "ESCS"), 
                                           labels=c("Mathematics", "Engineering", "Physical Science",
                                                    "Social Science", "Computer Science")))


# Plot setup ####
mytheme <- theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 axis.text=element_text(size=16, colour = "black"), axis.title=element_text(size=18, colour = "black"), 
                 legend.text = element_text(size = 14), legend.title = element_text(size = 16), legend.title.align = 0.5)

colors = c("gray20", "gray35", "gray60", "gray75", "red") # Red for CS collaborations

# Plot total papers and collaborative papers per year ####
prop_collab <- left_join(ES_collaborations, CS_papers_by_year) %>% 
  select(Year, Total_Collab_Papers, NonCS_Collab_Papers, Total_Annual_Papers, Annual_CS_Papers) %>%
  gather(Metric, Value, Total_Collab_Papers:Annual_CS_Papers) %>%
  mutate(Metric = factor(Metric, levels= c("Total_Annual_Papers", "Total_Collab_Papers", "NonCS_Collab_Papers", "Annual_CS_Papers"), 
                                           labels = c("Total Analyzed", "Total Interdisciplinary", "Non-Computer Science Collaborative", 
                                                      "Computer Science")))

# Paper Figure 1A
a <- ggplot(subset(prop_collab, Metric != "Non-Computer Science Collaborative"), aes(x = Year, y = Value, col = Metric)) + mytheme + 
  geom_line(lwd = 1.05) + 
  geom_point(size = 2) +
  #geom_smooth() +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0,400, 50)) +
  scale_x_continuous(limits = c(1973, 2016), breaks=seq(1975,2015,5)) + 
  scale_colour_manual(values = c('black', 'gray50', 'red')) +
  labs(y = expression(paste("Papers analyzed from ", italic("Ecology")))) +
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
  labs(x = "Year", y = "Proportion of interdisciplinary papers") + 
  scale_x_continuous(expand = c(0,0), limits = c(1971, 2016), breaks=seq(1975,2015,5)) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual("Discipline", values = colors) 

# Frequency within Collaborative Papers: Env Sci and other disciplines 
ggplot(ES_collab_frequency, aes(x = Year, y = OverallFreq, fill = Collaboration)) + mytheme + 
  geom_col() + 
  labs(x = "Year", y = "Proportion of analyzed papers") + 
  scale_x_continuous(expand = c(0,0), limits = c(1973, 2016), breaks=seq(1975,2015,5)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,0.25)) + 
  scale_fill_manual("Discipline", values = colors) 

# Figure 1 panel: ####
plot_grid(a, b, nrow = 1, labels = c('A', 'B'), rel_widths = c(1, 1.4))
