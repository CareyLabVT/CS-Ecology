#' Use categorized author affiliations to estimate and plot collaboration rates
#' ("author_affiliations.csv", output from "Author_Categorization.R" script)
#' Written by AIK and KJF, last updated 6 Feb 2018

### Load packages and data file ####
# install.packages('pacman') 
pacman::p_load(tidyverse) # Install and load libraries
library(reshape2)

auth <- read_csv('./output_data/author_affiliations.csv') # Load author categorizations

## Analysis of Env Biologist Collaborations Over Time #### 
collabs = 0
collabsrecent = 0
years = sort(unique(as.numeric(papersShoverPaps[,2])))[sort(unique(as.numeric(papersShoverPaps[,2]))) != 0 &
                                                         sort(unique(as.numeric(papersShoverPaps[,2]))) != 2017]
countemup = matrix(0, ncol = length(years), nrow = 3)
countemupCSES = matrix(0, ncol = length(years), nrow = 3) # CS person with ecologist
countemupMAES = matrix(0, ncol = length(years), nrow = 3) # math person with ecologist
countemupEGES = matrix(0, ncol = length(years), nrow = 3) # engineering person with ecologist
countemupPSES = matrix(0, ncol = length(years), nrow = 3) # math person with ecologist
for (g in 2:nrow(papersShoverPaps)) {
  if (sum(papersShoverPaps[g,3]) != 0 && sum(papersShoverPaps[g,8]) != 0 # && papersShoverPaps[g,8] != 0 && !is.na(papersShoverPaps[g, 2]) 
      && papersShoverPaps[g, 2] - years[1]  + 1 <= ncol(countemup)) {
    collabs = collabs + 1
    countemupCSES[1, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] = 
      countemupCSES[1, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] + 1
  }
  
  if (sum(papersShoverPaps[g,4]) != 0 && sum(papersShoverPaps[g,8]) != 0 # && papersShoverPaps[g,8] != 0 && !is.na(papersShoverPaps[g, 2]) 
      && papersShoverPaps[g, 2] - years[1]  + 1 <= ncol(countemup)) {
    collabs = collabs + 1
    countemupMAES[1, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] = 
      countemupMAES[1, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] + 1
  }
  
  if (sum(papersShoverPaps[g,5]) != 0 && sum(papersShoverPaps[g,8]) != 0 # && papersShoverPaps[g,8] != 0 && !is.na(papersShoverPaps[g, 2]) 
      && papersShoverPaps[g, 2] - years[1]  + 1 <= ncol(countemup)) {
    collabs = collabs + 1
    countemupEGES[1, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] = 
      countemupEGES[1, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] + 1
  }
  
  if (sum(papersShoverPaps[g,6]) != 0 && sum(papersShoverPaps[g,8]) != 0 # && papersShoverPaps[g,8] != 0 && !is.na(papersShoverPaps[g, 2]) 
      && papersShoverPaps[g, 2] - years[1]  + 1 <= ncol(countemup)) {
    collabs = collabs + 1
    countemupPSES[1, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] = 
      countemupPSES[1, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] + 1
  }
  
  if (!is.na(papersShoverPaps[g, 2]) && (papersShoverPaps[g, 2] - years[1]  + 1) <= ncol(countemup)) {
    countemupCSES[2, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] = # set all second rows to be total # ES
      countemupMAES[2, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] =
      countemupEGES[2, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] =
      countemupPSES[2, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] =
      countemup[2, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] =
      countemup[2, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] + 1
    collabsrecent = collabsrecent + sum(papersShoverPaps[g, 3: 7])#8])
    countemupCSES[3, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] = # set all second rows to be total # non-ES
      countemupMAES[3, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] =
      countemupEGES[3, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] =
      countemupPSES[3, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] =
      countemup[3, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] =
      countemup[3, as.numeric(papersShoverPaps[g, 2]) - years[1] + 1] + sum(papersShoverPaps[g, 3:7])#8])
  }
}

# Get mean within different timeframes #
fromVar = 'CS' # enter relationship you want with Env Sci
toVar = 'ES' # don't change this 
pastYears = 6 # number of years prior to 2016 you consider recent
oldYears = 8 # number of years to look at from the start of the dataset
means = eval(as.name(paste0('countemup', fromVar, toVar)))[1,] /
  eval(as.name(paste0('countemup', fromVar, toVar)))[2,] # create mean of ES with some other var
print(paste0("The overall mean of papers with authors from ", fromVar, " with ", toVar, " is:"))
print(mean(means))
meansrecent = eval(as.name(paste0('countemup', fromVar, toVar)))[1, (ncol(countemup) - pastYears):ncol(countemup)] /
  eval(as.name(paste0('countemup', fromVar, toVar)))[2, (ncol(countemup) - pastYears):ncol(countemup)] 

print(paste0("The mean of papers with authors from ", fromVar, " with ", toVar, " in the last ", pastYears, " years is:"))
print(mean(meansrecent))
meansold = eval(as.name(paste0('countemup', fromVar, toVar)))[1, 1:oldYears] /
  eval(as.name(paste0('countemup', fromVar, toVar)))[2, 1:oldYears] 
print(paste0("The mean of papers with authors from ", fromVar, " with ", toVar, " in the first ", oldYears, " years is:"))
print(mean(meansold))

# Change if different # categories
sums = c(sum(papersShoverPaps[,2]), sum(papersShoverPaps[,3]), sum(papersShoverPaps[,4]), sum(papersShoverPaps[,5]), sum(papersShoverPaps[,6]), sum(papersShoverPaps[,7]))
sumsnoenv = c(sum(papersShoverPaps[,2]), sum(papersShoverPaps[,3]), sum(papersShoverPaps[,4]), sum(papersShoverPaps[,5]), sum(papersShoverPaps[,6]))


## Plot outputs ## <-- subheading where we'll organize our plots!
plot(as.numeric(rownames(yearSaver))[1:nrow(yearSaver) - 1], yearSaver[(1:nrow(yearSaver) - 1), 2], col = colors[1], pch = 20, ylab = "Frequency", xlab = "Year")
for (i in 2:(ncol(yearSaver) - 1)) {
  points(yearSaver[(1:nrow(yearSaver) - 1), i + 1] ~ as.numeric(rownames(yearSaver))[1:nrow(yearSaver) - 1], col = colors[i], pch = 20)
  lines(rowsum(yearSaver[1:nrow(yearSaver) - 1,]) ~ as.numeric(rownames(yearSaver))[1:nrow(yearSaver) - 1])
}
legend("topleft", categories[1:length(categories) - 1], fill = colors)

sumsnoES = sum(yearSaver[1,1:5])
for (i in 2:(nrow(yearSaver) - 1)) {
  sumsnoES = c(sumsnoES, sum(yearSaver[i,1:5]))
}
holder = yearSaver[1:nrow(yearSaver) - 1,1:5]
holder = holder / sumsnoES
holder = holder[6:nrow(holder),]
colnames(holder) = c(categories[1:length(categories) - 1])
holder2 = melt(holder) # should use "gather" function from tidyverse to minimize number of packages used (so no reshape2 needed)

colorsnew = c(gray.colors(4, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL), "red")
colorsnew[4] = "white"
colornums = c(0)
for (t in 1:nrow(holder2)) {
  colornums = c(colornums, colorsnew[holder2$Var2[t] == categories])
}
holder2$cols = colornums[2:length(colornums)]

holder2 <- within(holder2, Var2 <- factor(Var2, levels = c("MA", "EG", "PS", "SS", "CS")))
levels(holder2$Var2) = c("Math", "Engineering", "Physical Science", "Human Sciences", "Computer Science")
colnames(holder2)[colnames(holder2) == "Var2"] = "Affiliation"
g = ggplot(holder2, aes(x = Var1, y = value, fill = Affiliation), colour = cols)
g + geom_col(colour = "black", show.legend = TRUE) + 
  ylab('Relative frequency per year') + xlab('Year') + scale_fill_manual(values = rep(colorsnew, length(holder2$cols) / 
                                                                                        length(colorsnew))) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18), legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16), legend.title.align = 0.5)
theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
