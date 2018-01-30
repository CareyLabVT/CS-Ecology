# Use categorized author affiliations to estimate and plot collaboration rates
  # ("CSEcol_Affiliations.RData"; from "Author_Categorization.R" script)
  # Written by AIK and KJF, last updated 30 Jan 2018

## Load packages and .RData workspace ####

# install.packages('pacman') 
pacman::p_load(tidyverse) # Install and load libraries

load('./CSEcol_Affiliations.RData') # Load file of CS-Ecology author categorizations

## Analysis of XXX ####
# Change if different # categories
sums = c(sum(papersShoverPaps[,2]), sum(papersShoverPaps[,3]), sum(papersShoverPaps[,4]), sum(papersShoverPaps[,5]), sum(papersShoverPaps[,6]), sum(papersShoverPaps[,7]))
sumsnoenv = c(sum(papersShoverPaps[,2]), sum(papersShoverPaps[,3]), sum(papersShoverPaps[,4]), sum(papersShoverPaps[,5]), sum(papersShoverPaps[,6]))
colors = c("blue", "yellow", "green", "orange", "pink")
colors = c("red", "green", "purple", "dark blue", "dark green")

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
holder2 = melt(holder)

colorsnew = c(gray.colors(4, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL), "red")#c("darkseagreen1", "navajowhite2", "lightpink2","honeydew", "cyan", "red") #c(topo.colors(4,alpha = 0.1), "red")
colornums = c(0)
for (t in 1:nrow(holder2)) {
  colornums = c(colornums, colorsnew[holder2$Var2[t] == categories])
}
holder2$cols = colornums[2:length(colornums)]
holder2 <- within(holder2, Var2 <- factor(Var2, levels = c("MA", "EG", "PS", "SS", "CS")))
levels(holder2$Var2) = c("Math", "Enginering", "Physical Science", "Social Science", "Computer Science")
colnames(holder2)[colnames(holder2) == "Var2"] = "Affiliation"
# colorsnew =  c(topo.colors(4, alpha = 0.1), "red")

# ggplot of Relative frequency of collaboration with EnvSci ####
g = ggplot(holder2, aes(x = Var1, y = value, fill = Affiliation), colour = cols)
g + geom_col(colour = "black", show.legend = TRUE) + 
  ylab('Relative frequency') + xlab('Year') + scale_fill_manual(values = rep(colorsnew, length(holder2$cols) / length(colorsnew))) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text=element_text(size=16),
          axis.title=element_text(size=18), legend.text = element_text(size = 14), legend.title = element_text(size = 16), legend.title.align = 0.5)#,face="bold")) #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                           #panel.background = element_blank(), axis.line = element_line(colour = "black"))
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

masterMatrix = matrix(0, nrow = length(categories), ncol = length(categories)) 
for (g in 1:nrow(papersShoverPaps)) {
  for (t in 3:ncol(papersShoverPaps)) {
    if (papersShoverPaps[g, t] != 0) {
      for (i in 2:ncol(papersShoverPaps)) {
        if (papersShoverPaps[g, i] != 0) {
          if (papersShoverPaps[g, t] > 1) {
            masterMatrix[t - 2, i - 2] = masterMatrix[t - 2, i - 2] + 1
            if (t != i) {
              masterMatrix[i - 2, t - 2] = masterMatrix[i - 2, t - 2] + 1
            }
          }
        }
      }
    }
  }
}
colnames(masterMatrix) = rownames(masterMatrix) = categories
mastt = melt(masterMatrix)
colnames(mastt) = c("from", "to", "edges")
mastt = data.frame(mastt)
edge_list <- get.data.frame(mastt, what = "edges") %>%
  inner_join(node_list %>% select(name, comm), by = c("from" = "name")) %>%
  inner_join(node_list %>% select(name, comm), by = c("to" = "name")) %>%
  mutate(group = ifelse(comm.x == comm.y, comm.x, NA) %>% factor())

m = as.matrix(masterMatrix)
net=graph.adjacency(m * 0.4,mode="undirected",weighted=TRUE,diag=FALSE) #the only difference between this and the weighted network code is that mode="directed"

coords = matrix(0, ncol = 2, nrow = length(categories))
coords[1,] = c(dev.size()[2] / 3, 3 * dev.size()[1] / 4) 
coords[6,] = c(2 * dev.size()[2] / 3, 3 * dev.size()[1] / 4)
coords[2,] = c(3 * dev.size()[2] / 5, dev.size()[1] / 5)
coords[3,] = c(2 * dev.size()[2] / 5, dev.size()[1] / 5)
coords[4,] = c(4 * dev.size()[2] / 5, dev.size()[1] * 0.38)
coords[5,] = c(1 * dev.size()[2] / 5, dev.size()[1] * 0.38)

createCurves = c(0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0)
createCurves = c(0, 0, 0, 2.0, 2.0, 0, -2.0, -2.8, 0, 1.2, 0, 0) #, -2.5, -2.8, -1)
edgeCols = c("red", "red", "red", "black", "black", "black", "black", "black", "black", "black", "black", "black") #plum3
add.vertex.shape("new", clip=igraph.shape.noclip, parameters=list(vertex.frame.color="black",
                                                                  vertex.frame.width=2))
oldcols = c("red", gray.colors(4, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL))
plot.igraph(net,vertex.label=V(net)$name,layout= coords,#layout.circle(net),#layout.fruchterman.reingold, 
            vertex.label.color=c("black", "black", "black", "black", "black", "white"),edge.curved = FALSE, vertex.color = c("red", topo.colors(4,alpha = 0.1), 
                                                                                    "black"), vertex.size = 53, edge.color=edgeCols
            , vertex.cex = 2, vertex.label.font = 2, vertex.label.cex = 2, edge.width=E(net)$weight/3, edge.arrow.size=1.5)

yearSaverByYear = matrix(0, nrow = nrow(yearSaver), ncol = 2)
for (t in 1:ncol(yearSaver)) {
  
}
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
means = countemup[1,] / countemup[2,]
meanofmeans = mean(means)
sdofmeans = sd(means)


meansrecent = countemup[1,(ncol(countemup) - 16):ncol(countemup)] / countemup[2,(ncol(countemup) - 16):ncol(countemup)]
recentmean = mean(meansrecent)

meansrecent = countemup[1,(ncol(countemup) - 6):ncol(countemup)] / countemup[2,(ncol(countemup) - 6):ncol(countemup)]
recentmean = mean(meansrecent)

meansold = countemup[1,1:8] / countemup[2,1:8]
oldmean = mean(meansold)
