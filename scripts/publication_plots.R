# Figure 1 ####
# use in conjunction with Analysis_Plotting.R
mytheme <- theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 axis.text=element_text(size=10, colour = "black"), 
                 axis.title=element_text(size=12, colour = "black"), 
                 legend.text = element_text(size = 10), legend.title = element_text(size = 12), 
                 legend.title.align = 0.5)

a <- ggplot(subset(prop_collab, Metric != "Non-Computer Science Collaborative"), aes(x = Year, y = Value, col = Metric)) + mytheme + 
  geom_line(lwd = 1.05) + 
  geom_point(size = 2) +
  #geom_smooth() +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0,400, 50)) +
  scale_x_continuous(limits = c(1971, 2016), breaks=seq(1975,2015,5)) + 
  scale_colour_manual(values = c('black', 'gray50', 'red')) +
  labs(y = expression(paste("Papers analyzed from ", italic("Ecology")))) +
  theme(legend.position = c(0,1), legend.justification = c(0,1), legend.title = element_blank()) 

b <- ggplot(ES_collab_frequency, aes(x = Year, y = RelativeFreq, fill = Collaboration)) + mytheme + 
  geom_col() + 
  labs(x = "Year", y = "Proportion of interdisciplinary papers") + 
  scale_x_continuous(expand = c(0,0), limits = c(1971, 2016), breaks=seq(1970,2015,5)) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual("Discipline", values = colors) 

x <- plot_grid(a, b, nrow = 1, labels = c('A', 'B'), rel_widths = c(1, 1.4))

tiff(filename = "./Figure1.tif", width = 10, height = 5, units = "in", compression = c("none"),res = 500)
x
dev.off()
