#my theme for ggplots in MIIN

require(ggplot2)
require(grid)

#Factor levels
traitCat_order<-c('percN','cn','litterpercN','littercn')
measCat_order<-c('nh', 'no', 'toti',
                 'ammonif', 'nitrif', 'nminz',
                 'soilmoi','som','soiln',
                 'soilcn','ph','biom',
                 'litterbiom','percN','cn',
                 'litterpercN','littercn')

#Factor labels
prettylabels.tr<-c('Leaf %N','Leaf C:N','Litter %N','Litter C:N')
prettylabels.meas<-c('Ammonium', 'Nitrate', 'Total inorg. N',
                     'Ammonification', 'Nitrification', 'Mineralization',
                     'Soil moisture','SOM','Soil N',
                     'Soil C:N','Soil pH','Plant biomass',
                     'Litter biomass','CWM Leaf %N','CWM Leaf C:N',
                     'CWM Litter %N','CWM Litter C:N')

#my ggplot template
mytheme <- theme_bw(base_size = 10, base_family = "Helvetica") +
  theme(panel.border = element_rect(colour = "black"),      #put a black box around the plotting area
        axis.line = element_line(colour = "black"),                 #axis lines are in black
        panel.grid.major = element_blank(),                         #turn off the gridlines
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(face='bold.italic', hjust=0.05),         #turn off the x axis facet labels
        strip.text.y = element_text(face='bold.italic', hjust=0.05),
        strip.background = element_rect(fill = 'white', colour='black'),    #make y axis facet labels be italic and top justified
        legend.key = element_blank(),                               #turn off box around legend
        plot.title=element_text(hjust=0, vjust=0.5, face='bold'), #style and position of the panel label
        plot.margin = unit(c(0.05,0.05,0.05,0.05),"in")
        )

