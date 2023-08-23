
#this code will make stacked bar graphs for relative abundance of functional feeding groups

ggplot(
  data=___ # put the data frame here
  aes(x=____,y=relab, fill = FFG))+ #put 1st factor as x 
  geom_bar(position = "fill", stat="identity")+
  ylab("Relative Abundance\n") +
  scale_fill_discrete(name="Functional Feeding Groups",
                      labels=c("collector filterers", "collector gatherers",
                               "predators", "scrapers", "shredders" ))+
  coord_cartesian(ylim=c(0,1), expand=FALSE, clip='off') +
  theme_classic(base_size=10) +
  
  #you can use the line below to split by season, year, location, or other factors. Just replace the blanks with the names of the columns you want to split by. Or you can remove this line of code (and the + at the end of the line above)
  facet_grid(rows=vars(____), cols=vars(____)) 



#see the resources below for some additional options to make a nice plot 
# https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
# https://ggplot2.tidyverse.org/index.html
# https://r-charts.com/part-whole/stacked-bar-chart-ggplot2/