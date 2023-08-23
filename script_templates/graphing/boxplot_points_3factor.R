 
# this code will make a boxplot with mean and all points included
# make sure you have loaded the ggbeeswarm package to use geom_quasirandom()
# alternatively, you can use geom_jitter() instead


ggplot(
  data = ___, #put the data frame name here
  aes(x = ___, y = ___, fill=___)) + #1st factor = x, continuous variable = y, 2nd factor = fill
  geom_boxplot(position=position_dodge(width=1), #adjust as needed
               width=0.3, alpha=0.25,      #adjust these values as needed
               outlier.shape= NA,         #remove outliers when all the points are included
               show.legend = FALSE) +       
  geom_quasirandom(aes(color=___),                    #2nd factor to match fill above
                   dodge.width=1, width=0.25,         #dodge.width here must match position_dodge(width= )
                   shape=21, size=1, alpha = 0.5) +   #sets the size, shape, and transparency of the points
  stat_summary(fun=mean, geom="point",                #adds mean with an X
               position=position_dodge(width=1),      #must match position_dodge(width= ) for boxplot
               shape=4, size=1, colour="black", stroke=1,  #adjust as needed
               show.legend = FALSE) + 
  ylab("___") +
  xlab("___") +
  coord_cartesian(expand=TRUE) +
  theme_classic(base_size=16)  +
  theme(legend.position = "top") +
  facet_wrap(vars(___))   #3rd factor here


#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-boxplot/
# https://ggplot2.tidyverse.org/index.html