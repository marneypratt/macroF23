
# this code will make a boxplot with mean and all points included plus the sample size on the x-axis
# see the "Intro to R for Biology: Graphing Grouped Continuous Data" tutorial for more help
# make sure you have loaded the ggbeeswarm package to use geom_quasirandom()
# alternatively, you can use geom_jitter() instead

# first calculate the sample size
sample_size = ___ %>% #put the data frame name here
  group_by(___) %>%  #factor here
  summarize (num=n())


#violin plot with box plots and sample sizes
___ %>%   #put the same data frame name here as above
  left_join(sample_size, by="___") %>% #put the same factor as above here
  mutate(myaxis = paste0(___, "\n", "n=", num)) %>%  #put the same factor as above here
  ggplot(aes(x=myaxis, y=___, fill=___)) + #continuous variable = y, same factor as above = fill,
  geom_boxplot(width=0.2, alpha=0.5,      #adjust these values as needed
               outlier.shape= NA) +       #remove the outliers when all the points are included
  geom_quasirandom(aes(color=___),  #same factor as above here
                   shape=21, size=1, alpha = 0.5, width=0.25) + #sets the size, shape, and transparency of the points
  stat_summary(fun=mean, geom="point", shape=4, size=2, colour="black", stroke=2) +  #adds mean with an X
  ylab("___") +
  xlab("___") +
  coord_cartesian(expand=TRUE) +
  theme_classic(base_size=16)  +
  theme(legend.position="none") # remove this line if you DO want a legend/key

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-boxplot/
# https://ggplot2.tidyverse.org/index.html