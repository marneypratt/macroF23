
# this code will make a violin plot with box plots and jittered points and sample size on the x-axis
# see the "Intro to R for Biology: Graphing Grouped Continuous Data" tutorial for more help


# first calculate the sample size
sample_size = ___ %>% #put the data frame name here
  group_by(___) %>%  #put the grouping variable name here
  summarize (num=n())

#violin plot with box plots, dots, and sample sizes
___ %>%   #put the data frame name here
  left_join(sample_size, by="___") %>% #put the grouping variable name here
  mutate(myaxis = paste0(___, "\n", "n=", num)) %>%  #put the grouping variable name here
  ggplot(aes(x=myaxis, y=___, fill=___)) + #put the y-variable first and then the grouping variable for fill
  geom_violin(width=0.5, alpha=0.2, scale="count") +
  geom_boxplot(width=0.1, color="black", alpha=1) +
  geom_quasirandom(shape=21, size=1, alpha = 0.2, width=0.15) +
  ylab("___") +
  xlab("___") +
  theme_classic(base_size=16) +
  theme(legend.position="none")

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-violin-plot/
# https://ggplot2.tidyverse.org/index.html