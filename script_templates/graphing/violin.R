
# this code will make a basic violin plot


ggplot(
  data = ___,                           #put the data frame name here
  aes(x = ___, y = ___, fill = ___)) +  #same factor is x and fill, y = continuous variable
  geom_violin() +
  ylab("___") +
  xlab("___") +
  theme_classic(base_size=18) 

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-violin-plot/
# https://ggplot2.tidyverse.org/index.html