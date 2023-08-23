
# this code will make a histogram


ggplot(
  data = ___, 
  aes(x = ___)) + 
  geom_histogram(binwidth = ___, color = "white", fill= "#00BFC4") +
  xlab("___") +
  ylab("Count") +
  theme_classic(base_size=18) 

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-histogram/
# https://ggplot2.tidyverse.org/index.html