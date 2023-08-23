
# this code will make a violin plot with boxplots inside


ggplot(
  data = ___,                               #put the data frame name here
  aes(x = ___, y = ___, fill = ___)) +      #factor = x and fill, y = continuous variable
  geom_violin(width=0.5, alpha=0.2) +                  #adjust values as needed
  geom_boxplot(width=0.1, color="black", alpha=0.8) +  #adjust values as needed
  ylab("___") +
  xlab("___") +
  theme_classic(base_size=18) +
  theme(legend.position="none") # remove this line if you DO want a legend/key

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-violin-plot/
# https://ggplot2.tidyverse.org/index.html