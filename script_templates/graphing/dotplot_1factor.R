
# this code will make a dot plot with median and all points included
# make sure you have loaded the ggbeeswarm package to use geom_quasirandom()
# alternatively, you can use geom_jitter() instead


ggplot(
  data = ___, #put the data frame name here
  aes(x = ___, y = ___, fill=___)) + #factor = x and fill, continuous variable = y
  geom_quasirandom(
    shape=21, size=3, alpha = 0.75, width=0.3) +
  stat_summary(fun = median, fun.min = median, fun.max = median, #change median to mean if desired
               geom = "crossbar", na.rm = TRUE,
               width = 0.5, size = 0.75, color = "black") +
  ylab("___") +
  xlab("___") +
  coord_cartesian(expand=TRUE) +
  theme_classic(base_size=16)  +
  theme(legend.position="none") # remove this line if you DO want a legend/key

#see the resources below for some additional options to make a nice plot 
# https://www.datanovia.com/en/lessons/ggplot-dot-plot/
# https://ggplot2.tidyverse.org/index.html