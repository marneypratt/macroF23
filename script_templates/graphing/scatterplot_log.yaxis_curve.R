

# this code will make a scatter plot with each graphed on a log-scale axis

ggplot(
  data = ___, 
  aes(x = ___, y = ___, color = ___)) + 
  geom_point(size = 3) +              #play with the point attributes as needed
  geom_smooth(method = "loess",       #adds locally weighted smoothing line
              se = FALSE, 
              formula = y ~ x) + 
  ylab("___") +
  xlab("___") +
  labs(color = "___") +
  theme_classic(base_size = 18) +
  scale_y_log10() +     #changes y-axis to log-scale
  annotation_logticks(sides = "l") # adds log-scale tick marks to the left axis

#see the resources below for some additional options to make a nice scatter plot 
# https://www.datanovia.com/en/lessons/ggplot-scatter-plot/
# https://ggplot2.tidyverse.org/index.html

#see the link below for more on smoothing lines using geom_smooth
# https://www.sharpsightlabs.com/blog/geom_smooth/ 