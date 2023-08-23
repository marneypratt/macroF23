

# this code will make a scatter plot with each graphed on a log-scale axis

ggplot(
  data = ___, 
  aes(x = ___, y = ___, color = ___)) + 
  geom_point(size = 3) +                   #play with the point attributes as needed
  geom_smooth(method = "lm", se = FALSE) + #adds line of best fit
  ylab("___") +
  xlab("___") +
  labs(color = "___") +
  theme_classic(base_size = 18) +
  scale_y_log10() +     #changes y-axis to log-scale
  scale_x_log10() +     #changes x-axis to log-scale
  annotation_logticks() # adds log-scale tick marks

#see the resources below for some additional options to make a nice scatter plot 
# https://www.datanovia.com/en/lessons/ggplot-scatter-plot/
# https://ggplot2.tidyverse.org/index.html