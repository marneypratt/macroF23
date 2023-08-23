

# this code will make a scatter plot with each variable log transformed by
# wrapping the x and y variables in the log() function

ggplot(
  data = ___, 
  aes(x = log(___), y = log(___), color = ___)) + 
  geom_point(size = 3) +                   #play with the point attributes as needed
  geom_smooth(method = "lm", se = FALSE) + #adds line of best fit
  ylab("log(___)") +
  xlab("log(___)") +
  labs(color = "___") +
  theme_classic(base_size = 18)

#see the resources below for some additional options to make a nice scatter plot 
# https://www.datanovia.com/en/lessons/ggplot-scatter-plot/
# https://ggplot2.tidyverse.org/index.html