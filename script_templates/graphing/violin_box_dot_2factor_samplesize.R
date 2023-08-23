
#Calculates the sample size of each group
n_fun <- function(x){
  return(data.frame(y = ___,                      #this must match the value used for y in geom_text below
                    label = length(x)))
}

#graph mix plot with dots, boxes, and violins
ggplot(
  data=___,  #put the data frame here 
  aes(x=___, y=___, color=___, fill=___)) + #1st factor is x, continuous var is y, 2nd factor is color and fill
  geom_violin(
    width=1, alpha=0.2, 
    position=position_dodge(width=0.75), #match with geom_boxplot
    show.legend = TRUE) +
  geom_boxplot(
    width=0.1, color="black", alpha=1, 
    position=position_dodge(width=0.75), #match with geom_violin
    show.legend = FALSE,
    outlier.shape = NA) +
  geom_quasirandom(shape=16, 
                   alpha=0.5, size=1,                #adjust as needed
                   dodge.width = 0.75,                 #match position_dodge 
                   show.legend = FALSE) +
  stat_summary(fun.data = n_fun, 
               geom = "text", 
               position = position_dodge(width=0.75), 
               show.legend = FALSE) +
  geom_text(x=0.59, y=___, label="N =", color = "black") +  #this must match the value used for y in n_fun above
  ylab("___") + #put formatted label for the axis here (include units as needed)
  xlab("___") + #put formatted label for the axis here (include units as needed)
  coord_cartesian(ylim = c(0, ___), expand=FALSE) +  #the upper y-limit must fit all the data and the N values
  theme_classic(base_size=20)  
