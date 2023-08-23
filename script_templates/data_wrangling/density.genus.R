
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data files must be imported before you can run the code below

#calculate density for each sampleID
density.df <- macros %>% 
  
  #join taxa info
  left_join(., master.taxa) %>%
  
  # Summarize for each sampleID and each genus 
  group_by(sampleID, genus) %>% 
  dplyr::summarise (density = sum(invDens, na.rm = TRUE)) %>% 
  
  #fill in group combos where there were none present with zeros
  ungroup() %>% 
  complete(sampleID, genus,
           fill = list(density = 0)) %>% 
  
  #filter for genus of interest (this needs to match what is in the macros file)
  filter(genus == "___") #replace this blank with the genus you want to keep

#get sample info
sample.info <- macros %>%
  select(date, sampleID, season, year, location, benthicArea) %>% 
  distinct()

#add sample info back to density data
density.df <- left_join(density.df, sample.info)


#select desired variables from env then join to density data
#remove the 3 lines of code below if you do not need any environmental variables
env2 <- env %>% 
  select(sampleID, ___) #add columns from env file that you want to keep

density.df <- left_join(density.df, env2) %>% 
  
  #filter as needed (below is an example that removes data from 2018)
  filter(year != "2018")


