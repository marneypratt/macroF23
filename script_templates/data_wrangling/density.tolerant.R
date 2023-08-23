# make sure the 'tidyverse' package is installed and loaded 
# macros, master.taxa, and env data files must be imported before you 
# can run the code below

#get sample info
sample.info <- macros %>%
  select(date, sampleID, season, year, location, benthicArea) %>% 
  distinct()

#get total samples
samples <- macros %>%
  select(sampleID) %>% 
  distinct()


#select env variable(s) you want by replacing the blank with the column name(s)
env.vars <- env %>% 
  select(sampleID, ___)


#calculate density for each sampleID
tolerant.density <- macros %>% 
  
  #join taxa info
  left_join(., master.taxa) %>%
  
  #filter for tolerant macros (8-10)
  dplyr::filter(tolerance >= 8) %>% 
  
  # Sum for each sampleID  
  # density of filtered macroinvertebrates
  # change group_by function to remove or add grouping variables as needed 
  group_by(sampleID) %>% 
  dplyr::summarise (density = sum(invDens, na.rm = TRUE)) %>% 
  
  #add back in samples that had none present
  right_join(., samples) %>% 
  
  #fill in group combos where there were none present with zeros
  ungroup() %>% 
  complete(sampleID,  
           fill = list(density = 0)) %>% 
  
  #add back the sampling info
  left_join(., sample.info) %>% 
  
  #add env variables
  left_join(., env.vars)


