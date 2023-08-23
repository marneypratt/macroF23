# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data must both be imported before you can run the code below

# select other variables you want present in your final dataset
variables <- macros %>% 
  
  #add or remove any variables from the original dataset that you want present
  #make sure you keep sampleID because this is what is used to match the data
  select(sampleID, date, location, year, season, benthicArea) %>% 
  distinct()


#this code will calculate the substrate stability ratio (SSR)
# SSR = (#scrapers + #collector filterers)/(#collector gatherers + shredders)

macro.ssr <- macros %>% 
  
  #join taxonomic information 
  left_join(., master.taxa) %>% 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID, FFG) %>% 
  summarize(number = sum(number, na.rm=TRUE)) %>% 
  
  #pivot wider to make column for each FFG
  pivot_wider(names_from = FFG, values_from = number, values_fill=0) %>% 
  
  #calc substrate stability ratio
  mutate(ssr = ((sum(scr,cf, na.rm=TRUE)+1)/(sum(cg,sh, na.rm=TRUE)+1))) %>% 
  
  #add other variables back
  left_join(., variables)