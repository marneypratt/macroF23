
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data must both be imported before you can run the code below

macro.tol <- macros %>% 
  
  #join taxonomic information 
  left_join(., master.taxa) %>%

  # remove missing values
  dplyr::filter(!is.na(number), !is.na(tolerance)) %>% 
  
  # multiply the tolerance value times the number present for each kind of organism in each sampleID
  # change group_by function to remove or add grouping variables as needed 
  # but you MUST keep organism_aggr and sampleID in the group_by statement
  group_by(sampleID, organism_aggr, year, season, location, benthicArea) %>% 
  dplyr::summarise(num = sum(number), 
                   tol = mean(tolerance)) %>% 
  mutate(x.t = num*tol)

# calculate the HBI for each sampleID
# change group_by function to remove or add grouping variables as needed 
#but you must keep sampleID in the group_by statement
macro.HBI <- macro.tol %>% 
  group_by(year, season, location, sampleID) %>% 
  dplyr::summarise(x.t.sum = sum(x.t),
                   total.n = sum(num)) %>% 
  mutate(HBI = x.t.sum/total.n)



# select other variables you want present in your final dataset
variables <- macros %>% 
  
  #add or remove any variables from the original dataset that you want present
  #make sure you keep sampleID because this is what is used to match the data
  select(sampleID, date, location, year, season, benthicArea) %>% 
  distinct() %>% 
  
  #join the env data
  left_join(., env) %>% 
  
  #add weather data and environmental variables of interest
  # change group_by function to remove or add grouping variables as needed 
  group_by(sampleID, date, location, year, season, benthicArea) %>% 
  dplyr::summarise (
    mon.ADD = round(mean(mon.ADD, na.rm=TRUE), digits=0),
    mon.precip = round(mean(mon.precip, na.rm=TRUE), digits=0),
    
    # replace the blanks below with your water quality variable of interest
    ___ = mean(___, na.rm = TRUE)) 

#this script takes the macro.div and joins it with the mydf one we just created
#sampleID is the "key" used to match up the two data frames
macro.HBI <- left_join(macro.HBI, variables) 