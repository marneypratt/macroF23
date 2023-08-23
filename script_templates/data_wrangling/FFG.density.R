
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data must both be imported before you can run the code below
  
  macro.ffg <- macros %>% 
    
    #join taxonomic information 
    left_join(., master.taxa) %>% 
  
  
  # Sum each FFG density for each sampleID 
  group_by(sampleID, FFG) %>% 
  dplyr::summarise (ffgDens = sum(invDens, na.rm = TRUE)) %>% 


  #adds back the zeros for FFGs that were not present in a sample
  #repeat all the grouping variables as above
  ungroup %>%
    complete(sampleID, FFG, fill = list(ffgDens = 0)) 
  
  
  #filter for the organisms without FFGs assigned
  macro.ffg.na <- macro.ffg %>% 
    filter(is.na(FFG) & ffgDens > 0)
  
  # select other variables you want present in your final dataset
  variables <- macros %>% 
    
    #add or remove any variables from the original dataset that you want present
    #make sure you keep sampleID because this is what is used to match the data
    select(sampleID, date, location, year, season, benthicArea) %>% 
    distinct()
  
  
  #add back the organisms without FFGs assigned
  macro.ffg <- macro.ffg %>% 
    filter(!is.na(FFG)) %>% 
    bind_rows(macro.ffg.na) %>% 
    
    #add variables of interest to final dataset
    left_join(., variables)