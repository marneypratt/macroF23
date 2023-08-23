
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros, master.taxa, and env data must all be imported before you can run the code below
  
  macro.ffg <- macros %>% 
    
    #join taxonomic information 
    left_join(., master.taxa) %>% 
    

  # Sum each FFG density for each sampleID 
  group_by(sampleID, FFG) %>% 
  dplyr::summarise (ffgDens = sum(invDens, na.rm = TRUE)) %>% 


  #adds back the zeros for FFGs that were not present in a sample
  #repeat all the grouping variables as above
  ungroup %>%
    complete(sampleID, FFG, 
             fill = list(ffgDens = 0)) 
  
  
  #filter for the organisms without FFGs assigned
  macro.ffg.na <- macro.ffg %>% 
    filter(is.na(FFG) & ffgDens > 0)
  
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
  
  #add back the organisms without FFGs assigned
  macro.ffg <- macro.ffg %>% 
    filter(!is.na(FFG)) %>% 
    bind_rows(macro.ffg.na) %>% 
    
    #add variables of interest to final dataset
    left_join(., variables) 
  
  
  
  