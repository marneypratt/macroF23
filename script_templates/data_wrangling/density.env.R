
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros, master.taxa, and env must all be imported before you can run the code below

macro.density <- macros %>% 
  
  #join taxa info
  left_join(., master.taxa) %>%
  
  
  #join the env data
  left_join(., env) %>% 
  
  # Sum for each sampleID and the different taxa 
  # density of ALL macroinvertebrates
  group_by(date, sampleID, season, year, location, benthicArea) %>% 
  dplyr::summarise (density = sum(invDens, na.rm = TRUE),
                    
                    mon.ADD = round(mean(mon.ADD, na.rm=TRUE), digits=0),
                    mon.precip = round(mean(mon.precip, na.rm=TRUE), digits=0),
                    
                    # replace the blanks below with your water quality variable of interest
                    ___ = mean(___, na.rm = TRUE)) %>% 
  
  #remove NA's for your water quality variable of interest
  dplyr::filter(!is.na(___))


