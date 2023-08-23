
# make sure the `vegan` and 'tidyverse' packages are installed and loaded to run the code below

# macros and master.taxa data files must both be imported before you can run the code below

# remove missing values, and make sure each taxon is summarized within sampleID
macro.long <- macros %>%  
  
  #join taxa info
  left_join(., master.taxa) %>%

  dplyr::filter(!is.na(invDens), year !="2018") %>% 
  dplyr::select(sampleID, organism_aggr, invDens) %>% 
  group_by(sampleID, organism_aggr) %>% 
  dplyr::summarise(density = sum(invDens))

# convert to wide format
macro.wide <- macro.long %>% 
  pivot_wider(names_from = organism_aggr, 
              values_from = density,
              values_fill = list(density = 0),
              values_fn = list(density = sum)) %>%
  tibble::column_to_rownames("sampleID")

#Calculate diversity index values

#Shannon
H <- diversity(macro.wide)


#effective number of species
effective.sp <- exp(H)


#Richness
rich <- specnumber(macro.wide) 

#Max possible diversity
max.H <- log(rich)


#Pielou's Evenness J 
J <- H / max.H

#put all diversity values into a single data frame
macro.div <- data.frame(H, effective.sp, rich, max.H, J) %>% 
  tibble::rownames_to_column("sampleID")


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
                    
                    # replace the blanks below with your water quality variables of interest
                    ___ = mean(___, na.rm = TRUE)) 

#this script takes the macro.div and joins it with the mydf one we just created
#sampleID is the "key" used to match up the two data frames
macro.div <- left_join(macro.div, variables) 
  




