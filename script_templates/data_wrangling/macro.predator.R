# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros, master.taxa, and env data must all be imported before you can run the code below

#this code will calculate the predator ratio 
# predator ratio = #predators /(#scrapers + #collector filterers + #collector gatherers + shredders)

macro.prd <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID, FFG) |> 
  summarize(number = sum(number, na.rm = TRUE)) |> 
  
  #pivot wider to make column for each FFG
  pivot_wider(names_from = FFG, values_from = number, values_fill = 0) |> 
  
  #add non-predators together
  mutate(not.prd = sum(scr,cf, cg, sh, na.rm = TRUE)) |> 
  
  #calculate the predator ratio
  mutate(prd.ratio = (prd/not.prd)) 


# select other variables you want present in your final dataset
variables <- macros |> 
  
  #join environmental variables
  left_join(env) |> 
  
  #select variables of interest
  #delete anything you don't need
  #add anything you do need in the blank with commas in between
  dplyr::select(date, sampleID, season, year, location, benthicArea,
                ___) |> 
  distinct()

#add in the variables just selected
#sampleID is the "key" used to match up the two data frames
macro.ffg <- left_join(macro.ffg, variables)