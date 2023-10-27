# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data must both be imported before you can run the code below

# select other variables you want present in your final dataset
variables <- macros |> 
  
  #join environmental variables
  left_join(env) |> 
  
  #add or remove any variables from the original dataset that you want present
  #make sure you keep sampleID because this is what is used to match the data
  #replace the blank with your environmental variables (or remove that part)
  select(sampleID, date, location, year, season, benthicArea, 
         ___) |> 
  distinct()


#this code will calculate the substrate stability ratio (SSR)
# SSR = (#scrapers + #collector filterers)/(#collector gatherers + shredders)

macro.t <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID) |> 
  summarize(total = sum(number, na.rm=TRUE))

macro.ssr <- macros |> 
  
  #join taxonomic information 
  left_join(master.taxa) |> 
  
  #calculate the number of each FFG in each sampleID
  group_by(sampleID, FFG) |> 
  summarize(number = sum(number, na.rm=TRUE)) |> 
  
  #pivot wider to make column for each FFG
  pivot_wider(names_from = FFG, values_from = number, values_fill=0) %>% 
  
  #calc number of macros that like rocks
  #calc number of macros that like sand
  #calc substrate stability ratio
  mutate(rock = sum(scr,cf, na.rm=TRUE),
         sand = sum(cg,sh, na.rm=TRUE),
    ssr = ((sum(scr,cf, na.rm=TRUE)+1)/(sum(cg,sh, na.rm=TRUE)+1))) |> 
  
  #add total of all macros
  left_join(macro.t ) |> 
  
  #calc the relative abundance of macros that like rocks and sand
  mutate(relab.rock = rock/total,
         relab.sand = sand/total) |> 
  
  #add other variables back
  left_join(variables, by="sampleID") |> 
  
  #calc the density of macros that like rocks and sand
  mutate(den.rock = rock/benthicArea,
         den.sand = sand/benthicArea)
  
