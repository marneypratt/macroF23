
# make sure the 'tidyverse' package is installed and loaded to run the code below

# macros and master.taxa data files must be imported before you can run the code below

macro.density <- macros  |>  
  
  #join taxa info
  left_join(master.taxa)  |> 
  
  # Sum for each sampleID and the different taxa 
  # density of ALL macroinvertebrates
  # change group_by function to remove or add grouping variables as needed 
  group_by(date, sampleID, season, location, year, benthicArea)  |>  
  dplyr::summarise (density = sum(invDens, na.rm = TRUE)) 


