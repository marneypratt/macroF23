
table.sum <- ___ %>%  #put the name of your wrangled data here
  
  #summarize by date, season, & location (to get number of microhabs and area sampled per date)
  group_by(date, season, location) %>% 
  summarize(
    area = sum(benthicArea),
    samples = n()
  ) %>% 
  
  #summarize by season & location 
  group_by(season, location) %>% 
  summarize(
    dates = str_c(date, collapse = ", "),
    samples = sum(samples),
    area = sum(area),
  )  


table.sum