
table.sum <- ___ %>%  #put the name of your wrangled data here
  
  #summarize by date, season, year, & location (to get number of microhabs and area sampled per date)
  group_by(date, season, year, location) %>% 
  summarize(
    area = sum(benthicArea),
    samples = n()
  ) %>% 
  
  
  #change date to month/day
  mutate(date = format(as.Date(date), '%m/%d')) %>% 
  
  #summarize by year, season, location 
  group_by(season, year, location) %>% 
  summarize(
    dates = str_c(date, collapse = ", "),
    samples = sum(samples),
    area = sum(area),
  )  


table.sum