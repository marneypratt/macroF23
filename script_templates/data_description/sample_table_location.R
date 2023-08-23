
table.sum <- ___ %>%  #put the name of your wrangled data here
  
  #summarize by date & location (to get number of microhabs and area sampled per date)
  group_by(date, location) %>% 
  summarize(
    area = sum(benthicArea),
    samples = n()
  ) %>% 

group_by(location) %>% 

  #summarize by location
  summarize(
    dates = str_c(date, collapse = ", "),
    samples = sum(samples),
    area = sum(area),
  )  


table.sum