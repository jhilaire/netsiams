load("data/all_bottomup_data.RData")

data_bu_storage <- all_data %>% filter(technology_detail == "Storage")
rm("all_data")



library(rworldmap)


data_bu_storage %>% 
  select(boundaries) %>% 
  unique()


regions <- c("global", "former soviet union", "usa", "north america", "south america", "africa", "india", "europe", "middle east", "other developing asia", "australia", "asia")