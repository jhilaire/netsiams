load("data/all_bottomup_data.RData")
data_storage <- all_data %>% 
  filter(technology == "Storage", !is.na(value))  
  
def_boundaries <- data.frame(
  boundaries = c("north  america (can, us)", "global", "europe", "africa", "india", "middle east", 
                 "western europe", "china", "australia", "canada", "central and south america", 
                 "eastern europe", "former soviet union", "japan", "mexico", "other developing asia", 
                 "south korea", "usa", "north america (can, us)", "baltic", "poland", "finland", 
                 "sweden", "denmark", "norway", "part russia", "nordic region", "oecd europe", 
                 "north america", "iran", "asia pacific", "eu & eurasia", "middle east & africa", 
                 "s. & c. america", "asia and oceania", "eastern europe (inc former ussr)", 
                 "south/central america", "global, review", 
                 "europe (uk southern north seas, partial denmark, germany, norway offshore, netherlands, greese, partial belgium)", 
                 "baltic", "australia and oceania", "east asia", "middle east and west asia", "south america", 
                 "southeast asia", "ussr", "northern europe", "algeria"),
  boundaries2 = c("NAM", "World", "EUR", "AFR", "IND", "MEA", "WEU", "CHN", "AUS", "CAN", "CSA", "EEU",
                  "FSU", "JPN", "MEX", "ODA", "SKOR", "USA", "NAM", "Baltic", "POL", "FIN", "SWE", "DEN",
                  "NOR", "RUS", "Nordic region", "EUR", "NAM", "IRN", "ASIAPAC", "EURASIA", "MAF", "CSA",
                  "ASIAOCE", "EEU", "CSA", "World", "EUR", "Baltic", "AUSOCE", "EAS", "MEAWAS", "SAM",
                  "SEA", "USSR", "NEU", "Algeria")
)

data_storage <- data_storage %>% 
  left_join(def_boundaries,
            by=c("boundaries"))

data_storage %>% filter(`Data categorisationresource` == "Total", measurement == "max") %>% group_by(boundaries2) %>% summarise(min=min(value), median=median(value), max=max(value)) %>% ungroup()


dooley_storage <- read.csv("data/Dooley2013-fig2Digitalised2.csv", 
                           header = FALSE, col.names = c("region", "type", "value"), 
                           stringsAsFactors = FALSE)

dooley_storage <- dooley_storage %>% 
  spread(region, value) %>% 
  gather(region, value, -type) %>% 
  mutate(value=ifelse(is.na(value), 0, value)) %>% 
  spread(region, value) %>% 
  mutate(USA=USA+Canada) %>% 
  select(-Canada) %>% 
  mutate(Europe = `Western Europe`+`Eastern Europe`) %>% 
  select(-`Western Europe`, -`Eastern Europe`) %>% 
  rename(India=`Indian Subcontinent`) %>% 
  rename(Other=`South Korea`) %>% 
  gather(region, value, -type) %>% 
  mutate(value=ifelse(is.na(value), 0, value))

write.csv(dooley_storage, file="data/dooley_storage.csv", row.names = FALSE)

# Compiled data
def_regions <- data.frame(
  region  = c("ASIA",           "Brazil",  "China", "EU",     "EU12",   "EU15",   "India", "Japan", "LAM",           "MAF",         "OECD90", "REF",                 "Russia",              "USA", "NORTH_AM", "EUROPE", "PAC_OECD", "REF_ECON",            "CHINA+", "INDIA+", "REST_ASIA",      "AFRICA", "MIDDLE_EAST", "LATIN_AM",      "REST_WORLD", "AFR",    "CHN",   "EUR",    "FSU",                 "IND",   "JPN",   "MEA",         "OAS",            "ROW",   "RUS"),
  region2 = c("Southeast Asia", "Other",   "China", "Europe", "Europe", "Europe", "India", "Japan", "Latin America", "Middle East", "Other",  "Former Soviet Union", "Former Soviet Union", "USA", "USA",      "Europe", "Other",    "Former Soviet Union", "China",  "India",  "Southeast Asia", "Africa", "Middle East", "Latin America", "Other",      "Africa", "China", "Europe", "Former Soviet Union", "India", "Japan", "Middle East", "Southeast Asia", "Other", "Former Soviet Union"),
  stringsAsFactors = FALSE
)

scenario_storage <- v_data_timeTechTemp_regional %>% 
  filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass") %>% 
  inner_join(def_regions, by=c("region")) %>% 
  filter(period %in% seq(2010,2100,10)) %>% 
  filter(!tempcat %in% c("Other scenario", "Likely 3.0°C scenario")) %>%
  mutate(tempcat = ifelse(tempcat %in% c("1.5°C scenario"), "1.5°C", "2.0°C")) %>% 
  select(-variable) %>% 
  group_by(model,scenario,region,region2,tempcat) %>% 
  mutate(dt=0.5*(lead(period, default=2100) - period) + 0.5*(period - lag(period, default=2010))) %>% 
  summarise(value=sum(value/1000*dt)) %>% 
  ungroup() %>% 
  group_by(region2,tempcat) %>% 
  summarise(
    min=min(value, na.rm=T),
    p05=quantile(value, 0.05, na.rm=T),
    p15=quantile(value, 0.15, na.rm=T),
    p25=quantile(value, 0.25, na.rm=T),
    med=median(value, na.rm=T),
    mean=mean(value, na.rm=T),
    p75=quantile(value, 0.75, na.rm=T),
    p85=quantile(value, 0.85, na.rm=T),
    p95=quantile(value, 0.95, na.rm=T),
    max=max(value, na.rm=T),
    count=n()
    ) %>% 
  ungroup() 

write.csv(scenario_storage, file="data/scenario_storage.csv", row.names = FALSE)

ggplot(scenario_storage) + 
  geom_bar(aes(x=tempcat, y=med, fill=tempcat), stat="identity")

ggplot(scenario_storage) + 
  geom_boxplot(aes(x=tempcat, ymin=min, ymax=max, lower=p25, upper=p75, middle=med, fill=tempcat), stat="identity") +
  facet_wrap(~region2, scales="free_y") +
  theme_bw() +
  theme(legend.position="bottom") +
  #guides(fill=FALSE) +
  xlab("")


# library(ggsubplot)
# library(ggplot2)
# library(maps)
# library(plyr)
# 
# #Get world map info
# world_map <- map_data("world")
# 
# #Create a base plot
# p <- ggplot() + 
#   geom_polygon(data=world_map, aes(x=long, y=lat, group=group), col = "blue4", fill = "lightgray") + 
#   theme_bw()
# 
# # Calculate the mean longitude and latitude per region (places where subplots are plotted),
# cntr <- ddply(world_map, .(region), summarize, long=mean(long),lat=mean(lat))
# 
# # example data
# myd <- data.frame (region = rep (c("USA","China","USSR","Brazil", "Australia","India", "Canada"),5),
#                    categ = rep (c("A", "B", "C", "D", "E"),7), frequency = round (rnorm (35, 8000, 4000), 0))
# 
# 
# subsetcntr <- subset(cntr, region %in% c("USA","China","USSR","Brazil", "Australia","India", "Canada"))
# 
# simdat <- merge(subsetcntr, myd)
# colnames(simdat) <- c( "region","long","lat", "categ", "myvar" )
# 
# 
# myplot <- p + 
#   geom_subplot2d(aes(long, lat, subplot = geom_bar(aes(x = categ, y = myvar, fill = categ, width=1), position = "identity")), ref = NULL, data = simdat)
# 
# print(myplot)