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
                           #header = FALSE, col.names = c("region", "type", "value"), 
                           stringsAsFactors = FALSE)

dooley_storage <- dooley_storage %>% 
  spread(region, value) %>% 
  gather(region, value, -storage_type) %>% 
  mutate(value=as.numeric(value)) %>%
  mutate(value=ifelse(is.na(value), 0.00, value)) %>%
  spread(storage_type, value) %>% 
  mutate(`Deep Saline Filled Formations-offshore` = `Deep Saline Filled Formations-offshore`-`Deep Saline Filled Formations-onshore`) %>% 
  mutate(`Deep Saline Filled Formations-onshore`  = `Deep Saline Filled Formations-onshore`-`Depleted Oil and Gas Fields`) %>% 
  mutate(`Depleted Oil and Gas Fields`            = `Depleted Oil and Gas Fields`-`Deep Unmineable Coal Seams`) %>% 
  gather(storage_type,value,-region) %>% 
  spread(region, value) %>%
  rename(`Pacific OECD`=Australia) %>% 
  #mutate(NAM = USA + Canada) %>% 
  #select(-Canada, -USA) %>% 
  mutate(Europe = `Western Europe`+`Eastern Europe`) %>% 
  select(-`Western Europe`, -`Eastern Europe`) %>% 
  rename(India=`Indian Subcontinent`) %>% 
  #rename(`Other regions`=`South Korea`) %>% 
  gather(region, value, -storage_type) %>% 
  mutate(value=ifelse(is.na(value), 0, value))

write.csv(dooley_storage, file="data/dooley_storage.csv", row.names = FALSE)

edmonds_storage <- read.csv2("data/Edmonds2013-storage.csv", 
                           #header = FALSE, col.names = c("region", "type", "value"), 
                           stringsAsFactors = FALSE)

edmonds_storage <- edmonds_storage %>% 
  mutate(storage_type=trimws(storage_type)) %>% 
  spread(region, value) %>% 
  gather(region, value, -storage_type) %>% 
  mutate(value=as.numeric(value)) %>%
  mutate(value=ifelse(is.na(value), 0.00, value)) %>%
  spread(storage_type, value) %>% 
  mutate(`Deep Saline Formation-Offshore` = `Deep Saline Formation-Offshore`-`Deep Saline Formation-Onshore`) %>% 
  mutate(`Deep Saline Formation-Onshore`  = `Deep Saline Formation-Onshore`-`Depleted Oil and Gas Fields`) %>% 
  mutate(`Depleted Oil and Gas Fields`    = `Depleted Oil and Gas Fields`-`Deep Unmineable Coal Basins`) %>% 
  gather(storage_type,value,-region) %>% 
  spread(region, value) %>%
  #mutate(NAM = USA + Canada) %>% 
  #select(-Canada, -USA) %>% 
  mutate(Europe = `Western Europe`+`Eastern Europe`) %>% 
  mutate(`Southeast Asia` = `Southeast Asia`) %>% 
  rename(`Pacific OECD`=Australia_NZ) %>% 
  select(-`Western Europe`, -`Eastern Europe`) %>% 
  #rename(India=`Indian Subcontinent`) %>% 
  #rename(`Other regions`=`South Korea`) %>% 
  gather(region, value, -storage_type) %>% 
  mutate(value=ifelse(is.na(value), 0, value)) %>% 
  mutate(storage_type=factor(storage_type,
                             levels=c("Deep Saline Formation-Onshore",
                                      "Deep Saline Formation-Offshore",
                                      "Depleted Oil and Gas Fields",
                                      "Deep Unmineable Coal Basins"),
                             labels=c("Deep Saline Filled Formations-onshore",
                                      "Deep Saline Filled Formations-offshore",
                                      "Depleted Oil and Gas Fields",
                                      "Deep Unmineable Coal Seams")))

write.csv(edmonds_storage, file="data/edmonds_storage.csv", row.names = FALSE)

# Compiled data
def_regions <- data.frame(
  region  = c("ASIA",           "Brazil",  "China", "EU",     "EU12",   "EU15",   "India", "Japan", "LAM",           "MAF",         "OECD90", "REF",                 "Russia",              "USA", "NORTH_AM", "EUROPE", "PAC_OECD", "REF_ECON",            "CHINA+", "INDIA+", "REST_ASIA",      "AFRICA", "MIDDLE_EAST", "LATIN_AM",      "REST_WORLD", "AFR",    "CHN",   "EUR",    "FSU",                 "IND",   "JPN",   "MEA",         "OAS",            "ROW",   "RUS"),
  region2 = c("Southeast Asia", "Other",   "China", "Europe", "Europe", "Europe", "India", "Japan", "Latin America", "Middle East", "Other",  "Former Soviet Union", "Former Soviet Union", "USA", "USA",      "Europe", "Other",    "Former Soviet Union", "China",  "India",  "Southeast Asia", "Africa", "Middle East", "Latin America", "Other",      "Africa", "China", "Europe", "Former Soviet Union", "India", "Japan", "Middle East", "Southeast Asia", "Other", "Former Soviet Union"),
  stringsAsFactors = FALSE
)

def_regions_mapHolz2817 <- data.frame(
  region  = c("OECD US", "G77 Brazil",    "G77 Mexico",    "G77 Other Latin America", "OECD EU27", "OECD Russia",         "Other Eastern Europe", "OECD Japan", "G77 China", "G77 India", "G77 South Africa", "G77 Other Africa", "G77 Middle East", "OECD Australia", "OECD New Zealand", "G77 Indonesia", "G77 Other Large Asia", "OECD South Korea", "G77 Small Asia"),
  region2 = c("USA",     "Latin America", "Latin America", "Latin America",           "Europe",    "Former Soviet Union", "Former Soviet Union",  "Japan",      "China",     "India",     "Africa",           "Africa",           "Middle East",     "Pacific OECD",   "Pacific OECD",     "Southeast Asia","Southeast Asia",       "Southeast Asia",   "Southeast Asia"),
  stringsAsFactors = FALSE
)

                                                                                  
mprs <- readxl::read_xlsx("data/model_proj_region_selection.xlsx")

scenario_storage <- v_data_timeTechTemp_regional %>% 
  filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass") %>% 
  #inner_join(def_regions, by=c("region")) %>% 
  filter(period %in% seq(2010,2100,10)) %>% 
  filter(!tempcat %in% c("Other scenario", "Likely 3.0°C scenario")) %>%
  mutate(tempcat = ifelse(tempcat %in% c("1.5°C scenario"), "1.5°C", "2.0°C")) %>% 
  select(-variable) %>% 
  group_by(model,scenario,region,tempcat) %>% 
  mutate(dt=0.5*(lead(period, default=2100) - period) + 0.5*(period - lag(period, default=2010))) %>% 
  summarise(value=sum(value/1000*dt)) %>% 
  ungroup() %>% 
  mutate(proj=substr(scenario,1,4)) %>% 
  inner_join(mprs, by=c("model","proj","region")) %>% 
  filter(keep == TRUE) %>% 
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

# ggplot(scenario_storage) + 
#   geom_bar(aes(x=tempcat, y=med, fill=tempcat), stat="identity")
# 
# ggplot(dooley_storage) + 
#   geom_bar(aes(x=region, y=value, fill=storage_type), stat="identity") +
#   facet_wrap(~region, ncol=6)
# 
# 
# ggplot(scenario_storage) + 
#   geom_boxplot(aes(x=tempcat, ymin=min, ymax=max, lower=p25, upper=p75, middle=med, fill=tempcat), stat="identity") +
#   facet_wrap(~region2, scales="free_y") +
#   theme_minimal() +
#   theme(legend.position="bottom") +
#   #guides(fill=FALSE) +
#   xlab("")

storage_colors <- data.frame(
  storage_type = c("Deep Saline Filled Formations-onshore",
                   "Deep Saline Filled Formations-offshore",
                   "Depleted Oil and Gas Fields",
                   "Deep Unmineable Coal Seams"),
  color        = c("#494529ff",
                   "#93ccddff",
                   "#fe0000ff",
                   "#212526ff"),
  stringsAsFactors = FALSE
)

dooley_storage %>% 
  group_by(region) %>% 
  summarise(value=sum(value)) %>% 
  ungroup()

edmonds_storage %>% 
  group_by(region) %>% 
  summarise(value=sum(value)) %>% 
  ungroup()

region_ymax <- data_frame(
  region = c("Africa",             # 11
             "China",              # 532
             "Europe",             #216
             "Former Soviet Union", # 171 
             "India",           #87
             "Japan",           #10
             "Latin America",   #901
             "Middle East",     #175
             "Southeast Asia",  #26 
             "Pacific OECD",    # 
             "USA",             #4273
             "Other"),          #?
  ymax   = c(100, 200, 200, 200, 100, 30, 500, 200, 300, 100, 200, 400),
  stringsAsFactors = FALSE
)

svglite::svglite("storage_scenarios.svg", width=10, height=10)
par(mfrow=c(3,4), las=1)

for (kreg in c("Africa", "China", "Europe", "Former Soviet Union", "India", "Japan", "Latin America", "Middle East", "Southeast Asia", "USA", "Pacific OECD")) {
 
  if (kreg == "Africa") {  
   plot(0,0,type="n",
        xaxs="i", yaxs="i",
        xlim=c(0.5,4.5),ylim=c(0,region_ymax$ymax[which(region_ymax$region == kreg)]),
        xlab="",ylab="",axes=F, main=kreg)
  } else {
    #par(new=TRUE)
    plot(0,0,type="n",
         xaxs="i", yaxs="i",
         xlim=c(0.5,4.5),ylim=c(0,region_ymax$ymax[which(region_ymax$region == kreg)]),
         xlab="",ylab="",axes=F, main=kreg)
  }
 
 offset=0.35
 
 lines(c(0.5,4.5), c(0,0))
 
 # Capacity
 pos=1
 curval=0
 for (kst in c("Deep Unmineable Coal Seams",
               "Depleted Oil and Gas Fields",
               "Deep Saline Filled Formations-onshore",
               "Deep Saline Filled Formations-offshore")) {
   print(paste(kreg, ":", kst, " > ", dooley_storage$value[which(dooley_storage$region == kreg & dooley_storage$storage_type == kst)]))
   rect(
     pos-offset, curval,
     pos+offset, curval+dooley_storage$value[which(dooley_storage$region == kreg & dooley_storage$storage_type == kst)],
     col=storage_colors$color[which(storage_colors$storage_type == kst)],
     border=NA)
   
   curval <- curval+dooley_storage$value[which(dooley_storage$region == kreg & dooley_storage$storage_type == kst)]
 }
 
 # Capacity
 pos=2
 curval=0
 for (kst in c("Deep Unmineable Coal Seams",
               "Depleted Oil and Gas Fields",
               "Deep Saline Filled Formations-onshore",
               "Deep Saline Filled Formations-offshore")) {
   print(paste(kreg, ":", kst, " > ", edmonds_storage$value[which(edmonds_storage$region == kreg & edmonds_storage$storage_type == kst)]))
   rect(
     pos-offset, curval,
     pos+offset, curval+edmonds_storage$value[which(edmonds_storage$region == kreg & edmonds_storage$storage_type == kst)],
     col=storage_colors$color[which(storage_colors$storage_type == kst)],
     border=NA)
   
   curval <- curval+edmonds_storage$value[which(edmonds_storage$region == kreg & edmonds_storage$storage_type == kst)]
 }
 
 
 # 2°C
 color <- "#3f91c5"
 pos = 3
 curtemp = "2.0°C"
 lines(c(pos, pos), 
       c(scenario_storage$min[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], 
         scenario_storage$max[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)]), 
         col=color)
 rect(
   pos-offset, scenario_storage$p15[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], 
   pos+offset, scenario_storage$p85[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], 
   col=color, border=NA
 )
 lines(c(pos-offset, pos+offset), 
       c(scenario_storage$med[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], scenario_storage$med[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)]), col="#ffffff")
 points(c(pos, pos), 
        c(scenario_storage$mean[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], scenario_storage$mean[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)]), pch=21, col=color, bg="#ffffff")
 
 
 # 1.5°C
 if (kreg != "Pacific OECD") {
   print("HELLO!")
   color <- "#074594"
   pos = 4
   curtemp = "1.5°C"
   lines(c(pos, pos), 
         c(scenario_storage$min[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], 
           scenario_storage$max[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)]), 
         col=color)
   rect(
     pos-offset, scenario_storage$p15[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], 
     pos+offset, scenario_storage$p85[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], 
     col=color, border=NA
   )
   lines(c(pos-offset, pos+offset), 
         c(scenario_storage$med[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], scenario_storage$med[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)]), col="#ffffff")
   points(c(pos, pos), 
          c(scenario_storage$mean[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)], scenario_storage$mean[which(scenario_storage$region2 == kreg & scenario_storage$tempcat == curtemp)]), pch=21, col=color, bg="#ffffff")
 }
 
 
 axis(2)
  
}
par(mfrow=c(1,1))
dev.off()

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