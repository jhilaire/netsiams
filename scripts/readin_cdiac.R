rfilename = "data/scenarios/RData/CDIAC.RData"
if (!file.exists(rfilename)) {
  
  dat_cdiac <- read.csv("data/scenarios/cdiac_nation.1751_2011.csv", header=FALSE, skip=3, na.strings=".")
  colnames(dat_cdiac) <- c("region", "period", 
                           "Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)",
                           "Emissions from solid fuel consumption",
                           "Emissions from liquid fuel consumption",
                           "Emissions from gas fuel consumption",
                           "Emissions from cement production",
                           "Emissions from gas flaring",
                           "Per capita CO2 emissions (metric tons of carbon)",
                           "Emissions from bunker fuels (not included in the totals)")
  dat_cdiac <- dat_cdiac %>% 
    gather(variable, value, -region, -period) 
  
  dat_cdiac <- readxl::read_xlsx("data/scenarios/Global_Carbon_Budget_2015_v1.1.xlsx", sheet=3, startRow=11)
  colnames(dat_cdiac) <- c("period", "total", 
                           "coal",
                           "oil",
                           "gas",
                           "cement",
                           "flaring",
                           "percapita",
                           "dump")
  dat_cdiac <- dat_cdiac %>%
    select(-dump) %>% 
    gather(variable, value, -period)
  
  save(dat_cdiac, file = rfilename)
} else {
  load(rfilename)
}
