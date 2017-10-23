rfilename = "../../data/RData/IEA.RData"
if (!file.exists(rfilename)) {
  tmp <- read.csv2("../../data/iea_2015_co2/processed_data/iea_2015_co2ref_total.csv", na.strings = "nan")
  
  names(tmp) <- c("region", "code", substr(names(tmp)[3:56],2,5))
  
  dat_iea <- tmp %>% 
    gather(period, value, -region, -code) %>% 
    mutate(model="IEA") %>% 
    mutate(scenario="IEA") %>% 
    mutate(variable="Total CO2 emissions") %>% 
    select(model,scenario,region,variable,period,value) %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(value = as.numeric(value))
  
  save(dat_iea, file = rfilename)
} else {
  load(rfilename)
}

