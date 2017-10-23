rfilename = "data/scenarios/RData/RCP.RData"
if (!file.exists(rfilename)) {
  tmp <- readxl::read_xlsx("data/scenarios/rcp_all_scenarios.xlsx", sheet=1)
  
  tmp <- tmp[,-17]  
    
  dat_rcp <- tmp %>% 
    separate(Scenario, into=c("model", "scenario"), sep=" - ") %>% 
    rename(region = Region) %>% 
    rename(variable = Variable) %>% 
    rename(unit = Unit) %>% 
    gather(period, value, -model, -region, -scenario, -variable, -unit) %>% 
    select(model,region,scenario,variable,unit,period,value) %>% 
    mutate(period = as.numeric(period))
  
  save(dat_rcp, file = rfilename)
} else {
  load(rfilename)
}