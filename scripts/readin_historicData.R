rfilename = "data/scenarios/RData/SSP_History.RData"
if (!file.exists(rfilename)) {
  tmp <- read.csv("data/scenarios/SSP_History1970_5Regs_2015-04-20.csv")
  
  names(tmp) <- tolower(names(tmp))
  names(tmp)[6:22] <- substr(names(tmp)[6:22],2,5)
  
  dat_histSSP <- tmp %>% 
    gather(period, value, -model, -scenario, -region, -variable, -unit) %>% 
    mutate(period = as.numeric(period)) 
  save(dat_histSSP, file=rfilename)
} else {
  load(rfilename)
}