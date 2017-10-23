rfilename = "data/scenarios/RData/IS92.RData"
if (!file.exists(rfilename)) {
  
  #== READ IN DATA ==========================
  dat_is92a = readxl::read_xlsx("data/scenarios/IS92_Scenarios_A-F_ver1.1_final.xlsx", sheet = 5,  colNames = FALSE)
  dat_is92b = readxl::read_xlsx("data/scenarios/IS92_Scenarios_A-F_ver1.1_final.xlsx", sheet = 6,  colNames = FALSE)
  dat_is92c = readxl::read_xlsx("data/scenarios/IS92_Scenarios_A-F_ver1.1_final.xlsx", sheet = 7,  colNames = FALSE)
  dat_is92d = readxl::read_xlsx("data/scenarios/IS92_Scenarios_A-F_ver1.1_final.xlsx", sheet = 8,  colNames = FALSE)
  dat_is92e = readxl::read_xlsx("data/scenarios/IS92_Scenarios_A-F_ver1.1_final.xlsx", sheet = 9,  colNames = FALSE)
  dat_is92f = readxl::read_xlsx("data/scenarios/IS92_Scenarios_A-F_ver1.1_final.xlsx", sheet = 10, colNames = FALSE)
  
  #== PROCESS DATA ==========================
  dat_is92a = process_IS92(dat_is92a, "IS92-A")
  dat_is92b = process_IS92(dat_is92b, "IS92-B")
  dat_is92c = process_IS92(dat_is92c, "IS92-C")
  dat_is92d = process_IS92(dat_is92d, "IS92-D")
  dat_is92e = process_IS92(dat_is92e, "IS92-E")
  dat_is92f = process_IS92(dat_is92f, "IS92-F")
  
  dat_is92 = bind_rows(dat_is92a, dat_is92b, dat_is92c, dat_is92d, dat_is92e, dat_is92f) %>% 
    mutate(scenario = factor(scenario)) %>% 
    mutate(region   = factor(region)) %>% 
    mutate(variable = factor(variable)) %>% 
    mutate(period   = as.numeric(period))
  
  #== CLEAN UP ==============================
  rm(dat_is92a, dat_is92b, dat_is92c, dat_is92d, dat_is92e, dat_is92f)
  
  save(dat_is92, file = rfilename)
} else {
  load(rfilename)
}

