rfilename = "data/scenarios/RData/SRES.RData"
if (!file.exists(rfilename)) {
  
  # SRES
  sres_scenarios <- c("A1", "A1C", "A1G", "A1V1", "A1V2", "A1T", "A2", "A2G", "A2-A1",
                      "B1", "B1T", "B1HIGH", "B2", "B2HIGH", "B2C")
  sres_models <- c("AIM", "ASF", "IMAGE", "MESSAGE", "MINICAM", "MARIA")
  time = c(1990,2000,2010,2020,2030,2040,2050,2060,2070,2080,2090,2100)
  
  skips = c()
  dat_sres = data.frame()
  
  for (k_scen in sres_scenarios) {
    for (k_mod in sres_models) {
      
      k_scenXmod = paste0(k_scen, " ", k_mod)
      
      .SKIP. = FALSE
      tryCatch(
        tmp <- readxl::read_xlsx("data/scenarios/sres_all_scenarios.xlsx", sheet=k_scenXmod, colNames=FALSE, startRow=2, skipEmptyRows=FALSE),
        error = function(e) {
          skips <<- c(skips, paste0(k_scen, " ", k_mod))
          .SKIP. <<- TRUE
          #print(paste0("Combination: ", k_scen, " ", k_mod, " does not exsist!"))
        })
      
      if (!.SKIP.) {
        
        # Check if all regions have the same number of rows
        rowids = which(grepl(k_scenXmod, tmp$X1))
        if (!all(duplicated(rowids[-1] - rowids[-length(rowids)])[-1])) stop()
        nbrows = rowids[2]-rowids[1]
        nbregs = length(rowids)
        
        for (kreg in 1:nbregs) {
          
          tmp2 = tmp[(1+(kreg-1)*nbrows):(kreg*nbrows),]
          
          region   = gsub(" ", "", strsplit(tmp2[1,1], "-")[[1]][1])
          scenario = strsplit(strsplit(tmp2[1,1], "-")[[1]][2], " ", fixed=TRUE)[[1]][2]
          model    = strsplit(strsplit(tmp2[1,1], "-")[[1]][2], " ", fixed=TRUE)[[1]][3]
          
          #Energy content
          energy_content_beg = c(8, 17,27,36)
          energy_content_end = c(15,25,30,42)
          for (k in 1:length(energy_content_beg)) {
            sector = tmp2[energy_content_beg[k],1]
            unit   = tmp2[energy_content_beg[k],3]
            
            if (!is.na(unit)) { 
              tmp2[(energy_content_beg[k]+1):energy_content_end[k],1] = paste0(sector,"|", gsub(" ", "", tmp2[(energy_content_beg[k]+1):energy_content_end[k],2]))
              tmp2[(energy_content_beg[k]+1):energy_content_end[k],3] = rep(unit, energy_content_end[k]-energy_content_beg[k])  
            } else {
              
              if (sector == "Cumulative Resources Production ZJ") {
                
                sector = "Cumulative Resources Production"
                unit   = "ZJ"
                
                tmp2[(energy_content_beg[k]+1):energy_content_end[k],1] = paste0(sector,"|", gsub(" ", "", tmp2[(energy_content_beg[k]+1):energy_content_end[k],2]))
                tmp2[(energy_content_beg[k]+1):energy_content_end[k],3] = rep(unit, energy_content_end[k]-energy_content_beg[k])
                
              } else {
                print("Case without unit")
                tmp2[(energy_content_beg[k]+1):energy_content_end[k],1] = paste0(sector,"|", gsub(" ", "", tmp2[(energy_content_beg[k]+1):energy_content_end[k],2]))
                tmp2[(energy_content_beg[k]+1):energy_content_end[k],3] = rep(unit, energy_content_end[k]-energy_content_beg[k])
              }
            }
          }
          
          #Emission content
          emission_content_beg = c(44)
          emission_content_end = c(56)
          for (k in 1:length(emission_content_beg)) {
            sector = tmp2[emission_content_beg[k],1]
            
            tmp2[(emission_content_beg[k]+1):emission_content_end[k],1] = paste0(sector,"|", gsub(" ", "", tmp[(emission_content_beg[k]+1):emission_content_end[k],2]))
          }
          
          tmp2 = tmp2[-c(1,energy_content_beg, emission_content_beg),] # remove also first line (header) and unecessary rows
          tmp2 = tmp2[which(!is.na(tmp2$X1)),]
          
          #tmp2[4,1] = "GNP/GDP per capita (mex)"
          tmp2 = tmp2[,c(-2,-16)] # remove unecessary columns (sub sectors and remarks)
          
          colnames(tmp2) <- c("variable","unit",paste0(time)) 
          
          tmp2 = tmp2 %>% 
            gather(period,value,-variable,-unit) %>% 
            mutate(model = model) %>% 
            mutate(scenario = paste0("SRES-",scenario)) %>% 
            mutate(region = region) %>% 
            select(model,scenario,region,variable,unit,period,value)
          
          dat_sres <- rbind(dat_sres, tmp2) 
          
        } # end loop region
      } # skip?
    }
  }
  
  dat_sres <- dat_sres %>% 
    mutate(model = factor(model)) %>% 
    mutate(scenario = factor(scenario)) %>% 
    mutate(region = factor(region)) %>% 
    mutate(variable = factor(variable)) %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(value = as.numeric(value))
  
  print(paste0("Skipping following missing combinations: ", paste(skips, collapse=", ")))
  
  
  save(dat_sres, file = rfilename)
} else {
  load(rfilename)
}



