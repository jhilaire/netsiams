#load("../../bitbucket/beccs/data/dataAll.RData")


# Compute aggregated consumption losses
data_plot <- v_data3 %>% 
  filter(region == "World", variable == "Policy Cost|Consumption Loss") %>% 
  filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  mutate(tempcat = paste(tempcat)) %>% 
  mutate(tempcat = ifelse(tempcat == "1.5°C scenario", "1.5°C", "2°C")) %>% 
  filter(period %in% c(seq(2010,2055,5), seq(2060,2100,10))) %>% 
  #filter(period %in% seq(2010,2100,10)) %>% 
  mutate(value=ifelse(grepl("Frag", timing), -value, value)) %>% 
  group_by(model,scenario,tempcat,techcat,timingcat) %>% 
  arrange(period) %>% 
  mutate(allinfo=paste0(period,"-",lead(period, default=2100),",",value,"#",lead(value))) %>% 
  filter(period != 2100) %>% 
  mutate(period=paste0(period+1,"-",lead(period, default=2100))) %>% 
  mutate(sum_disc=sapply(allinfo, function(x) {
    
    info_period <- strsplit(x, ",")[[1]][1]
    info_value  <- strsplit(x, ",")[[1]][2]
    
    period_start <- as.numeric(strsplit(info_period, "-")[[1]][1]) +1
    period_end   <- as.numeric(strsplit(info_period, "-")[[1]][2])
    
    value_start <- as.numeric(strsplit(info_value, "#")[[1]][1])
    value_end   <- as.numeric(strsplit(info_value, "#")[[1]][2])
    
    res_interp <- approx(c(period_start, period_end), c(value_start, value_end), seq(period_start,period_end,1))
    
    out <- sum(res_interp$y*1/(1+0.05)^(seq(period_start,period_end,1) - 2011))
    
    return(out)
  })) %>% 
  dplyr::select(-allinfo) %>% 
  ungroup() %>% 
  group_by(model,scenario,tempcat,techcat,timingcat) %>% 
  summarise(value=-sum(sum_disc)/1000) %>% 
  ungroup() %>% 
  inner_join(v_data3 %>% filter(region == "World", period == 2005, variable == "Emissions|CO2") %>% select(scenario, filter)) %>% 
  filter(!techcat %in% c("lowEI", "other")) %>% 
  mutate(techcat = ifelse(techcat %in% c("noCCS", "noBECCS"), "No CCS/BECCS", techcat))



data_plot <- v_data3 %>% 
  filter(region == "World", variable == "Policy Cost|GDP Loss") %>% 
  filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  mutate(tempcat = paste(tempcat)) %>% 
  mutate(tempcat = ifelse(tempcat == "1.5°C scenario", "1.5°C", "2°C")) %>% 
  filter(period %in% c(seq(2010,2055,5), seq(2060,2100,10))) %>% 
  #filter(period %in% seq(2010,2100,10)) %>% 
  mutate(value=ifelse(grepl("Frag", timing), -value, value)) %>% 
  group_by(model,scenario,tempcat,techcat,timingcat) %>% 
  arrange(period) %>% 
  mutate(allinfo=paste0(period,"-",lead(period, default=2100),",",value,"#",lead(value))) %>% 
  filter(period != 2100) %>% 
  mutate(period=paste0(period+1,"-",lead(period, default=2100))) %>% 
  mutate(sum_disc=sapply(allinfo, function(x) {
    
    info_period <- strsplit(x, ",")[[1]][1]
    info_value  <- strsplit(x, ",")[[1]][2]
    
    period_start <- as.numeric(strsplit(info_period, "-")[[1]][1]) +1
    period_end   <- as.numeric(strsplit(info_period, "-")[[1]][2])
    
    value_start <- as.numeric(strsplit(info_value, "#")[[1]][1])
    value_end   <- as.numeric(strsplit(info_value, "#")[[1]][2])
    
    res_interp <- approx(c(period_start, period_end), c(value_start, value_end), seq(period_start,period_end,1))
    
    out <- sum(res_interp$y*1/(1+0.05)^(seq(period_start,period_end,1) - 2011))
    
    return(out)
  })) %>% 
  dplyr::select(-allinfo) %>% 
  ungroup() %>% 
  group_by(model,scenario,tempcat,techcat,timingcat) %>% 
  summarise(value=-sum(sum_disc)/1000) %>% 
  ungroup() %>% 
  inner_join(v_data3 %>% 
               filter(region == "World", period == 2005, 
                      variable == "Emissions|CO2") %>% 
               select(scenario, filter)) %>% 
  filter(!techcat %in% c("lowEI", "other")) %>% 
  mutate(techcat = ifelse(techcat %in% c("noCCS", "noBECCS"), "No CCS/BECCS", techcat))
  


# Compute consumption losses from scratch
v_data3 %>% 
  filter(region == "World", variable == "Consumption") %>% 
  inner_join(
    v_data3 %>% 
      filter(region == "World", period == 2005, 
             variable == "Emissions|CO2") %>% 
      select(scenario, technology, timing, tax_level) %>% 
      filter(technology %in% c("Default", "NoCCS", "NoBECCS", "LimBio")) %>% 
      mutate(technology = ifelse(technology %in% c("NoCCS", "NoBECCS"), "NoCCS/BECCS", technology)) %>% 
      inner_join(
        v_data3 %>% 
          filter(region == "World", period == 2005, 
                 variable == "Emissions|CO2") %>% 
          select(scenario, technology, timing, tax_level) %>% 
          filter(technology %in% c("Default", "NoCCS", "NoBECCS", "LimBio")) %>% 
          mutate(technology = ifelse(technology %in% c("NoCCS", "NoBECCS"), "NoCCS/BECCS", technology)) %>% 
          group_by(technology,timing,tax_level) %>% 
          filter(tax_level==0) %>% 
          rename(baseline=scenario) %>% 
          ungroup() %>% 
          select(-tax_level)
    )) %>% 
  inner_join(
    v_data3 %>% 
      filter(region == "World", variable == "Consumption", temp_cat == "Baseline") %>% 
      select(scenario,region,variable,period,value) %>% 
      rename(value_bau=value),
    by=c("baseline"="scenario", "region", "variable", "period")
  ) %>% 
  select(scenario,region,variable,period,value,value_bau,tempcat,timingcat,techcat) %>% 
  filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  mutate(tempcat = paste(tempcat)) %>% 
  mutate(tempcat = ifelse(tempcat == "1.5°C scenario", "1.5°C", "2°C")) %>% 
  mutate(value=(value-value_bau)/1000) %>% 
  filter(period %in% c(seq(2010,2055,5), seq(2060,2100,10))) %>% 
  #filter(period %in% seq(2010,2100,10)) %>% 
  #mutate(value=ifelse(grepl("Frag", timing), -value, value)) %>% 
  group_by(model,scenario,tempcat,techcat,timingcat) %>% 
  arrange(period) %>% 
  mutate(allinfo=paste0(period,"-",lead(period, default=2100),",",value,"#",lead(value))) %>% 
  filter(period != 2100) %>% 
  mutate(period=paste0(period+1,"-",lead(period, default=2100))) %>% 
  mutate(sum_disc=sapply(allinfo, function(x) {
    
    info_period <- strsplit(x, ",")[[1]][1]
    info_value  <- strsplit(x, ",")[[1]][2]
    
    period_start <- as.numeric(strsplit(info_period, "-")[[1]][1]) +1
    period_end   <- as.numeric(strsplit(info_period, "-")[[1]][2])
    
    value_start <- as.numeric(strsplit(info_value, "#")[[1]][1])
    value_end   <- as.numeric(strsplit(info_value, "#")[[1]][2])
    
    res_interp <- approx(c(period_start, period_end), c(value_start, value_end), seq(period_start,period_end,1))
    
    out <- sum(res_interp$y*1/(1+0.05)^(seq(period_start,period_end,1) - 2011))
    
    return(out)
  })) %>% 
  dplyr::select(-allinfo) %>% 
  ungroup() %>% 
  group_by(model,scenario,tempcat,techcat,timingcat) %>% 
  summarise(value=-sum(sum_disc)/1000) %>% 
  ungroup() %>% 
  inner_join(v_data3 %>% 
               filter(region == "World", period == 2005, 
                      variable == "Emissions|CO2") %>% 
               select(scenario, filter)) %>% 
  filter(!techcat %in% c("lowEI", "other")) %>% 
  mutate(techcat = ifelse(techcat %in% c("noCCS", "noBECCS"), "No CCS/BECCS", techcat))

