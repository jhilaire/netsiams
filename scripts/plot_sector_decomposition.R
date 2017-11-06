tmp <- v_data_timeTechTemp_world_plot %>% 
  filter(grepl("Emissions.CO2.*Energy Supply.Electricity|Emissions.CO2.*Energy Demand.*|Emissions.CO2.Land|Emissions.CH4|Emissions.N2O|F-Gases", variable), 
         period %in% c(2030,2050,2100),
         tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  mutate(tempcat = paste(tempcat)) %>% 
  mutate(tempcat = ifelse(tempcat == "1.5°C scenario", "1.5°C", "2°C")) %>% 
  mutate(model.scenario = paste0(model,scenario)) %>% 
  mutate(period=paste(period)) %>% 
  mutate(value = value*1e-3) %>% 
  mutate(value = ifelse(grepl("CH4", variable), 25*value, value)) %>% 
  mutate(value = ifelse(grepl("N2O", variable), 298*value*1e-3, value)) %>% 
  mutate(variable = ifelse(grepl("CH4", variable),                    "CH4", 
                           ifelse(grepl("N2O", variable),                    "N2O", 
                                  ifelse(grepl("F-Gases", variable),                "F-Gases", 
                                         ifelse(grepl("Supply", variable),                 "CO2 - Power", 
                                                ifelse(grepl("Energy Demand.Industry", variable), "CO2 - Industry", 
                                                       ifelse(grepl("Residential", variable),            "CO2 - Buildings", 
                                                              ifelse(grepl("Transport", variable),              "CO2 - Transportation", 
                                                                     ifelse(grepl("Land", variable),                   "CO2 - Land-Use", 
                                                                            "CO2 - Agg. demand"))))))))) %>% 
  spread(variable, value) %>% 
  mutate(nonCO2KG = `CH4` + `N2O` + `F-Gases`) %>% 
  select(-`CH4`, -`N2O`, -`F-Gases`) %>%
  mutate(`CO2 - Industry+Buildings` = `CO2 - Agg. demand`- `CO2 - Transportation`) %>% 
  gather(variable, value, -model, -scenario, -region, -model.scenario, -period, -tempcat, -allcat, -techcat, -timingcat) %>% 
  mutate(variable = factor(variable, levels=c("CO2 - Power", "CO2 - Agg. demand", "CO2 - Industry+Buildings", "CO2 - Industry", 
                                              "CO2 - Buildings","CO2 - Transportation", "CO2 - Land-Use",
                                              "nonCO2KG"), ordered=TRUE))

tmp <- ar5data %>% 
  #ar5rm_2050models() %>% 
  mutate(period = as.numeric(format(period, "%Y"))) %>% 
  filter(region == "World", 
         period %in% c(2030,2050,2100),
         grepl("Emissions.CO2.*Energy Supply$|Emissions.CO2.*Energy Demand.*|Emissions.CO2.Land|Emissions.CH4|Emissions.N2O|F-Gases", variable)) %>% 
  filter(!is.na(value)) %>% 
  inner_join(ar5scen %>% 
               select(model,scenario,climate_metric,climate_value,climate,policy,technology), 
             by=c("model", "scenario")) %>% 
  filter(climate %in% c("Category 1", "Category 1 - 2"),
         policy == "P1") %>% 
  mutate(tempcat = "2°C") %>% 
  mutate(allcat = ifelse(technology == "T0", "Default",
                  ifelse(technology == "T3" | grepl("noccs|nobeccs|eere|no beccs", tolower(scenario)), "No CCS/BECCS", "other"))) %>% 
  mutate(model.scenario = paste0(model,scenario)) %>% 
  mutate(period=paste(period)) %>% 
  mutate(value = value*1e-3) %>% 
  mutate(value = ifelse(grepl("CH4", variable), 25*value, value)) %>% 
  mutate(value = ifelse(grepl("N2O", variable), 298*value*1e-3, value)) %>%
  select(-unit) %>% 
  mutate(variable = ifelse(grepl("CH4", variable),                    "CH4", 
                    ifelse(grepl("N2O", variable),                    "N2O", 
                    ifelse(grepl("F-Gases", variable),                "F-Gases", 
                    ifelse(grepl("Supply", variable),                 "Electricity", 
                    ifelse(grepl("Energy Demand.Industry", variable), "Industry", 
                    ifelse(grepl("Residential", variable),            "Buildings", 
                    ifelse(grepl("Transport", variable),              "Transportation", 
                    ifelse(grepl("Land", variable),                   "Net AFOLU", 
                                                                      "Agg. demand"))))))))) %>% 
  spread(variable, value) %>% 
  mutate(`Non CO2` = `CH4` + `N2O` + `F-Gases`) %>% 
  select(-`CH4`, -`N2O`, -`F-Gases`) %>%
  mutate(`Industry+Buildings` = `Agg. demand`- `Transportation`) %>% 
  gather(variable, value, -model, -scenario, -region, -model.scenario, -period, -tempcat, -allcat, -climate_metric, -climate_value, -climate, -policy, -technology) %>% 
  mutate(variable = factor(variable, levels=c("Electricity", "Agg. demand", "Industry+Buildings", "Industry", 
                                              "Buildings","Transportation", "Net AFOLU",
                                              "Non CO2"), ordered=TRUE))

tmp <- ar5data %>% 
  #ar5rm_2050models() %>% 
  mutate(period = as.numeric(format(period, "%Y"))) %>% 
  filter(region == "World", 
         period %in% c(2030,2050,2100),
         grepl("Emissions.CO2.*Energy Supply.Electricity|Emissions.CO2.*Energy Demand.*|Emissions.CO2.Land|Emissions.CH4|Emissions.N2O|F-Gases", variable)) %>% 
  filter(!is.na(value)) %>% 
  inner_join(ar5scen %>% 
               select(model,scenario,climate_metric,climate_value,climate,policy,technology), 
             by=c("model", "scenario")) %>% 
  filter(climate %in% c("Category 1", "Category 1 - 2"),
         policy == "P1") %>% 
  mutate(tempcat = "2°C") %>% 
  mutate(allcat = ifelse(technology == "T0", "Default",
                         ifelse(technology == "T3" | grepl("noccs|nobeccs|eere|no beccs", tolower(scenario)), "No CCS/BECCS", "other"))) %>% 
  mutate(model.scenario = paste0(model,scenario)) %>% 
  mutate(period=paste(period)) %>% 
  mutate(value = value*1e-3) %>% 
  mutate(value = ifelse(grepl("CH4", variable), 25*value, value)) %>% 
  mutate(value = ifelse(grepl("N2O", variable), 298*value*1e-3, value)) %>%
  select(-unit) %>% 
  mutate(variable = ifelse(grepl("CH4", variable),                    "CH4", 
                           ifelse(grepl("N2O", variable),                    "N2O", 
                                  ifelse(grepl("F-Gases", variable),                "F-Gases", 
                                         ifelse(grepl("Supply", variable),                 "Electricity", 
                                                ifelse(grepl("Energy Demand.Industry", variable), "Industry", 
                                                       ifelse(grepl("Residential", variable),            "Buildings", 
                                                              ifelse(grepl("Transport", variable),              "Transportation", 
                                                                     ifelse(grepl("Land", variable),                   "Net AFOLU", 
                                                                            "Agg. demand"))))))))) %>% 
  spread(variable, value) %>% 
  mutate(`Non CO2` = `CH4` + `N2O` + `F-Gases`) %>% 
  select(-`CH4`, -`N2O`, -`F-Gases`) %>%
  mutate(`Industry & Buildings` = `Agg. demand`- `Transportation`) %>% 
  gather(variable, value, -model, -scenario, -region, -model.scenario, -period, -tempcat, -allcat, -climate_metric, -climate_value, -climate, -policy, -technology) %>% 
  mutate(variable = factor(variable, levels=c("Electricity", "Agg. demand", "Industry & Buildings", "Industry", 
                                              "Buildings","Transportation", "Net AFOLU",
                                              "Non CO2"), ordered=TRUE)) %>% 
  select(model,scenario,model.scenario,region,period,variable,value,tempcat,allcat) %>% 
  rbind(v_data_timeTechTemp_world_plot %>% 
          filter(grepl("Emissions.CO2.*Energy Supply|Emissions.CO2.*Energy Demand.*|Emissions.CO2.Land|Emissions.CH4|Emissions.N2O|F-Gases", variable), 
                 period %in% c(2030,2050,2100),
                 tempcat %in% c("1.5°C scenario")) %>% 
          mutate(tempcat = paste(tempcat)) %>% 
          mutate(tempcat = "1.5°C") %>% 
          mutate(allcat = "Default") %>% 
          mutate(model.scenario = paste0(model,scenario)) %>% 
          mutate(period=paste(period)) %>% 
          mutate(value = value*1e-3) %>% 
          mutate(value = ifelse(grepl("CH4", variable), 25*value, value)) %>% 
          mutate(value = ifelse(grepl("N2O", variable), 298*value*1e-3, value)) %>% 
          mutate(variable = ifelse(grepl("CH4", variable),                    "CH4", 
                            ifelse(grepl("N2O", variable),                    "N2O", 
                            ifelse(grepl("F-Gases", variable),                "F-Gases", 
                            ifelse(grepl("Supply", variable),                 "Electricity", 
                            ifelse(grepl("Energy Demand.Industry", variable), "Industry", 
                            ifelse(grepl("Residential", variable),            "Buildings", 
                            ifelse(grepl("Transport", variable),              "Transportation", 
                            ifelse(grepl("Land", variable),                   "Net AFOLU", 
                                                                              "Agg. demand"))))))))) %>% 
          spread(variable, value) %>% 
          mutate(`Non CO2` = `CH4` + `N2O` + `F-Gases`) %>% 
          select(-`CH4`, -`N2O`, -`F-Gases`) %>%
          mutate(`Industry & Buildings` = `Agg. demand`- `Transportation`) %>% 
          gather(variable, value, -model, -scenario, -region, -model.scenario, -period, -tempcat, -allcat, -techcat, -timingcat) %>% 
          mutate(variable = factor(variable, levels=c("Electricity", "Agg. demand", "Industry & Buildings", "Industry", 
                                                      "Buildings","Transportation", "Net AFOLU",
                                                      "Non CO2"), ordered=TRUE)) %>% 
          select(model,scenario,model.scenario,region,period,variable,value,tempcat,allcat))

  
#== PLOT DATA ========
# svg export (original 2x2 layout: 976x600) (new 1x3 layout: 1100*600)
p_emiAll_bySec_byTemp <- ggplot(tmp %>% 
                                  filter(!variable %in% c("Agg. demand", "Industry", "Buildings"),
                                         allcat %in% c("Default", "No CCS/BECCS")) %>% 
                                  mutate(period=factor(period))) + 
  geom_boxplot(aes(x=variable, y=value, fill=variable, alpha=period)) + 
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax), data=data.frame(variable=c("Electricity", "Industry & Buildings", 
                                                                                     "Transportation", "Net AFOLU",
                                                                                     "Non CO2"), xmin=0.5, xmax=5.5, ymin=0, ymax=0), lty=2, color="red") +
  facet_wrap(allcat ~ tempcat, ncol=3, scales = "free_y") + 
  theme_bw() + 
  theme(legend.position="bottom") +
  #ylim(-30,40) +
  xlab("") + ylab("Emissions [Gt CO2-eq/yr]") +
  scale_fill_manual(name="Sector",
                    values=c("Electricity"=RColorBrewer::brewer.pal(5, "Set1")[1],
                             "Industry & Buildings"=RColorBrewer::brewer.pal(5, "Set1")[5],
                             "Transportation"=RColorBrewer::brewer.pal(5, "Set1")[2],
                             "Net AFOLU"=RColorBrewer::brewer.pal(5, "Set1")[3],
                             "Non CO2"=RColorBrewer::brewer.pal(5, "Set1")[4]))
print(p_emiAll_bySec_byTemp)