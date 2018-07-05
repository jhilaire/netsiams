data_plot <- rbind(
  # data_edmonds %>% 
  #   filter(scenario %in% c("T1(Ref)xIdealized", "T3(No CCS)xIdealized",
  #                          "T1(Ref)xDelayed",   "T3(No CCS)xDelayed")) %>% 
  #   rename(variable=technology) %>% 
  #   mutate(study="Edmonds et al (2013)") %>% 
  #   select(study,scenario,variable,value),
  data_hump %>% 
    filter(region == "World", 
           scenario %in% c("AFF", "BECCS", "AFF+BECCS"),
           variable %in% c("CO2 Emissions BECCS", "CO2 Emissions Land incl Defor+AFF"), 
           period == 2095) %>% 
    mutate(value=ifelse(value > 0, 0, value)) %>% 
    mutate(value=-value) %>% 
    mutate(variable=paste(factor(variable, levels=rev(c("CO2 Emissions BECCS", "CO2 Emissions Land incl Defor+AFF")), labels=rev(c("BECCS", "AR")), ordered=TRUE))) %>% 
    mutate(scenario=paste(factor(scenario, levels=rev(c("AFF", "BECCS", "AFF+BECCS")), labels=rev(c("2°C-AR", "2°C-BECCS", "2°C-AR+BECCS")), ordered=TRUE))) %>% 
    mutate(study="Humpenöder et al (2014)") %>% 
    select(study,scenario,variable,value),
  data_marcucci2017 %>% 
    filter(scenario != "BAU") %>%
    filter(variable %in% c("Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Direct Air Capture")) %>% 
    select(-model,-unit) %>% 
    filter(period >= 2010) %>% 
    group_by(scenario, variable) %>% 
    mutate(dt=0.5*(lead(period, default=2100) - period) + 0.5*(period - lag(period, default=2010))) %>% 
    mutate(value=value*dt) %>% 
    summarise(value=sum(value)/1000) %>% 
    ungroup()  %>% 
    mutate(variable=paste(factor(variable, levels=c("Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Direct Air Capture"), labels=c("BECCS", "DAC"), ordered=TRUE))) %>% 
    mutate(scenario=paste(factor(scenario, levels=c("DAC15_50", "DAC2_66", "S2_66"), labels=c("1.5°C-BECCS+DAC", "2°C-BECCS+DAC", "2°C-BECCS")))) %>% 
    mutate(study = "Marcucci et al (2017)") %>% 
    select(study,scenario,variable,value),
  data_holz2018 %>% 
    filter(region == "World") %>% 
    filter(variable %in% c("Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Direct Air Capture", "Carbon Sequestration|Land Use|Afforestation", "Carbon Sequestration|Land Use|Soil Carbon Management", "Carbon Sequestration|Land Use|Biochar", "Carbon Sequestration|Enhanced Weathering")) %>% 
    select(-model,-unit) %>% 
    filter(period >= 2010) %>% 
    group_by(scenario, variable) %>% 
    mutate(dt=0.5*(lead(period, default=2100) - period) + 0.5*(period - lag(period, default=2010))) %>% 
    mutate(value=value*dt) %>% 
    summarise(value=sum(value)/1000) %>% 
    ungroup() %>% 
    filter(scenario != "Reference") %>% 
    filter(scenario %in% c("Ratchet-1.5-allCDR", "Ratchet-1.5-limCDR")) %>% 
    mutate(variable=factor(variable, 
                           levels=c("Carbon Sequestration|Land Use|Afforestation", "Carbon Sequestration|Land Use|Soil Carbon Management", "Carbon Sequestration|Land Use|Biochar", "Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Direct Air Capture", "Carbon Sequestration|Enhanced Weathering" ), 
                           labels=c("AR", "SCS", "Biochar", "BECCS", "DAC", "EW"), ordered=TRUE)) %>% 
    mutate(scenario=factor(scenario, levels=c("Ratchet-1.5-allCDR", "Ratchet-1.5-limCDR"), #, "Ratchet-1.5-limCDR-noOS", "Ratchet-1.5-noCDR-OS", "Ratchet-1.5-noCDR-noOS"
                           labels=c("1.5°C-All", "1.5°C-AR")))  %>%  #, "1.5°C-AF(noOS)", "noCDR-OS", "noCDR-noOS")))
    mutate(study = "Holz et al (2018)") %>% 
    select(study,scenario,variable,value)
    
)  %>% 
  mutate(variable=paste(variable)) %>% 
  mutate(variable=factor(variable, levels=rev(c("AR", "SCS", "Biochar", "BECCS", "DAC", "EW")), ordered=TRUE)) %>% 
  mutate(tempgoal = ifelse(grepl("1.5°C", scenario), "1.5°C", "2°C"))

ggplot(data_plot %>% 
         mutate(study.scenario=paste0(study, " ", scenario))) +
  geom_bar(aes(x=study.scenario, y=value, fill=variable), stat="identity") +
  #coord_flip()+
  facet_wrap(~tempgoal, ncol=2) +
  theme_bw() +
  scale_fill_manual(values = c("AR"=RColorBrewer::brewer.pal(9, "Set1")[3], "BECCS"=RColorBrewer::brewer.pal(9, "Set1")[2], "Biochar"=RColorBrewer::brewer.pal(9, "Set1")[1], "DAC"=RColorBrewer::brewer.pal(9, "Set1")[5], "EW"=RColorBrewer::brewer.pal(9, "Set1")[4], "SCS"=RColorBrewer::brewer.pal(9, "Set1")[7])) +
  xlab("") + ylab("Cumulative negative CO2 emissions [GtCO2]") +
  guides(fill="none")
