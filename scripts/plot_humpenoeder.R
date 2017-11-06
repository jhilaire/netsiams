data_hump <- read.csv2("data/Humpenoeder_et_al_ERL_2014.mif", stringsAsFactors = FALSE) %>% 
  select(-X) %>% 
  gather(period, value, -Model, -Scenario, -Region, -Variable, -Unit) %>% 
  mutate(period=as.numeric(substr(period,2,5))) %>% 
  mutate(value=as.numeric(value))

names(data_hump) <- c("model", "scenario", "region", "variable", "unit",
                      "period", "value")


data_hump %>% 
  filter(region == "World", 
         scenario %in% c("AFF", "BECCS", "AFF+BECCS"),
         variable %in% c("CO2 Emissions BECCS", "CO2 Emissions Land incl Defor+AFF"), 
         period == 2095)
  
data_plot <- data_hump %>% 
  filter(region == "World", 
         scenario %in% c("AFF", "BECCS", "AFF+BECCS"),
         variable %in% c("CO2 Emissions BECCS", "CO2 Emissions Land incl Defor+AFF"), 
         period == 2095)# %>% 
  #spread(variable, value) %>% 
  #mutate(total=`CO2 Emissions BECCS`+`CO2 Emissions Land incl Defor+AFF`)

ggplot(data_plot %>% 
         mutate(value=ifelse(value > 0, 0, value)) %>% 
         mutate(value=-value) %>% 
         mutate(variable=factor(variable, levels=rev(c("CO2 Emissions BECCS", "CO2 Emissions Land incl Defor+AFF")), labels=rev(c("BECCS", "AR")), ordered=TRUE)) %>% 
         mutate(scenario=factor(scenario, levels=rev(c("AFF", "BECCS", "AFF+BECCS")), labels=rev(c("2°C-AR", "2°C-BECCS", "2°C-AR+BECCS")), ordered=TRUE))) +
  geom_bar(aes(x=scenario, y=value, fill=variable), stat="identity") +
  coord_flip()+
  theme_bw() +
  scale_fill_manual(values = c("BECCS"=RColorBrewer::brewer.pal(9, "Set1")[2], "AR"=RColorBrewer::brewer.pal(9, "Set1")[3])) +
  xlab("") + ylab("Cumulative negative CO2 emissions [GtCO2]") +
  guides(fill="none")
