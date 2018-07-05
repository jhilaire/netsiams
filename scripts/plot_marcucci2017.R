data_marcucci2017 <- readxl::read_xlsx("data/dataDACpaper_jerome.xlsx")

names(data_marcucci2017) <- c("model","scenario","region","variable","unit","2000","2005","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100")


data_marcucci2017 <- data_marcucci2017 %>% 
  gather(period, value, -model, -scenario, -region, -variable, -unit) %>% 
  mutate(period=as.numeric(period)) %>% 
  mutate(value=as.numeric(value)) %>% 
  mutate(technology=ifelse(scenario == "S2_66", "BECCS", "BECCS+DAC"))

save(data_marcucci2017, file="data/data_marcucci2017.RData")

ggplot(data_marcucci2017 %>% 
         filter(variable %in% c("Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|CCS|Fossil", "Carbon Sequestration|Direct Air Capture"))) +
  geom_line(aes(period,value,color=variable, lty=scenario))

data_plot <- data_marcucci2017 %>% 
  filter(variable %in% c("Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Direct Air Capture")) %>% 
  select(-model,-unit) %>% 
  filter(period >= 2010) %>% 
  group_by(scenario, variable) %>% 
  mutate(dt=0.5*(lead(period, default=2100) - period) + 0.5*(period - lag(period, default=2010))) %>% 
  mutate(value=value*dt) %>% 
  summarise(value=sum(value)/1000) %>% 
  ungroup()

ggplot(data_plot %>% 
         filter(scenario != "BAU") %>% 
         mutate(variable=factor(variable, levels=rev(c("Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Direct Air Capture")), labels=rev(c("BECCS", "DAC")), ordered=TRUE)) %>% 
         mutate(scenario=factor(scenario, levels=c("DAC15_50", "DAC2_66", "S2_66"), labels=c("1.5°C-BECCS+DAC", "2°C-BECCS+DAC", "2°C-BECCS")))) +
  geom_bar(aes(x=scenario, y=value, fill=variable), stat="identity") +
  coord_flip()+
  theme_bw() +
  scale_fill_manual(values = c("BECCS"=RColorBrewer::brewer.pal(9, "Set1")[2], "DAC"=RColorBrewer::brewer.pal(9, "Set1")[5])) +
  xlab("") + ylab("Cumulative negative CO2 emissions [GtCO2]") +
  guides(fill="none")