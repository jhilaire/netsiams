data_holz2018_1 <- readxl::read_xlsx("data/IPCC_SR1p5C_template_CI C-ROADS-5.005 041518 FULL.xlsx", sheet = "data")
data_holz2018_2 <- readxl::read_xlsx("data/IPCC_SR1p5C_template_CI C-ROADS-5.005 041518 FULL.xlsx", sheet = "data2")
data_holz2018_3 <- readxl::read_xlsx("data/IPCC_SR1p5C_template_CI C-ROADS-5.005 041518 FULL.xlsx", sheet = "data3")
data_holz2018_4 <- readxl::read_xlsx("data/IPCC_SR1p5C_template_CI C-ROADS-5.005 041518 FULL.xlsx", sheet = "data4")
data_holz2018_5 <- readxl::read_xlsx("data/IPCC_SR1p5C_template_CI C-ROADS-5.005 041518 FULL.xlsx", sheet = "data5")
data_holz2018_6 <- readxl::read_xlsx("data/IPCC_SR1p5C_template_CI C-ROADS-5.005 041518 FULL.xlsx", sheet = "data6")

data_holz2018 <- bind_rows(
  data_holz2018_1,
  data_holz2018_2,
  data_holz2018_3,
  data_holz2018_4,
  data_holz2018_5,
  data_holz2018_6) %>% 
  tidyr::gather(period, value, - Model,-Scenario,-Region,-Variable,-Unit) %>% 
  dplyr::mutate(period = as.numeric(period))

names(data_holz2018) <- c("model", "scenario", "region", "variable", "unit", "period", "value")

save(data_holz2018, file="data/data_holz2018.RData")

ggplot(data_holz2018 %>% 
         filter(region == "World") %>% 
         filter(variable %in% c("Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Direct Air Capture", "Carbon Sequestration|Land Use|Afforestation", "Carbon Sequestration|Land Use|Soil Carbon Management", "Carbon Sequestration|Land Use|Biochar", "Carbon Sequestration|Enhanced Weathering"))) +
  geom_line(aes(period,value,color=variable, lty=scenario))

data_plot <- data_holz2018 %>% 
  filter(region == "World") %>% 
  filter(variable %in% c("Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Direct Air Capture", "Carbon Sequestration|Land Use|Afforestation", "Carbon Sequestration|Land Use|Soil Carbon Management", "Carbon Sequestration|Land Use|Biochar", "Carbon Sequestration|Enhanced Weathering")) %>% 
  select(-model,-unit) %>% 
  filter(period >= 2010) %>% 
  group_by(scenario, variable) %>% 
  mutate(dt=0.5*(lead(period, default=2100) - period) + 0.5*(period - lag(period, default=2010))) %>% 
  mutate(value=value*dt) %>% 
  summarise(value=sum(value)/1000) %>% 
  ungroup()

ggplot(data_plot %>% 
         filter(scenario != "Reference") %>% 
         mutate(variable=factor(variable, 
                                levels=rev(c("Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Direct Air Capture", "Carbon Sequestration|Land Use|Afforestation", "Carbon Sequestration|Land Use|Soil Carbon Management", "Carbon Sequestration|Land Use|Biochar", "Carbon Sequestration|Enhanced Weathering" )), 
                                labels=rev(c("BECCS", "DAC", "AF", "SCS", "Biochar", "EW")), ordered=TRUE)) %>% 
         mutate(scenario=factor(scenario, levels=c("Ratchet-1.5-allCDR", "Ratchet-1.5-limCDR", "Ratchet-1.5-limCDR-noOS", "Ratchet-1.5-noCDR-OS", "Ratchet-1.5-noCDR-noOS"), 
                                labels=c("allCDR", "limCDR-OS", "limCDR-noOS", "noCDR-OS", "noCDR-noOS")))) +
  geom_bar(aes(x=scenario, y=value, fill=variable), stat="identity") +
  coord_flip()+
  theme_bw() +
  scale_fill_manual(values = c("AF"=RColorBrewer::brewer.pal(9, "Set1")[3], "BECCS"=RColorBrewer::brewer.pal(9, "Set1")[2], "Biochar"=RColorBrewer::brewer.pal(9, "Set1")[1], "DAC"=RColorBrewer::brewer.pal(9, "Set1")[5], "EW"=RColorBrewer::brewer.pal(9, "Set1")[4], "SCS"=RColorBrewer::brewer.pal(9, "Set1")[7])) +
  xlab("") + ylab("Cumulative negative CO2 emissions [GtCO2]") +
  guides(fill="none")