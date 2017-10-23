#== GET DATA ========
load("../../bitbucket/beccs/data/dataplotAll.RData")

#== PROCESS DATA ========
tmp <- v_data_timeTechTemp_world_plot %>% 
  filter(grepl("Emissions.CO2.*Energy Supply|Emissions.CO2.*Energy Demand.*|Emissions.CO2.Land Use|Emissions.CH4|Emissions.N2O|F-Gases", variable), 
         period %in% c(2030,2050,2100)) %>% 
  mutate(model.scenario = paste0(model,scenario)) %>% 
  mutate(period=paste(period)) %>% 
  mutate(value = value*1e-3) %>% 
  mutate(value = ifelse(grepl("CH4", variable), 25*value, value)) %>% 
  mutate(value = ifelse(grepl("N2O", variable), 298*value*1e-3, value)) %>% 
  mutate(variable = ifelse(grepl("CH4", variable), "CH4", 
                           ifelse(grepl("N2O", variable), "N2O", 
                                  ifelse(grepl("F-Gases", variable), "F-Gases", 
                                         ifelse(grepl("Supply", variable), "CO2 - Power", 
                                                ifelse(grepl("Energy Demand.Industry", variable), "CO2 - Industry", 
                                                       ifelse(grepl("Residential", variable), "CO2 - Buildings", 
                                                              ifelse(grepl("Transport", variable), "CO2 - Transportation", 
                                                                     ifelse(grepl("Land", variable), "CO2 - Land-Use", "CO2 - Agg. demand"))))))))) %>% 
  mutate(variable = factor(variable, levels=c("CO2 - Power", "CO2 - Agg. demand", "CO2 - Industry", 
                                              "CO2 - Buildings","CO2 - Transportation", "CO2 - Land-Use",
                                              "CH4", "N2O", "F-Gases"), ordered=TRUE))

#== PLOT DATA ========
p_emiAll_bySec_byTemp <- ggplot(tmp) + 
  geom_boxplot(aes(x=period, y=value, fill=tempcat)) + 
  #geom_jitter(aes(x=period, y=value, colour=tempcat, group=model.scenario), fill="#333333", pch=21) +
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax), data=data.frame(variable=c("CO2 - Power", "CO2 - Agg. demand", "CO2 - Industry", 
                                                                                     "CO2 - Buildings","CO2 - Transportation", "CO2 - Land-Use",
                                                                                     "CH4", "N2O", "F-Gases"), xmin=0.5, xmax=3.5, ymin=0, ymax=0), lty=2, color="red") +
  facet_wrap(~ variable, ncol=5) + 
  theme_bw() + 
  theme(legend.position="bottom") +
  ylim(-30,40)
print(p_emiAll_bySec_byTemp)


ggplot(tmp %>% 
         filter(
           tempcat != "Other scenario",
           allcat %in% c("Default", "Limited bioenergy", "No CCS/BECCS", "Delayed action until 2030"))) + 
  geom_boxplot(aes(x=period, y=value, fill=tempcat)) + 
  #geom_jitter(aes(x=period, y=value, colour=tempcat, group=model.scenario), fill="#333333", pch=21) +
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax), data=data.frame(variable=c("CO2 - Power", "CO2 - Agg. demand", "CO2 - Industry", 
                                                                                     "CO2 - Buildings","CO2 - Transportation", "CO2 - Land-Use",
                                                                                     "CH4", "N2O", "F-Gases"), xmin=0.5, xmax=3.5, ymin=0, ymax=0), lty=2, color="red") +
  facet_grid(variable~allcat, scales = "free_y") + 
  theme_bw() + 
  theme(legend.position="bottom")


ggplot(tmp %>% 
         filter(
           tempcat != "Other scenario",
           allcat %in% c("Default", "Limited bioenergy", "No CCS/BECCS", "Delayed action until 2030"))) + 
  geom_boxplot(aes(x=period, y=value, fill=tempcat)) + 
  #geom_jitter(aes(x=period, y=value, colour=tempcat, group=model.scenario), fill="#333333", pch=21) +
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax), data=data.frame(variable=c("CO2 - Power", "CO2 - Agg. demand", "CO2 - Industry", 
                                                                                     "CO2 - Buildings","CO2 - Transportation", "CO2 - Land-Use",
                                                                                     "CH4", "N2O", "F-Gases"), xmin=0.5, xmax=3.5, ymin=0, ymax=0), lty=2, color="red") +
  facet_grid(allcat~variable) + 
  theme_bw() + 
  theme(legend.position="bottom") +
  ylim(-30,40)
