load("../../bitbucket/beccs/data/dataplotAll.RData")
load("../../bitbucket/beccs/data/one_five_scenarios.Rdata")

source("../../bitbucket/beccs/functions/useful_functions.R")

library(dplyr)
library(tidyr)
library(ggplot2)


v_dataplot <- v_data_tempTargets_world_plot %>% 
  filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                         "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
  mutate(value = value/1000) %>% 
  group_by(model,scenario,region,tempcat) %>% 
  mutate(keep=ifelse(any(is.na(value)), FALSE, TRUE)) %>% 
  ungroup() %>% 
  spread(variable, value) %>% 
  mutate(keep = ifelse(is.na(`Emissions|CO2`) | is.na(`Emissions|CO2|Fossil Fuels and Industry`) | 
                         is.na(`Emissions|CO2|Land Use`) | is.na(`Emissions|CO2|Carbon Capture and Storage|Biomass`), FALSE, TRUE)) %>% 
  mutate(check = `Emissions|CO2` - (`Emissions|CO2|Fossil Fuels and Industry`+`Emissions|CO2|Land Use`)) %>% 
  mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross` = `Emissions|CO2|Fossil Fuels and Industry` + `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
  mutate(`Emissions|CO2|Carbon Capture and Storage|Biomass` = -`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
  mutate(period = as.numeric(period)) %>% 
  mutate(period > 2005) %>% 
  group_by(model, scenario, region, tempcat) %>% 
  arrange(period) %>% 
  mutate(dt  = (period   - lag(period, default = 2010))) %>% 
  mutate(tmp = dt*(`Emissions|CO2` + lag(`Emissions|CO2`, default = 0))/2) %>% 
  mutate(`Emissions|CO2|Cumulative` = cumsum(tmp)) %>% 
  mutate(`Emissions|CO2|Cumulative|Positive` = cumsum(ifelse(tmp >= 0, tmp, 0))) %>% 
  mutate(`Emissions|CO2|Cumulative|Negative` = cumsum(ifelse(tmp <  0, tmp, 0))) %>% 
  mutate(tmp = dt*(`Emissions|CO2|Carbon Capture and Storage|Biomass` + lag(`Emissions|CO2|Carbon Capture and Storage|Biomass`, default = 0))/2) %>% 
  mutate(`Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative` = cumsum(tmp)) %>% 
  mutate(tmp = dt*(`Emissions|CO2|Fossil Fuels and Industry` + lag(`Emissions|CO2|Fossil Fuels and Industry`, default = 0))/2) %>% 
  mutate(`Emissions|CO2|Fossil Fuels and Industry|Cumulative` = cumsum(tmp)) %>% 
  mutate(tmp = dt*(`Emissions|CO2|Fossil Fuels and Industry|Gross` + lag(`Emissions|CO2|Fossil Fuels and Industry|Gross`, default = 0))/2) %>% 
  mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative` = cumsum(tmp)) %>% 
  mutate(tmp = dt*(`Emissions|CO2|Land Use` + lag(`Emissions|CO2|Land Use`, default = 0))/2) %>% 
  mutate(`Emissions|CO2|Land Use|Cumulative` = cumsum(tmp)) %>% 
  select(-dt, -tmp) %>% 
  select(model,scenario,region,period,tempcat, keep, check,
         `Emissions|CO2`, `Emissions|CO2|Fossil Fuels and Industry`, `Emissions|CO2|Fossil Fuels and Industry|Gross`, 
         `Emissions|CO2|Carbon Capture and Storage|Biomass`, `Emissions|CO2|Land Use`, 
         `Emissions|CO2|Cumulative`, `Emissions|CO2|Cumulative|Positive`, `Emissions|CO2|Cumulative|Negative`, `Emissions|CO2|Fossil Fuels and Industry|Cumulative`, `Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative`, 
         `Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative`, `Emissions|CO2|Land Use|Cumulative`) %>% 
  ungroup() %>% 
  filter(keep == TRUE) %>% 
  select(-keep, -check) %>% 
  gather(variable, value, -model, -scenario, -region, -period, -tempcat) %>% 
  filter(period %in% c(2050, 2100)) %>% 
  spread(period, value) %>% 
  mutate(`2051-2100`=`2100`-`2050`) %>% 
  rename(`2011-2050`=`2050`) %>% 
  rename(`2011-2100`=`2100`) %>% 
  gather(period, value, -model,-scenario,-region,-tempcat,-variable) %>% 
  filter(tempcat != "Other scenario") %>% 
  mutate(tempcat = factor(tempcat, levels=c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario", "Likely 3.0°C scenario"), ordered=TRUE)) %>% 
  spread(variable, value) %>% 
  mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative` = `Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative` + `Emissions|CO2|Land Use|Cumulative`) %>%
  gather(variable, value, -model, -scenario, -region, -period, -tempcat)

tmp <- v_dataplot %>% 
  as.data.frame() %>% 
  filter(variable == "Emissions|CO2", period == "2011-2100") %>% 
  group_by(tempcat,variable) %>% 
  summarise(
    count_model    = length(unique(model)),
    count_scenario = length(unique(scenario)),
    count = n()
  ) %>% 
  ungroup()

v_dataplot_stat <- v_dataplot  %>%
  group_by(tempcat,period,variable) %>% 
  summarise(
    min  = min(value, na.rm=TRUE), 
    q05  = quantile(value, 0.05, na.rm=TRUE), 
    q10  = quantile(value, 0.10, na.rm=TRUE), 
    q15  = quantile(value, 0.15, na.rm=TRUE), 
    q25  = quantile(value, 0.25, na.rm=TRUE), 
    med  = median(value, na.rm=TRUE), 
    mean = mean(value, na.rm=TRUE), 
    q75  = quantile(value, 0.75, na.rm=TRUE), 
    q85  = quantile(value, 0.85, na.rm=TRUE), 
    q90  = quantile(value, 0.90, na.rm=TRUE), 
    q95  = quantile(value, 0.95, na.rm=TRUE), 
    max  = max(value, na.rm=TRUE),
    count=n()) %>% 
  ungroup() %>% 
  mutate(tempcat = factor(tempcat, levels=c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario", "Likely 3.0°C scenario"), ordered=TRUE))


ratios <- v_dataplot %>% 
  filter(period == "2011-2100",
         variable %in% c("Emissions|CO2|Cumulative|Positive", "Emissions|CO2|Cumulative|Negative", "Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative", 
                         "Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative")) %>% 
  select(-region, -period) %>% 
  spread(variable, value) %>% 
  rename(gross_pos = `Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative`) %>% 
  rename(gross_neg = `Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative`) %>% 
  rename(net_pos = `Emissions|CO2|Cumulative|Positive`) %>% 
  rename(net_neg = `Emissions|CO2|Cumulative|Negative`) %>% 
  mutate(r_nP_gP = abs(net_pos/gross_pos)) %>% 
  mutate(r_nN_gN = abs(net_neg/gross_neg)) %>% 
  mutate(r_nP_nN = abs(net_pos/net_neg)) %>% 
  mutate(r_nP_gN = abs(net_pos/gross_neg)) %>%
  mutate(r_nN_gP = abs(net_neg/gross_pos)) %>%
  mutate(r_gP_gN = abs(gross_pos/gross_neg)) %>% 
  select(-net_neg,-net_pos,-gross_neg,-gross_pos) %>% 
  gather(variable, value, -model, -scenario, -tempcat) %>% 
  mutate(value=ifelse(!is.finite(value), NA, value))

p <- ggplot(ratios %>% 
              filter(model != "WITCH_AMPERE" & scenario != "AMPERE2-550-NoCCS-OPT")) +
  geom_histogram(aes(x=value, fill=model)) +
  facet_grid(tempcat~variable, scales = "free") +
  theme_bw() +
  theme(legend.position="bottom")
print(p)
ggsave("plots/ratios_frequency_distribution_byMod.pdf", width = 10, heigh = 9)

p <- ggplot(ratios %>% 
              filter(model != "WITCH_AMPERE" & scenario != "AMPERE2-550-NoCCS-OPT") %>% 
              filter(grepl("AMPERE2|REMIND_Scen", scenario)) %>% 
              left_join(one_five_scenarios %>% 
                          select(scenario, `policy timing`, technology) %>% 
                          mutate(scenario = paste0("REMIND_", scenario)),
                        by=c("scenario")) %>% 
              mutate(tech = "Other") %>% 
              mutate(tech = ifelse(grepl("FullTech", scenario), "FT", 
                                   ifelse(grepl("LimBio", scenario),   "limBio", 
                                          ifelse(grepl("LowEI", scenario),    "lowEI",
                                                 ifelse(grepl("Conv", scenario), "Conv", "Other"))))) %>% 
              mutate(tech = ifelse(grepl("Default", technology), "FT", 
                                   ifelse(grepl("LimBio", scenario),   "limBio", 
                                          ifelse(grepl("LowEI", scenario),    "lowEI", "Other"))))) +
  geom_histogram(aes(x=value, fill=tech)) +
  facet_grid(tempcat~variable, scales = "free") +
  theme_bw() +
  theme(legend.position="bottom")
print(p)
ggsave("plots/ratios_frequency_distribution_byTechOpts.pdf", width = 10, heigh = 9)

p <- ggplot(ratios %>% 
              filter(model != "WITCH_AMPERE" & scenario != "AMPERE2-550-NoCCS-OPT") %>% 
              filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario"))) +
  geom_histogram(aes(x=value)) +
  facet_grid(tempcat~variable, scales = "free") +
  theme_bw() +
  theme(legend.position="bottom")
print(p)
ggsave("plots/ratios_frequency_distribution_15_L2.pdf", width = 10, heigh = 9)

p <- ggplot(ratios %>% 
              filter(model != "WITCH_AMPERE" & scenario != "AMPERE2-550-NoCCS-OPT")) +
  geom_boxplot(aes(x=1, y=value)) +
  coord_flip() +
  facet_grid(tempcat~variable, scales = "free") +
  theme_bw() +
  theme(legend.position="bottom")
print(p)
ggsave("plots/ratios_boxplot.pdf", width = 10, heigh = 9)

p <- ggplot(ratios %>% 
              filter(model != "WITCH_AMPERE" & scenario != "AMPERE2-550-NoCCS-OPT") %>% 
              filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario")))+
  geom_boxplot(aes(x=1, y=value)) +
  coord_flip() +
  facet_grid(tempcat~variable, scales = "free") +
  theme_bw() +
  theme(legend.position="bottom")
print(p)
ggsave("plots/ratios_boxplot_15_L2.pdf", width = 10, heigh = 9)
