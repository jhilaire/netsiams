plot_variable <- function(data, histdata, factor, varname)  {
  
  p <- ggplot() +
    #-- Historical data ---------
    #--- CDIAC
    geom_line(aes(x=period, y=value*1e-6), data=dat_cdiac %>%
                filter(variable == "Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)") %>% 
                group_by(period, variable) %>% 
                summarise(value=sum(value, na.rm=TRUE)) %>% 
                ungroup(),
              size=1) +
    #--- IEA
    #   geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_iea %>%
    #               filter(variable == "Total CO2 emissions", region == "World"),
    #             size=1) +
    #--- SSP historical data
    #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "IEA"), size=1) +
    #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "RCP"), size=1) +
    #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "MAGICC6"), size=1) +
    #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "EDGAR"), size=1) +
    geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "CDIAC"), size=1) +
    #-- Scenario data ---------
    #--- IS92 scenarios
    geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
                data=dat_is92 %>% 
                  filter(region == "World", variable == "Emissions from Combustion|CO2") %>% 
                  select(-unit, -region, -variable) %>% 
                  group_by(period) %>% 
                  summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(project="IS92"),
                fill="#ffaaaa66") +
#     geom_line(aes(x=period, y=value), color="red",
#               data=dat_is92 %>% 
#                 filter(scenario == "IS92-B", region == "World", variable == "Emissions from Combustion|CO2") %>% 
#                 mutate(project="IS92")) +
    #   geom_line(aes(x=period, y=value, color=scenario), 
    #             data=dat_is92 %>% filter(region == "World", variable == "Emissions from Combustion|CO2")) +
    #--- SRES scenarios
    geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
                data=dat_sres %>% 
                  mutate(value = value + p_sresCorrector) %>% 
                  filter(region == "World", variable == "Anthropogenic Emissions (standardized)|FossilFuelCO2") %>% 
                  select(-unit, -region, -variable) %>% 
                  group_by(period) %>% 
                  summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(project="SRES"),
                fill="#aaaaff66") +
    #   geom_line(aes(x=period, y=value, color=scenario, linetype=model), 
    #             data=dat_sres %>% filter(region == "World", variable == "Anthropogenic Emissions (standardized)|FossilFuelCO2")) +
    #--- RCP scenarios
    geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
                data=dat_rcp %>% 
                  mutate(value = value + p_rcpCorrector) %>% 
                  filter(region == "World", variable == "CO2 emissions - Fossil fuels and Industry") %>% 
                  select(-unit, -region, -variable) %>% 
                  group_by(period) %>% 
                  summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(project="RCP"),
                fill="#aaffaa99") +
    #   geom_line(aes(x=period, y=value, color=scenario, linetype=model), 
    #             data=dat_rcp %>% filter(region == "World", variable == "CO2 emissions - Fossil fuels and Industry")) +
    #--- AR5 scenarios
    geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
                data=dat_ar5_corr %>% 
                  mutate(value = value/1000*12/44) %>% 
                  filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
                  select(-unit, -region, -variable) %>% 
                  group_by(period) %>% 
                  summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(project="AR5"),
                fill="#aaaaaa33") +
    geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
                data=dat_ar5_corr %>% 
                  mutate(value = value/1000*12/44) %>% 
                  filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
                  select(-unit, -region, -variable) %>% 
                  group_by(period) %>% 
                  summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(project="AR5"),
                fill="#aaaaaa66") +
    geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
                data=dat_ar5_corr %>% 
                  mutate(value = value/1000*12/44) %>% 
                  filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
                  select(-unit, -region, -variable) %>% 
                  group_by(period) %>% 
                  summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(project="AR5"),
                fill="#aaaaaa99") +
    #   geom_line(aes(x=period, y=value, color=scenario, linetype=model), 
    #             data=dat_ar5 %>% 
    #                   mutate(period = as.numeric(format(period, "%Y"))) %>%  
    #                   mutate(value = value/1000*12/44) %>% 
    #                   filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry")) +
    #--- SSP scenarios
    geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
                data=dat_SSP  %>% 
                  mutate(value = value/1000*12/44) %>% 
                  filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
                  select(-unit, -region, -variable) %>% 
                  group_by(period) %>% 
                  summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(project="SSP"),
                fill="#aaaaaa33") +
    geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
                data=dat_SSP  %>% 
                  mutate(value = value/1000*12/44) %>% 
                  filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
                  select(-unit, -region, -variable) %>% 
                  group_by(period) %>% 
                  summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(project="SSP"),
                fill="#aaaaaa66") +
    geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
                data=dat_SSP  %>% 
                  mutate(value = value/1000*12/44) %>% 
                  filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
                  select(-unit, -region, -variable) %>% 
                  group_by(period) %>% 
                  summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(project="SSP"),
                fill="#aaaaaa99") +
    #   geom_line(aes(x=period, y=value, color=scenario, linetype=model), 
    #             data=dat_SSP %>% 
    #                   mutate(period = as.numeric(format(period, "%Y"))) %>%  
    #                   mutate(value = value/1000*12/44) %>% 
    #                   filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry")) +
    facet_wrap(~project, ncol=2) +
    theme_bw() +
    xlab("") +
    ylab("CO2 emissions [Gt C]") +
    xlim(c(1990,2030)) +
    ylim(c(0,20))
  
  print(p)
}