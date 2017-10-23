u_path_1p5   <- "../../bitbucket/beccs/data/REMIND_generics_20150512.csv"
u_path_1p5ScenarioInfo <- "../../bitbucket/beccs/data/one_five_scenarios.Rdata"

compute_grossCO2emiFFI <- function(i_data) {
  out <- rbind(
    i_data,
    i_data %>% 
      filter(variable %in% c("Emissions|CO2|Fossil Fuels and Industry", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      spread(variable, value) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross` = `Emissions|CO2|Fossil Fuels and Industry` + `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>%
      dplyr::select(-`Emissions|CO2|Fossil Fuels and Industry`, -`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      gather(variable, value, -model, -scenario, -region, -period, -tempcat)
  )
  
  return(out)
}
compute_grossCO2emiFFI_wCP <- function(i_data) {
  out <- rbind(
    i_data,
    i_data %>% 
      filter(variable %in% c("Emissions|CO2|Fossil Fuels and Industry", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      spread(variable, value) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross` = `Emissions|CO2|Fossil Fuels and Industry` + `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>%
      dplyr::select(-`Emissions|CO2|Fossil Fuels and Industry`, -`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      gather(variable, value, -model, -scenario, -region, -period, -tempcat, -cp2030)
  )
  
  return(out)
}
compute_grossCO2emiFFI_TTT <- function(i_data) {
  out <- rbind(
    i_data,
    i_data %>% 
      filter(variable %in% c("Emissions|CO2|Fossil Fuels and Industry", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      spread(variable, value) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross` = `Emissions|CO2|Fossil Fuels and Industry` + `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>%
      dplyr::select(-`Emissions|CO2|Fossil Fuels and Industry`, -`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      gather(variable, value, -model, -scenario, -region, -period, -tempcat, -timingcat, -techcat, -allcat)
  )
  
  return(out)
}

library(plotly)

#== GET DATA ========
load("../../bitbucket/beccs/data/dataplotAll.RData")

v_RData_1p5 <- paste0(substr(u_path_1p5, 1, nchar(u_path_1p5)-3), "RData")
if (!file.exists(v_RData_1p5) && u_readOriginalData) {
  data_1p5 <- read.csv2(u_path_1p5) %>% 
    select(Model, Scenario, Region, Variable, Unit, `X2005`, `X2010`, `X2015`, `X2020`, `X2025`, `X2030`, `X2035`, 
           `X2040`, `X2045`, `X2050`, `X2055`, `X2060`, `X2070`, `X2080`, `X2090`, `X2100`)
  names(data_1p5) <- c("model", "scenario", "region", "variable", "unit", paste(c(seq(2005,2055,5), seq(2060,2100,10))))
  data_1p5 <- data_1p5 %>% 
    gather(period, value, -model, -scenario, -region, -variable, -unit)
  
  save(data_1p5, file = v_RData_1p5)
  
} else {
  load(v_RData_1p5)
}
load(u_path_1p5ScenarioInfo)

# MESSAGE and REMIND data from Joeri
data_1p5v2scenclass <- readxl::read_xls("../../bitbucket/beccs/data/1p5CSCENs_categorization_forJanMINX.xls")

#== PROCESS data =====
data_1p5 <- data_1p5 %>% 
  filter(
    variable %in% c(
      "Policy Cost|GDP Loss",
      "Policy Cost|Consumption Loss",
      "Total Energy costs|Elec|Biomass|w/ CCS",
      "Operational costs|Elec|Bio|w/ CCS",
      "Price|Carbon",
      "Emi|CO2",
      "Emi|CH4",
      "Emi|N2O",
      "Emi|F-Gases",
      "Emi|CO2|Fossil fuels and Industry",
      "Emi|CO2|Fossil Fuels and Industry|Coal",
      "Emi|CO2|Fossil Fuels and Industry|Gas",
      "Emi|CO2|Fossil Fuels and Industry|Oil",
      "Emi|CO2|Electricity Production",
      "Emi|CO2|Fossil Fuels and Industry|Cement",
      "Emi|CO2|Fossil Fuels and Industry|Energy Supply",
      "Emi|CO2|Fossil Fuels and Industry|Demand",
      "Emi|CO2|Transportation",
      "Emi|CO2|Land-Use Change",
      "Emi|CO2|Carbon Capture and Storage",
      "Emi|CO2|Carbon Capture and Storage|Biomass",
      "PE|Biomass",
      "PE|Biomass|w/ CCS",
      "PE|Biomass|w/o CCS",
      "PE",
      "SE",
      "SE|Electricity|Biomass|w/o CCS",
      "SE|Electricity|Biomass|w/ CCS",
      "SE|Liquids|Biomass|w/o CCS",
      "SE|Liquids|Biomass|w/ CCS",
      "SE|Hydrogen|Biomass|w/o CCS",
      "SE|Hydrogen|Biomass|w/ CCS",
      "Energy Investments|Elec|Biomass|w/ CCS",
      "Total Energy costs|Elec|Biomass|w/ CCS"
    )) %>% 
  mutate(variable = paste0(variable)) %>% 
  mutate(variable = 
           ifelse(variable == "Emi|CO2", "Emissions|CO2", 
                  ifelse(variable == "Emi|CO2|Fossil fuels and Industry", "Emissions|CO2|Fossil Fuels and Industry",
                         ifelse(variable == "Emi|CO2|Land-Use Change", "Emissions|CO2|Land Use", 
                                ifelse(variable == "Emi|CO2|Carbon Capture and Storage", "Emissions|CO2|Carbon Capture and Storage", 
                                       ifelse(variable == "Emi|CO2|Carbon Capture and Storage|Biomass", "Emissions|CO2|Carbon Capture and Storage|Biomass", 
                                              ifelse(variable == "PE|Biomass", "Primary Energy|Biomass",
                                                     ifelse(variable == "PE|Biomass|w/ CCS", "Primary Energy|Biomass|w/ CCS", 
                                                            ifelse(variable == "PE|Biomass|w/o CCS", "Primary Energy|Biomass|w/o CCS", 
                                                                   ifelse(variable == "PE", "Primary Energy", 
                                                                          ifelse(variable == "SE|Electricity|Biomass|w/ CCS", "Secondary Energy|Electricity|Biomass|w/ CCS", 
                                                                                 ifelse(variable == "SE|Electricity|Biomass|w/o CCS", "Secondary Energy|Electricity|Biomass|w/o CCS", 
                                                                                        ifelse(variable == "SE|Liquids|Biomass|w/ CCS", "Secondary Energy|Liquids|Biomass|w/ CCS", 
                                                                                               ifelse(variable == "SE|Liquids|Biomass|w/o CCS", "Secondary Energy|Liquids|Biomass|w/o CCS", 
                                                                                                      ifelse(variable == "SE|Hydrogen|Biomass|w/ CCS", "Secondary Energy|Hydrogen|Biomass|w/ CCS", 
                                                                                                             ifelse(variable == "SE|Hydrogen|Biomass|w/o CCS", "Secondary Energy|Hydrogen|Biomass|w/o CCS", 
                                                                                                                    ifelse(variable == "SE", "Secondary Energy", 
                                                                                                                           ifelse(variable == "Energy Investments|Elec|Biomass|w/ CCS", "Investment|Energy Supply|Electricity|Non-fossil|Biomass|w/ CCS",
                                                                                                                                  ifelse(variable == "Total Energy costs|Elec|Biomass|w/ CCS", "Total cost|Energy Supply|Electricity|Non-fossil|Biomass|w/ CCS", variable))))))))))))))))))) %>% 
  mutate(period = as.numeric(period)) %>% 
  mutate(value = as.numeric(value))

data_1p5 <- rbind(data_1p5,
                  data_1p5 %>% 
                   dplyr::filter(variable %in% c("Secondary Energy|Electricity|Biomass|w/ CCS", "Secondary Energy|Electricity|Biomass|w/o CCS")) %>%
                    dplyr::mutate(value = ifelse(is.na(value), 0, value)) %>% 
                    dplyr::select(-unit) %>% 
                    tidyr::spread(variable, value) %>% 
                    dplyr::mutate(`Secondary Energy|Electricity|Biomass` = `Secondary Energy|Electricity|Biomass|w/ CCS` + `Secondary Energy|Electricity|Biomass|w/o CCS`) %>% 
                    tidyr::gather(variable, value, -model, -scenario, -region, -period) %>%
                    dplyr::mutate(unit="EJ/yr") %>% 
                    dplyr::select(model, scenario, region, variable, unit, period, value) %>% 
                    dplyr::filter(variable == "Secondary Energy|Electricity|Biomass"))

data_1p5 <- left_join(
  data_1p5 %>% 
    filter(grepl("^FFrun", scenario)) %>% 
    mutate(scenario = gsub("FFrun", "Scen", scenario)),
  one_five_scenarios,
  by=c("scenario"))

#-- Add Joeri's temperature categories --
data_1p5 <- data_1p5 %>% 
  mutate(scenIIASA = gsub("Scen", "REMIND_Scen", scenario)) %>%
  left_join(data_1p5v2scenclass[-1,c(1,2)],
            by=c("scenIIASA"="IIASADB name")) %>% 
  mutate(tempcat = ifelse(X__1 == "A", "1.5°C scenario", 
                          ifelse(X__1 == "B", "Likely 2.0°C scenario", "Medium 2.0°C scenario"))) %>% 
  select(-`X__1`)


#-- Add other scenario categories --
data_1p5 <- data_1p5 %>% 
  mutate(tempcatREMIND = ifelse(temp_cat == "1.5C", "1.5°C scenario",
                                ifelse(temp_cat == "2C" & `2°c in 2100` >= 2/3, "Likely 2.0°C scenario",
                                       ifelse(temp_cat == "2C" & `2°c in 2100` < 2/3, "Medium 2.0°C scenario",
                                              ifelse(temp_cat == "2C" & tempcat == "Likely 2.0°C scenario", "Likely 2.0°C scenario",
                                                     ifelse(temp_cat == "greater_than_2" & `2100 temp(median)` <= 3.0, "Medium 3.0°C scenario", "Other scenario")))))) %>% 
  mutate(timingcat = "NA") %>% 
  mutate(timingcat = ifelse(timing == "Immediate", "immediate", timingcat)) %>%
  mutate(timingcat = ifelse(timing == "WeakPol",   "weakPol",   timingcat)) %>%
  mutate(timingcat = ifelse(timing == "Frag2015",  "delay2015", timingcat)) %>% 
  mutate(timingcat = ifelse(timing == "Frag2020",  "delay2020", timingcat)) %>% 
  mutate(timingcat = ifelse(timing == "Frag2030",  "delay2030", timingcat)) %>% 
  mutate(techcat = "other") %>% 
  mutate(techcat = ifelse(technology == "Default", "default", techcat)) %>%
  mutate(techcat = ifelse(technology == "NoCCS",   "noCCS",   techcat)) %>%
  mutate(techcat = ifelse(technology == "NoBECCS", "noBECCS", techcat)) %>%
  mutate(techcat = ifelse(technology == "LowEI",   "lowEI",   techcat)) %>%
  mutate(techcat = ifelse(technology == "LimBio",  "limBio",  techcat))

data_1p5 <- data_1p5 %>% 
  #filter(!is.na(category_temppol)) %>% 
  mutate(value = as.numeric(value))%>% 
  mutate(period = as.numeric(period)) 


#== PLOT DATA ========
#-- CO2 emissions --------
data_plot <- data_1p5 %>% 
  filter(
    region == "World",
    variable == "Emissions|CO2",
    filter == "feasible") %>% 
  mutate(value = value/1000)

svglite::svglite("plots/conceptual_plot.svg")
p = ggplot() +
  geom_line(aes(x=period, y=value),
            data=data_plot %>% filter(temp_cat == "Baseline", technology == "Default")) +
  geom_line(aes(x=period, y=value),
            data=data_plot %>% filter(temp_cat == "2C",   timing == "Immediate", tax_level == 30,  technology == "Default"), color="red") +
  geom_line(aes(x=period, y=value),
            data=data_plot %>% filter(temp_cat == "2C",   timing == "Immediate", tax_level == 100, technology == "NoCCS"),   color="blue") +
  geom_line(aes(x=period, y=value),
            data=data_plot %>% filter(temp_cat == "2C",   timing == "Frag2030",  tax_level == 50,  technology == "Default"), color="purple") +
  geom_line(aes(x=period, y=value),
            data=data_plot %>% filter(temp_cat == "1.5C", timing == "Immediate", tax_level == 100, technology == "Default"), color="green") +

  theme_bw()
print(p)
dev.off()

#-- Cumulative net emissions --------
data_plot <- data_1p5 %>% 
  filter(
    period >= 2010,
    region == "World",
    variable == "Emissions|CO2",
    filter == "feasible",
    tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  dplyr::select(scenario,period,value,tempcat,timingcat,tax_level,techcat) %>% 
  group_by(scenario,tempcat,timingcat,techcat,tax_level) %>% 
  arrange(period) %>% 
  mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
  mutate(demi = value*dt) %>% 
  summarise(value = sum(demi)/1000) %>% 
  ungroup()

p = ggplot(data_plot %>% 
             filter(
               timingcat %in% c("immediate", "delay2030"), 
               techcat %in% c("default", "limBio", "noCCS"))) +
  geom_density(aes(x=value, fill=tempcat), alpha=0.5) +
  facet_grid(timingcat~techcat) +
  theme_bw() + 
  theme(legend.position = "bottom")
print(p)


plot_scenario_var_boxplot <- function(i_data, i_variable, i_period=seq(2010,2100,10), i_factor=1, i_varname=NULL, i_ylim=NULL, i_cumulate=FALSE) {
  data_plot <- i_data %>% 
    filter(
      period %in% i_period,
      region == "World",
      variable == i_variable,
      tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
    dplyr::select(scenario,period,value,tempcat,timingcat,techcat)
  
  if (i_cumulate) {
    data_plot <- data_plot %>% 
      group_by(scenario,tempcat,timingcat,techcat) %>% 
      arrange(period) %>% 
      mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
      mutate(demi = value*dt) %>% 
      summarise(value = sum(demi)*i_factor) %>% 
      ungroup()
  }
  
  p = ggplot(data_plot %>% 
               filter(
                 timingcat %in% c("immediate", "delay2030"), 
                 techcat %in% c("default", "limBio", "noCCS"))) +
    geom_boxplot(aes(x=techcat, y=value, color=timingcat)) +
    facet_grid(~tempcat, scales="free_y") +
    theme_bw() + 
    theme(legend.position = "bottom") +
    xlab("")
  
  if (is.null(i_varname)) {
    p <- p + ylab(ifelse(i_cumulate, paste0("Cumulative ", i_variable), i_variable))  
  } else {
    p <- p + ylab(i_varname)  
  }
  
  if (!is.null(i_ylim)) p <- p + ylim(i_ylim)  
  
  print(p)
}




plot_scenario_var_scatter <- function(i_data, i_x, i_y, i_period=seq(2010,2100,10), i_factorX=1, i_factorY=1, i_varnameX=NULL, i_varnameY=NULL, i_xlim=NULL, i_ylim=NULL, i_cumX=FALSE, i_cumY=FALSE, i_negX=FALSE, i_negY=FALSE, i_posX=FALSE, i_posY=FALSE) {
  
  data_plot <- i_data %>% 
    filter(
      period %in% i_period,
      region == "World",
      variable %in% c(i_x, i_y),
      tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
    dplyr::select(scenario,variable,period,value,tempcat)
  
  if (i_factorX != 1) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_x, value*i_factorX, value))
  
  if (i_factorY != 1) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_y, value*i_factorY, value))
  
  if (i_posX) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_x & value >= 0, value, 0))
  if (i_negX) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_x & value <  0, value, 0))
  if (i_negY) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_y & value >= 0, value, 0))
  if (i_posY) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_y & value <  0, value, 0))
  
  if (i_cumX & i_cumY) {
    data_plot <- rbind(
      data_plot %>% 
        filter(variable == i_x) %>% 
        group_by(scenario,variable,tempcat) %>% 
        arrange(period) %>% 
        mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
        mutate(demi = value*dt) %>% 
        summarise(value = sum(demi)) %>% 
        ungroup(),
      data_plot %>% 
        filter(variable == i_y) %>% 
        group_by(scenario,variable,tempcat) %>% 
        arrange(period) %>% 
        mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
        mutate(demi = value*dt) %>% 
        summarise(value = sum(demi)) %>% 
        ungroup()
    )
  }
  
  p <- ggplot(data_plot %>% 
                mutate(variable=paste0(variable)) %>% 
                mutate(variable=ifelse(variable == i_x, "X", "Y")) %>% 
                spread(variable, value)) +
    geom_point(aes_string(x="X", y="Y", color="tempcat")) +
    theme_bw() + 
    theme(legend.position = "bottom")
  
  if (is.null(i_varnameX)) {
    p <- p + xlab(ifelse(i_cumX, paste0("Cumulative ", i_x), i_x))  
  } else {
    p <- p + xlab(i_varnameX)  
  }
  
  if (is.null(i_varnameY)) {
    p <- p + ylab(ifelse(i_cumY, paste0("Cumulative ", i_y), i_y))  
  } else {
    p <- p + ylab(i_varnameY)  
  }
  
  if (!is.null(i_xlim)) p <- p + xlim(i_xlim)  
  if (!is.null(i_ylim)) p <- p + ylim(i_ylim)  
  
  print(p)
  
  return(p)
  
}
plot_scenario_var_scatter_TTT <- function(i_data, i_x, i_y, i_period=seq(2010,2100,10), i_factorX=1, i_factorY=1, i_varnameX=NULL, i_varnameY=NULL, i_xlim=NULL, i_ylim=NULL, i_cumX=FALSE, i_cumY=FALSE) {
  
  data_plot <- i_data %>% 
    filter(
      period %in% i_period,
      region == "World",
      variable %in% c(i_x, i_y),
      tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
    dplyr::select(scenario,variable,period,value,tempcat,timingcat,techcat)
  
  if (i_factorX != 1) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_x, value*i_factorX, value))
  
  if (i_factorY != 1) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_y, value*i_factorY, value))
  
  if (i_cumX & i_cumY) {
    data_plot <- rbind(
      data_plot %>% 
        filter(variable == i_x) %>% 
        group_by(scenario,variable,tempcat,timingcat,techcat) %>% 
        arrange(period) %>% 
        mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
        mutate(demi = value*dt) %>% 
        summarise(value = sum(demi)) %>% 
        ungroup(),
      data_plot %>% 
        filter(variable == i_y) %>% 
        group_by(scenario,variable,tempcat,timingcat,techcat) %>% 
        arrange(period) %>% 
        mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
        mutate(demi = value*dt) %>% 
        summarise(value = sum(demi)) %>% 
        ungroup()
      )
  }
  
  p <- ggplot(data_plot %>% 
                filter(
                  timingcat %in% c("immediate", "delay2030"), 
                  techcat %in% c("default", "limBio", "noCCS")) %>% 
                mutate(variable=paste0(variable)) %>% 
                mutate(variable=ifelse(variable == i_x, "X", "Y")) %>% 
                spread(variable, value)) +
    geom_point(aes_string(x="X", y="Y", color="timingcat", shape="techcat")) +
    facet_grid(~tempcat) +
    theme_bw() + 
    theme(legend.position = "bottom")
  
  if (is.null(i_varnameX)) {
    p <- p + xlab(ifelse(i_cumX, paste0("Cumulative ", i_x), i_x))  
  } else {
    p <- p + xlab(i_varnameX)  
  }
  
  if (is.null(i_varnameY)) {
    p <- p + ylab(ifelse(i_cumY, paste0("Cumulative ", i_y), i_y))  
  } else {
    p <- p + ylab(i_varnameY)  
  }
  
  if (!is.null(i_xlim)) p <- p + xlim(i_xlim)  
  if (!is.null(i_ylim)) p <- p + ylim(i_ylim)  
  
  print(p)
  
}

plot_scenario_var_scatter(v_data_tempTargets_world_plot, i_x="Emissions|CO2", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)
plot_scenario_var_scatter(v_data_tempTargets_world_plot %>% compute_grossCO2emiFFI(), i_x="Emissions|CO2|Fossil Fuels and Industry|Gross", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)
plot_scenario_var_scatter_TTT(v_data_timeTechTemp_world_plot, i_x="Emissions|CO2", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)



add_emi2030  <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable == "Emissions|CO2", 
           period == 2030) %>% 
    mutate(value = value*1e-3)
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "value"))]
  names(tmp)[which(names(tmp) == "value")] <- "emi2030"
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
}
add_cp2030   <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable == "Price|Carbon", 
           period == 2030)
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "value"))]
  names(tmp)[which(names(tmp) == "value")] <- "cp2030"
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
}
add_cp2040   <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable == "Price|Carbon", 
           period == 2040)
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "value"))]
  names(tmp)[which(names(tmp) == "value")] <- "cp2040"
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
}
add_cp2050   <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable == "Price|Carbon", 
           period == 2050)
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "value"))]
  names(tmp)[which(names(tmp) == "value")] <- "cp2050"
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
}
add_BECCSDep <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass", 
           period == 2100) %>% 
    mutate(value = value*1e-3)
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "value"))]
  names(tmp)[which(names(tmp) == "value")] <- "beccsDep"
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
  
}
add_cumNE    <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(
      period >= 2010,
      region == "World",
      variable == "Emissions|CO2|Carbon Capture and Storage|Biomass") %>% 
    group_by_at(keep_cols) %>% 
    arrange(period) %>% 
    mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
    mutate(demi = value*dt) %>% 
    summarise(value = sum(demi)/1000) %>% 
    ungroup()
  
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "value"))]
  names(tmp)[which(names(tmp) == "value")] <- "cumNE"
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
  
}
add_avgEmiRed20302050 <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable == "Emissions|CO2", 
           period %in% c(2030,2050)) %>% 
    mutate(value = value/1000) %>% 
    spread(period, value) %>% 
    mutate(avgEmiRed20302050=(`2050`-`2030`)/20) %>% 
    dplyr::select(-`2030`, -`2050`)
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "avgEmiRed20302050"))]
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
  
}
add_avgNegEmi20302050 <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass", 
           period %in% c(2030,2050)) %>% 
    mutate(value = value/1000) %>% 
    spread(period, value) %>% 
    mutate(avgNegEmi20302050=(`2050`-`2030`)/20) %>% 
    dplyr::select(-`2030`, -`2050`)
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "avgNegEmi20302050"))]
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
  
}
add_RavgEmiRed20302050 <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable == "Emissions|CO2", 
           period %in% c(2030,2050)) %>% 
    mutate(value = value/1000) %>% 
    spread(period, value) %>% 
    mutate(RavgEmiRed20302050=(`2050`-`2030`)/`2030`/20) %>% 
    dplyr::select(-`2030`, -`2050`)
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "RavgEmiRed20302050"))]
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
  
}
add_RavgNegEmi20302050 <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass", 
           period %in% c(2030,2050)) %>% 
    mutate(value = value/1000) %>% 
    spread(period, value) %>% 
    mutate(RavgNegEmi20302050=(`2050`-`2030`)/`2030`/20) %>% 
    dplyr::select(-`2030`, -`2050`)
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "RavgNegEmi20302050"))]
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
  
}
add_residualCO2Emi <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(variable %in% c("Emissions|CO2|Carbon Capture and Storage|Biomass", 
                           "Emissions|CO2|Fossil Fuels and Industry"), 
           period == 2100) %>% 
    mutate(value=`Emissions|CO2|Fossil Fuels and Industry`+`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
    select(-`Emissions|CO2|Fossil Fuels and Industry`, -`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
    mutate(value = value*1e-3)
  
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "value"))]
  names(tmp)[which(names(tmp) == "value")] <- "resEmi"
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
  
}
add_cumPE    <- function(i_data, i_keepcol) {
  
  keep_cols <- c("model", "scenario", i_keepcol)
  
  tmp <- i_data %>% 
    filter(
      period >= 2010,
      region == "World",
      variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
    group_by_at(keep_cols) %>% 
    arrange(period) %>% 
    mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
    mutate(demi = value*dt) %>% 
    summarise(value = sum(demi)/1000) %>% 
    ungroup()
  
  tmp <- tmp[which(names(tmp) %in% c(keep_cols, "value"))]
  names(tmp)[which(names(tmp) == "value")] <- "cumPE"
  
  out <- i_data %>% 
    left_join(
      tmp,
      by=c(keep_cols)
    )
  
  return(out)
  
}

add_allLevers <- function(i_data, i_keepcol) {
  out <- i_data %>% 
    add_emi2030(i_keepcol) %>% 
    add_cp2030(c(i_keepcol, "emi2030")) %>% 
    add_BECCSDep(c(i_keepcol, "emi2030","cp2030")) %>% 
    add_avgEmiRed20302050(c(i_keepcol, "emi2030","cp2030","beccsDep")) %>% 
    add_avgNegEmi20302050(c(i_keepcol, "emi2030","cp2030","beccsDep","avgEmiRed20302050")) %>% 
    add_RavgEmiRed20302050(c(i_keepcol, "emi2030","cp2030","beccsDep","avgEmiRed20302050","avgNegEmi20302050")) %>% 
    add_RavgNegEmi20302050(c(i_keepcol, "emi2030","cp2030","beccsDep","avgEmiRed20302050","avgNegEmi20302050","RavgEmiRed20302050")) %>% 
    add_cumNE(c(i_keepcol, "emi2030","cp2030","beccsDep","avgEmiRed20302050","avgNegEmi20302050","RavgEmiRed20302050","RavgNegEmi20302050")) %>% 
    add_cp2040(c(i_keepcol, "emi2030","cp2030","beccsDep","avgEmiRed20302050","avgNegEmi20302050","RavgEmiRed20302050","RavgNegEmi20302050", "cumNE")) %>% 
    add_cp2050(c(i_keepcol, "emi2030","cp2030","beccsDep","avgEmiRed20302050","avgNegEmi20302050","RavgEmiRed20302050","RavgNegEmi20302050", "cumNE", "cp2040"))
  
  return(out)
}

plot_scenario_var_scatter_wCP <- function(i_data, i_x, i_y, i_period=seq(2010,2100,10), i_factorX=1, i_factorY=1, i_varnameX=NULL, i_varnameY=NULL, i_xlim=NULL, i_ylim=NULL, i_cumX=FALSE, i_cumY=FALSE) {
  
  data_plot <- i_data %>% 
    filter(
      period %in% i_period,
      region == "World",
      variable %in% c(i_x, i_y),
      tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
    dplyr::select(scenario,variable,period,value,tempcat,cp2030)
  
  if (i_factorX != 1) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_x, value*i_factorX, value))
  
  if (i_factorY != 1) data_plot <- data_plot %>% mutate(value=ifelse(variable == i_y, value*i_factorY, value))
  
  if (i_cumX & i_cumY) {
    data_plot <- rbind(
      data_plot %>% 
        filter(variable == i_x) %>% 
        group_by(scenario,variable,tempcat,cp2030) %>% 
        arrange(period) %>% 
        mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
        mutate(demi = value*dt) %>% 
        summarise(value = sum(demi)) %>% 
        ungroup(),
      data_plot %>% 
        filter(variable == i_y) %>% 
        group_by(scenario,variable,tempcat,cp2030) %>% 
        arrange(period) %>% 
        mutate(dt   = 0.5*(period - lag(period, default=2010)) + 0.5*(lead(period, default=2100) - period)) %>% 
        mutate(demi = value*dt) %>% 
        summarise(value = sum(demi)) %>% 
        ungroup()
    )
  }
  
  p <- ggplot(data_plot %>% 
                mutate(variable=paste0(variable)) %>% 
                mutate(variable=ifelse(variable == i_x, "X", "Y")) %>% 
                spread(variable, value)) +
    geom_point(aes_string(x="X", y="Y", color="tempcat", size="cp2030")) +
    theme_bw() + 
    theme(legend.position = "bottom")
  
  if (is.null(i_varnameX)) {
    p <- p + xlab(ifelse(i_cumX, paste0("Cumulative ", i_x), i_x))  
  } else {
    p <- p + xlab(i_varnameX)  
  }
  
  if (is.null(i_varnameY)) {
    p <- p + ylab(ifelse(i_cumY, paste0("Cumulative ", i_y), i_y))  
  } else {
    p <- p + ylab(i_varnameY)  
  }
  
  if (!is.null(i_xlim)) p <- p + xlim(i_xlim)  
  if (!is.null(i_ylim)) p <- p + ylim(i_ylim)  
  
  print(p)
  
}
plot_scenario_var_scatter_wCP(v_data_tempTargets_world_plot %>% compute_grossCO2emiFFI() %>% add_cp2030("tempcat"), i_x="Emissions|CO2|Fossil Fuels and Industry|Gross", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)




tmp <- data_1p5 %>% 
  filter(
    region == "World",
    grepl("Emi.CO2|Emi.CH4|Emi.N2O|Emi.F-Gases", variable)) %>% 
  filter(!grepl("Cumulated|LDV", variable))

  