#### USER SECTION ######################################
u_path_1p5             <- "../../bitbucket/beccs/data/REMIND_generics_20150512.csv"
u_path_1p5ScenarioInfo <- "../../bitbucket/beccs/data/one_five_scenarios.Rdata"
u_reload1p5_data       <- FALSE


#### INITIALISE ########################################
#=== Libraries =========================================
library(tidyverse)
library(ggplot2)
library(plotly)

#=== Own functions =====================================
source("functions/functions_quantitativeAnalysis.R")


#### LOAD DATA #########################################
#=== Compiled data from NETs review ====================
load("../../bitbucket/beccs/data/dataplotAll_20180222.RData")

#== Add new data =======================================
v_data_tempTargets_world_plot <- v_data_tempTargets_world_plot %>% 
  mutate(model    = paste(model)) %>% 
  mutate(scenario = paste(scenario)) %>% 
  mutate(region   = paste(region)) %>% 
  mutate(variable = paste(variable)) %>% 
  mutate(variable = ifelse(variable == "Emissions|CO2|Land-Use Change", "Emissions|CO2|Land Use", variable)) %>% 
  mutate(tempcat  = paste(tempcat))

# Rogelj et al 2018 already included in NETs review
# Holz et al 2018 (1.5°C scenario)
load("data/data_holz2018.RData")
v_data_tempTargets_world_plot <- v_data_tempTargets_world_plot %>% 
  rbind(
    data_holz2018 %>% 
      select(-unit) %>% 
      filter(period %in% c(2005, seq(2010,2100,10))) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|AFOLU", "Emissions|CO2|Land Use", variable)) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|Fossil Fuels and Industry", variable)) %>% 
      mutate(variable = ifelse(variable %in% c('Carbon Sequestration|Land Use', 
                                               #'Carbon Sequestration|Land Use|Afforestation', 
                                               #'Carbon Sequestration|Land Use|Biochar', 
                                               #'Carbon Sequestration|Land Use|Soil Carbon Management', 
                                               'Carbon Sequestration|CCS|Biomass', 
                                               'Carbon Sequestration|Direct Air Capture', 
                                               'Carbon Sequestration|Enhanced Weathering'), "Emissions|CO2|Carbon Capture and Storage|Biomass", variable)) %>% 
      group_by(model,scenario,region,variable,period) %>% 
      summarize(value=sum(value)) %>% 
      ungroup() %>% 
      mutate(tempcat = ifelse(scenario!="Reference", "1.5°C scenario", "Other scenario"))
    )

# van Vuuren et al 2018 (1.5°C scenario)
load("data/data_vanVuuren2018.RData")
v_data_tempTargets_world_plot <- v_data_tempTargets_world_plot %>% 
  rbind(
    data_vanVuuren2018 %>% 
      select(-unit)  %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|AFOLU", "Emissions|CO2|Energy and Industrial Processes", 
                             "Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Land Use", 
                             "Carbon Sequestration|Other")) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|AFOLU", "Emissions|CO2|Land Use", variable)) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|Fossil Fuels and Industry", variable)) %>% 
      mutate(variable = ifelse(variable %in% c('Carbon Sequestration|Land Use', 
                                               'Carbon Sequestration|CCS|Biomass', 
                                               'Carbon Sequestration|Other'), "Emissions|CO2|Carbon Capture and Storage|Biomass", variable)) %>% 
      group_by(model,scenario,region,variable,period) %>% 
      summarize(value=sum(value)) %>% 
      ungroup() %>% 
      mutate(tempcat = "1.5°C scenario") 
  )
  
# Grubler et al 2018 (1.5°C scenario)
load("data/data_grubler2018.RData")
v_data_tempTargets_world_plot <- v_data_tempTargets_world_plot %>% 
  rbind(
    data_grubler2018 %>% 
      select(-unit)  %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|AFOLU", "Emissions|CO2|Energy and Industrial Processes", 
                             "Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Land Use", 
                             "Carbon Sequestration|Other")) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|AFOLU", "Emissions|CO2|Land Use", variable)) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|Fossil Fuels and Industry", variable)) %>% 
      mutate(variable = ifelse(variable %in% c('Carbon Sequestration|Land Use', 
                                               'Carbon Sequestration|CCS|Biomass', 
                                               'Carbon Sequestration|Other'), "Emissions|CO2|Carbon Capture and Storage|Biomass", variable)) %>% 
      group_by(model,scenario,region,variable,period) %>% 
      summarize(value=sum(value)) %>% 
      ungroup() %>% 
      mutate(tempcat = "1.5°C scenario") 
  )

# Kobler et al 2018
#readxl::read_excel("data/ETSAP_Book_CO2_emissions_SentToJerome.xlsx")
# Only until 2060, only energy system emissions (CO2 emissions FFI, CCS, BECCS), only 1 1.5 scenario

# Marcucci et al 2018 scenarios ?
load("data/data_marcucci2017.RData")
v_data_tempTargets_world_plot <- v_data_tempTargets_world_plot %>% 
  rbind(
    data_marcucci2017 %>% 
      select(-unit, -technology) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|Non Energy", "Emissions|CO2|Land Use", variable)) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|Energy", "Emissions|CO2|Fossil Fuels and Industry", variable)) %>% 
      mutate(variable = ifelse(variable %in% c("Carbon Sequestration|CCS|Biomass",
                                               "Carbon Sequestration|Direct Air Capture"), "Emissions|CO2|Carbon Capture and Storage|Biomass", variable)) %>% 
      group_by(model, scenario, region, variable,period) %>% 
      summarize(value=sum(value)) %>% 
      ungroup() %>% 
      # filter(! variable %in% c("Emissions|CO2|Energy", "Carbon Sequestration|CCS|Fossil")) %>% 
      # rbind(
      #   data_marcucci2017 %>% 
      #     select(-unit) %>% 
      #     filter(variable %in% c("Emissions|CO2|Energy", "Carbon Sequestration|CCS|Fossil")) %>% 
      #     spread(variable, value) %>% 
      #     mutate(`Emissions|CO2|Fossil Fuels and Industry` = `Emissions|CO2|Energy`-`Carbon Sequestration|CCS|Fossil`)
      # ) %>% 
      mutate(tempcat = ifelse(scenario == "DAC15_50", "1.5°C scenario", scenario)) %>% 
      mutate(tempcat = ifelse(scenario %in% c("S2_66", "DAC2_66"), "Likely 2.0°C scenario", scenario))
    )

# Luderer et al 2018
load("data/ADVANCE_scenarios/advance_compare_20180428-180811_proc.RData")
v_data_tempTargets_world_plot <- v_data_tempTargets_world_plot %>% 
  rbind(
    v_procDataScen %>% 
      select(-unit)  %>% 
      mutate(model = paste(model)) %>% 
      mutate(scenario = paste(scenario)) %>% 
      mutate(region = paste(region)) %>% 
      mutate(variable = paste(variable)) %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|AFOLU", "Emissions|CO2|Energy and Industrial Processes", 
                             "Carbon Sequestration|CCS|Biomass", "Carbon Sequestration|Land Use", 
                             "Carbon Sequestration|Other")) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|AFOLU", "Emissions|CO2|Land Use", variable)) %>% 
      mutate(variable = ifelse(variable=="Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|Fossil Fuels and Industry", variable)) %>% 
      mutate(variable = ifelse(variable %in% c('Carbon Sequestration|Land Use', 
                                               'Carbon Sequestration|CCS|Biomass', 
                                               'Carbon Sequestration|Other'), "Emissions|CO2|Carbon Capture and Storage|Biomass", variable)) %>% 
      group_by(model,scenario,region,variable,period) %>% 
      summarize(value=sum(value)) %>% 
      ungroup() %>% 
      left_join(
        v_procDataScen %>% 
          filter(variable == "Diagnostics|MAGICC6|Temperature|Global Mean") %>% 
          select(-unit)  %>% 
          mutate(model = paste(model)) %>% 
          mutate(scenario = paste(scenario)) %>% 
          mutate(region = paste(region)) %>% 
          mutate(variable = paste(variable)) %>% 
          group_by(model, scenario) %>% 
          summarise(
            temp2100      = sum(ifelse(period == 2100, value, 0))) %>% 
          mutate(tempcat = ifelse(temp2100 < 1.5, "1.5°C scenario", "Other scenario")) %>% 
          mutate(tempcat = ifelse(temp2100 >= 1.5 & temp2100 < 2, "Likely 2°C scenario", "Other scenario")) %>% 
          ungroup() %>% 
          select(-temp2100),
        by=c("model", "scenario")
      )
  )



  

#=== Rogelj et al (2015) scenarios (REMIND only) =======
if (u_reload1p5_data) {
  #--- Load REMIND data ----------------------------------
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
  #--- Load REMIND scenario information ------------------
  load(u_path_1p5ScenarioInfo)
  #=== MESSAGE and REMIND data from Joeri ================
  data_1p5v2scenclass <- readxl::read_xls("../../bitbucket/beccs/data/1p5CSCENs_categorization_forJanMINX.xls")
}


#### PROCESS DATA ######################################
if (u_reload1p5_data) {
  data_1p5 <- process_1p5data(data_1p5, onefivescenarios, data_1p5v2scenclass)
  save(data_1p5, file = "data/data_1p5.RData")
} else {
  load("data/data_1p5.RData")
}


#### PLOT DATA #########################################
#=== Panel figure Emissions & CP ========
source("scripts/plot_panel_emi_cp.R")


#=== Cum gross NE vs cum gross PE (scatter plots) ======
source("scripts/plot_quantAnal_cumGPEvscumGNE.R")

#=== Average emission reduction VS cumulative negative emissions VS carbon price in 2030 =====
source("scripts/plot_avgEmiRed_cumNE_CP.R")

#=== Carbon prices =====================================


#=== Scatter plots =====================================
source("scripts/plot_quantAnal_sandbox.R")

#=== Scatter plots =====================================
source("scripts/plot_quantAnal_scatters.R")

#=== Histograms ========================================
#source("scripts/plot_quantAnal_histograms.R")

#=== Boxplots ==========================================
source("scripts/plot_quantAnal_boxplots.R")

#=== Density plots =====================================
#source("scripts/plot_quantAnal_densityPlots.R")

#=== Maps ==============================================
#source("scripts/plot_quantAnal_maps.R")