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
load("../../bitbucket/beccs/data/dataplotAll.RData")
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