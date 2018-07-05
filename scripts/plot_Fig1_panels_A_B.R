#==== User section ============
u_tick_x <- c(2005,seq(2010,2100,10))
u_tick_y <- c(-25, seq(-20,50,10))

u_col_case           <- list()
u_col_case$`2C`      <- c("#3f91c5", "#cfdfeaff")
u_col_case$`15C`     <- c("#800000", "#e8cccf66")
u_col_case$`NoBECCS` <- c("#984EA3", "#eed7f466")
u_col_case$Delay     <- c("#FF7F00", "#ffe6d566")
u_col_case$SSP5      <- c("#000000", "#ebe9e866")
u_col_case$LED       <- c("#008000", "#e8ffe866")


#==== Functions ===============
get_data <- function(i_case, i_metric) {
  
  u_2degcases <- c("Likely 2.0°C scenario", "Medium 2.0°C scenario")
  
  compute_pathway        <- function(i_data) {
    out <- i_data %>% 
      group_by(model,scenario,region,category) %>% 
      mutate(keep=ifelse(any(is.na(value)), FALSE, TRUE)) %>% 
      ungroup() %>% 
      spread(variable, value) %>% 
      mutate(keep = ifelse(is.na(`Emissions|CO2`) | is.na(`Emissions|CO2|Fossil Fuels and Industry`) | 
                             is.na(`Emissions|CO2|Land Use`) | is.na(`Emissions|CO2|Carbon Capture and Storage|Biomass`), FALSE, TRUE)) %>% 
      mutate(check = `Emissions|CO2` - (`Emissions|CO2|Fossil Fuels and Industry`+`Emissions|CO2|Land Use`)) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross` = `Emissions|CO2|Fossil Fuels and Industry` + `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      mutate(`Emissions|CO2|Carbon Capture and Storage|Biomass` = -`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross` = `Emissions|CO2|Fossil Fuels and Industry|Gross` + ifelse(`Emissions|CO2|Land Use`>= 0, `Emissions|CO2|Land Use`, 0)) %>% 
      mutate(`Emissions|CO2|Carbon Capture and Storage|Biomass` = `Emissions|CO2|Carbon Capture and Storage|Biomass` + ifelse(`Emissions|CO2|Land Use`< 0, `Emissions|CO2|Land Use`, 0)) %>% 
      mutate(period = as.numeric(period)) %>% 
      filter(period > 2005) %>% 
      filter(keep == TRUE) %>% 
      select(-keep, -check) %>% 
      gather(variable, value, -model, -scenario, -region, -period, -category) %>% 
      group_by(category,period,variable)  %>%  
      summarise(
        min=min(value, na.rm=T),
        p01=quantile(value, 0.01, na.rm=T),
        p05=quantile(value, 0.05, na.rm=T),
        p10=quantile(value, 0.10, na.rm=T),
        p15=quantile(value, 0.15, na.rm=T),
        mean=mean(value, na.rm=T),
        med=median(value, na.rm=T),
        p85=quantile(value, 0.85, na.rm=T),
        p90=quantile(value, 0.90, na.rm=T),
        p95=quantile(value, 0.95, na.rm=T),
        p99=quantile(value, 0.99, na.rm=T),
        max=max(value, na.rm=T),
        n=n()
      ) %>% 
      ungroup() 
    return(out)
  }
  compute_grossCO2emi    <- function(i_data) {
    out <- i_data %>% 
      filter(variable %in% c("Emissions|CO2|Fossil Fuels and Industry", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      group_by(model,scenario,region,category) %>% 
      mutate(keep=ifelse(any(is.na(value)), FALSE, TRUE)) %>% 
      ungroup() %>% 
      spread(variable, value) %>% 
      mutate(keep = ifelse(is.na(`Emissions|CO2|Fossil Fuels and Industry`) | is.na(`Emissions|CO2|Carbon Capture and Storage|Biomass`), FALSE, TRUE)) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross` = `Emissions|CO2|Fossil Fuels and Industry` + `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      mutate(period = as.numeric(period)) %>% 
      filter(period > 2005) %>% 
      filter(keep == TRUE) %>% 
      select(-keep, -`Emissions|CO2|Fossil Fuels and Industry`, - `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      gather(variable, value, -model, -scenario, -region, -period, -category) %>%
      filter(period %in% c(2030,2050)) %>% 
      spread(period, value) %>% 
      mutate(value = -(`2050`-`2030`)/20) %>% 
      group_by(category) %>% 
      summarise(
        min=min(value, na.rm=T),
        p01=quantile(value, 0.01, na.rm=T),
        p05=quantile(value, 0.05, na.rm=T),
        p10=quantile(value, 0.10, na.rm=T),
        p15=quantile(value, 0.15, na.rm=T),
        mean=mean(value, na.rm=T),
        med=median(value, na.rm=T),
        p85=quantile(value, 0.85, na.rm=T),
        p90=quantile(value, 0.90, na.rm=T),
        p95=quantile(value, 0.95, na.rm=T),
        p99=quantile(value, 0.99, na.rm=T),
        max=max(value, na.rm=T)
      ) %>% 
      ungroup()
    return(out)
  }
  compute_grossnegCO2emi <- function(i_data) {
    out <- i_data %>% 
      filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass") %>% 
      filter(period %in% c(2030,2050)) %>% 
      spread(period, value) %>% 
      mutate(value = (`2050`-`2030`)/20) %>% 
      group_by(category) %>% 
      summarise(
        min=min(value, na.rm=T),
        p01=quantile(value, 0.01, na.rm=T),
        p05=quantile(value, 0.05, na.rm=T),
        p10=quantile(value, 0.10, na.rm=T),
        p15=quantile(value, 0.15, na.rm=T),
        mean=mean(value, na.rm=T),
        med=median(value, na.rm=T),
        p85=quantile(value, 0.85, na.rm=T),
        p90=quantile(value, 0.90, na.rm=T),
        p95=quantile(value, 0.95, na.rm=T),
        p99=quantile(value, 0.99, na.rm=T),
        max=max(value, na.rm=T),
        n=n()
      ) %>% 
      ungroup()
  }
  compute_resco2emi      <- function(i_data) {
    out <- i_data %>% 
      filter(variable %in% c("Emissions|CO2|Fossil Fuels and Industry", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      group_by(model,scenario,region,category) %>% 
      mutate(keep=ifelse(any(is.na(value)), FALSE, TRUE)) %>% 
      ungroup() %>% 
      spread(variable, value) %>% 
      mutate(keep = ifelse(is.na(`Emissions|CO2|Fossil Fuels and Industry`) | is.na(`Emissions|CO2|Carbon Capture and Storage|Biomass`), FALSE, TRUE)) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross` = `Emissions|CO2|Fossil Fuels and Industry` + `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      mutate(period = as.numeric(period)) %>% 
      filter(period > 2005) %>% 
      filter(keep == TRUE) %>% 
      select(-keep, -`Emissions|CO2|Fossil Fuels and Industry`, - `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      gather(variable, value, -model, -scenario, -region, -period, -category) %>%
      filter(period %in% c(2090,2100)) %>% 
      spread(period, value) %>% 
      mutate(value = (`2100`+`2090`)/2) %>% 
      group_by(category) %>% 
      summarise(
        min=min(value, na.rm=T),
        p01=quantile(value, 0.01, na.rm=T),
        p05=quantile(value, 0.05, na.rm=T),
        p10=quantile(value, 0.10, na.rm=T),
        p15=quantile(value, 0.15, na.rm=T),
        mean=mean(value, na.rm=T),
        med=median(value, na.rm=T),
        p85=quantile(value, 0.85, na.rm=T),
        p90=quantile(value, 0.90, na.rm=T),
        p95=quantile(value, 0.95, na.rm=T),
        p99=quantile(value, 0.99, na.rm=T),
        max=max(value, na.rm=T)
      ) %>% 
      ungroup()
    
    return(out)
  }
  compute_cumnegCO2emi   <- function(i_data) {
    out <- out %>% 
      filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass") %>% 
      mutate(period = as.numeric(period)) %>% 
      filter(period > 2005) %>% 
      group_by(model, scenario, region, category) %>% 
      arrange(period) %>% 
      mutate(dt  = (period   - lag(period, default = 2010))) %>% 
      mutate(tmp = dt*(value + lag(value, default = 0))/2) %>% 
      summarise(value = sum(tmp)) %>% 
      ungroup() %>% 
      group_by(category) %>%  
      summarise(
        min=min(value, na.rm=T),
        p01=quantile(value, 0.01, na.rm=T),
        p05=quantile(value, 0.05, na.rm=T),
        p10=quantile(value, 0.10, na.rm=T),
        p15=quantile(value, 0.15, na.rm=T),
        mean=mean(value, na.rm=T),
        med=median(value, na.rm=T),
        p85=quantile(value, 0.85, na.rm=T),
        p90=quantile(value, 0.90, na.rm=T),
        p95=quantile(value, 0.95, na.rm=T),
        p99=quantile(value, 0.99, na.rm=T),
        max=max(value, na.rm=T),
        n=n()
      ) %>% 
      ungroup()
  }
  compute_cumCO2emi_all <- function(i_data) {
    out <- i_data %>% 
      group_by(model,scenario,region,category) %>% 
      mutate(keep=ifelse(any(is.na(value)), FALSE, TRUE)) %>% 
      ungroup() %>% 
      spread(variable, value) %>% 
      mutate(keep = ifelse(is.na(`Emissions|CO2`) | is.na(`Emissions|CO2|Fossil Fuels and Industry`) | 
                             is.na(`Emissions|CO2|Land Use`) | is.na(`Emissions|CO2|Carbon Capture and Storage|Biomass`), FALSE, TRUE)) %>% 
      mutate(check = `Emissions|CO2` - (`Emissions|CO2|Fossil Fuels and Industry`+`Emissions|CO2|Land Use`)) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross` = `Emissions|CO2|Fossil Fuels and Industry` + `Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      mutate(`Emissions|CO2|Carbon Capture and Storage|Biomass` = -`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
      mutate(period = as.numeric(period)) %>% 
      filter(period > 2005) %>% 
      group_by(model, scenario, region, category) %>% 
      arrange(period) %>% 
      mutate(dt  = (period   - lag(period, default = 2010))) %>% 
      mutate(tmp = dt*(`Emissions|CO2` + lag(`Emissions|CO2`, default = 0))/2) %>% 
      mutate(`Emissions|CO2|Cumulative` = cumsum(tmp)) %>% 
      mutate(tmp = dt*(`Emissions|CO2|Carbon Capture and Storage|Biomass` + lag(`Emissions|CO2|Carbon Capture and Storage|Biomass`, default = 0))/2) %>% 
      mutate(`Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative` = cumsum(tmp)) %>% 
      mutate(tmp = dt*(`Emissions|CO2|Fossil Fuels and Industry` + lag(`Emissions|CO2|Fossil Fuels and Industry`, default = 0))/2) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Cumulative` = cumsum(tmp)) %>% 
      mutate(tmp = dt*(`Emissions|CO2|Fossil Fuels and Industry|Gross` + lag(`Emissions|CO2|Fossil Fuels and Industry|Gross`, default = 0))/2) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative` = cumsum(tmp)) %>% 
      mutate(tmp = dt*(`Emissions|CO2|Land Use` + lag(`Emissions|CO2|Land Use`, default = 0))/2) %>% 
      mutate(`Emissions|CO2|Land Use|Cumulative` = cumsum(tmp)) %>% 
      select(-dt, -tmp) %>% 
      select(model,scenario,region,period,category, keep, check,
             `Emissions|CO2`, `Emissions|CO2|Fossil Fuels and Industry`, `Emissions|CO2|Fossil Fuels and Industry|Gross`, 
             `Emissions|CO2|Carbon Capture and Storage|Biomass`, `Emissions|CO2|Land Use`, 
             `Emissions|CO2|Cumulative`, `Emissions|CO2|Fossil Fuels and Industry|Cumulative`, `Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative`, 
             `Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative`, `Emissions|CO2|Land Use|Cumulative`) %>% 
      ungroup() %>% 
      filter(keep == TRUE) %>% 
      select(-keep, -check) %>% 
      gather(variable, value, -model, -scenario, -region, -period, -category) %>% 
      filter(period %in% c(2050, 2100)) %>% 
      spread(period, value) %>% 
      mutate(`2051-2100`=`2100`-`2050`) %>% 
      rename(`2011-2050`=`2050`) %>% 
      rename(`2011-2100`=`2100`) %>% 
      gather(period, value, -model,-scenario,-region,-category,-variable) %>% 
      spread(variable, value) %>% 
      mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative` = `Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative` + `Emissions|CO2|Land Use|Cumulative`) %>%
      gather(variable, value, -model, -scenario, -region, -period, -category) %>% 
      group_by(category,period,variable)  %>%  
      summarise(
        min=min(value, na.rm=T),
        p01=quantile(value, 0.01, na.rm=T),
        p05=quantile(value, 0.05, na.rm=T),
        p10=quantile(value, 0.10, na.rm=T),
        p15=quantile(value, 0.15, na.rm=T),
        mean=mean(value, na.rm=T),
        med=median(value, na.rm=T),
        p85=quantile(value, 0.85, na.rm=T),
        p90=quantile(value, 0.90, na.rm=T),
        p95=quantile(value, 0.95, na.rm=T),
        p99=quantile(value, 0.99, na.rm=T),
        max=max(value, na.rm=T)
      ) %>% 
      ungroup() 
  }
  
  if (i_case == "2C") {
    out <- v_data_timeTechTemp_world_plot %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                             "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      filter(tempcat %in% u_2degcases) %>% 
      mutate(tempcat = paste(tempcat)) %>% 
      mutate(tempcat = "2°C") %>% 
      filter(allcat == "Default") %>% 
      rename(category=tempcat) %>% 
      select(-allcat, -timingcat, -techcat) %>% 
      rbind(v_data_tempTargets_world_plot %>% 
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::rename(category=tempcat) %>%
              dplyr::mutate(category = paste(category)) %>%
              dplyr::filter(category %in% u_2degcases) %>% 
              mutate(category = "2°C")) %>% 
      mutate(modscen=paste(model,"-",scenario)) %>% 
      group_by(region,variable,period) %>% 
      filter(!duplicated(modscen)) %>% 
      ungroup() %>% 
      select(-modscen) %>% 
      mutate(value = value/1000)
    
    # if (i_metric == "pathway") out <- out %>% compute_pathway_tempcat()
    # if (i_metric == "grossposCO2emired") out <- out %>% compute_grossCO2emi_tempcat()
    # if (i_metric == "grossnegCO2emiinc") out <- out %>% compute_grossnegCO2emi_tempcat()
    # if (i_metric == "resCO2emi") out <- out %>% compute_resco2emi_tempcat()
    # if (i_metric == "cumnegCO2emi") out <- out %>% compute_cumnegCO2emi_tempcat()
  }
  if (i_case == "2C_vs_15C") {
    out <- v_data_timeTechTemp_world_plot %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                             "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      filter(tempcat %in% u_2degcases) %>% 
      mutate(tempcat = paste(tempcat)) %>% 
      mutate(tempcat = "2°C") %>%
      filter(allcat == "Default") %>% 
      rename(category=tempcat) %>% 
      select(-allcat, -timingcat, -techcat) %>% 
      rbind(v_data_tempTargets_world_plot %>% 
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::rename(category=tempcat) %>%
              dplyr::mutate(category = paste(category)) %>%
              dplyr::filter(category %in% u_2degcases) %>% 
              mutate(category = "2°C")) %>% 
      mutate(modscen=paste(model,"-",scenario)) %>% 
      group_by(region,variable,period) %>% 
      filter(!duplicated(modscen)) %>% 
      ungroup() %>% 
      select(-modscen) %>% 
      rbind(v_data_tempTargets_world_plot %>% 
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::rename(category=tempcat) %>%
              dplyr::mutate(category = paste(category)) %>%
              dplyr::filter(category=="1.5°C scenario") %>% 
              mutate(category = "1.5°C")) %>% 
      mutate(value = value/1000) 
    
    # if (i_metric == "pathway") out <- out %>% compute_pathway_tempcat()
    # if (i_metric == "grossposCO2emired") out <- out %>% compute_grossCO2emi_tempcat()
    # if (i_metric == "grossnegCO2emiinc") out <- out %>% compute_grossnegCO2emi_tempcat()
    # if (i_metric == "resCO2emi") out <- out %>% compute_resco2emi_tempcat()
    # if (i_metric == "cumnegCO2emi") out <- out %>% compute_cumnegCO2emi_tempcat()
  }
  if (i_case == "2C_vs_NoBECCS") {
    out <- v_data_timeTechTemp_world_plot %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                             "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      filter(tempcat %in% u_2degcases) %>% 
      mutate(tempcat = paste(tempcat)) %>% 
      mutate(tempcat = "2°C") %>% 
      filter(allcat %in% c("Default", "No CCS/BECCS")) %>% 
      rename(category=allcat) %>% 
      select(-tempcat,-timingcat,-techcat) %>% 
      rbind(v_data_tempTargets_world_plot %>% 
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::rename(category=tempcat) %>%
              dplyr::mutate(category = paste(category)) %>%
              dplyr::filter(category %in% u_2degcases) %>% 
              mutate(category = "Default")) %>% 
      mutate(modscen=paste(model,"-",scenario)) %>% 
      group_by(region,variable,period) %>% 
      filter(!duplicated(modscen)) %>% 
      ungroup() %>% 
      select(-modscen) %>% 
      mutate(value = value/1000)
    
    # if (i_metric == "pathway") out <- out %>% compute_pathway_allcat()
    # if (i_metric == "grossposCO2emired") out <- out %>% compute_grossCO2emi_allcat()
    # if (i_metric == "grossnegCO2emiinc") out <- out %>% compute_grossnegCO2emi_allcat()
    # if (i_metric == "resCO2emi") out <- out %>% compute_resco2emi_allcat()
    # if (i_metric == "cumnegCO2emi") out <- out %>% compute_cumnegCO2emi_allcat()
  }
  if (i_case == "2C_vs_Delay") {
    out <- v_data_timeTechTemp_world_plot %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                             "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      filter(tempcat %in% c(u_2degcases)) %>% 
      mutate(tempcat = paste(tempcat)) %>% 
      mutate(tempcat = "2°C") %>% 
      filter(allcat %in% c("Default", "Delayed action until 2030")) %>% 
      rename(category=allcat) %>% 
      select(-tempcat,-timingcat,-techcat) %>% 
      rbind(v_data_tempTargets_world_plot %>% 
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::rename(category=tempcat) %>%
              dplyr::mutate(category = paste(category)) %>%
              dplyr::filter(category %in% u_2degcases) %>% 
              mutate(category = "Default")) %>% 
      mutate(modscen=paste(model,"-",scenario)) %>% 
      group_by(region,variable,period) %>% 
      filter(!duplicated(modscen)) %>% 
      ungroup() %>% 
      select(-modscen) %>% 
      mutate(value = value/1000)
    # if (i_metric == "pathway") out <- out %>% compute_pathway_allcat()
    # if (i_metric == "grossposCO2emired") out <- out %>% compute_grossCO2emi_allcat()
    # if (i_metric == "grossnegCO2emiinc") out <- out %>% compute_grossnegCO2emi_allcat()
    # if (i_metric == "resCO2emi") out <- out %>% compute_resco2emi_allcat()
    # if (i_metric == "cumnegCO2emi") out <- out %>% compute_cumnegCO2emi_allcat()
  }
  if (i_case == "2C_vs_SSP5") {
    case_ssp = "5"
    out <- v_data_timeTechTemp_world_plot %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                             "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      filter(tempcat %in% u_2degcases) %>% 
      mutate(tempcat = paste(tempcat)) %>% 
      mutate(tempcat = "2°C") %>% 
      filter(allcat == "Default") %>% 
      mutate(allcat = paste(allcat)) %>% 
      rename(category=allcat) %>% 
      select(-tempcat, -timingcat, -techcat) %>% 
      rbind(v_data_tempTargets_world_plot %>% 
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::rename(category=tempcat) %>%
              dplyr::mutate(category = paste(category)) %>%
              dplyr::filter(category %in% u_2degcases) %>% 
              mutate(category = "Default")) %>% 
      mutate(modscen=paste(model,"-",scenario)) %>% 
      group_by(region,variable,period) %>% 
      filter(!duplicated(modscen)) %>% 
      ungroup() %>% 
      select(-modscen) %>% 
      rbind(v_data_tempTargets_world_plot %>% 
        dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                               "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
        dplyr::mutate(scenario = paste(scenario)) %>% 
        dplyr::filter(scenario %in% c(paste0("SSP",case_ssp, "-26-SPA",case_ssp,"-V16"), paste0("SSP",case_ssp, "-26-SPA",case_ssp,"-V17"))) %>%  
        dplyr::mutate(scenario = ifelse(scenario == paste0("SSP", case_ssp, "-26-SPA",case_ssp,"-V16"), paste0("SSP", case_ssp, "-26-SPA",case_ssp,"-V17"), scenario)) %>% 
          dplyr::select(-tempcat) %>% 
        dplyr::mutate(category = "SSP5")) %>% 
      mutate(value = value/1000) 
    
    # if (i_metric == "pathway") out <- out %>% compute_pathway_allcat()
    # if (i_metric == "grossposCO2emired") out <- out %>% compute_grossCO2emi_allcat()
    # if (i_metric == "grossnegCO2emiinc") out <- out %>% compute_grossnegCO2emi_allcat()
    # if (i_metric == "resCO2emi") out <- out %>% compute_resco2emi_allcat()
    # if (i_metric == "cumnegCO2emi") out <- out %>% compute_cumnegCO2emi_allcat()
      
  }
  if (i_case == "2C_vs_LED") {
    out <- v_data_timeTechTemp_world_plot %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                             "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      filter(tempcat %in% u_2degcases) %>% 
      mutate(tempcat = paste(tempcat)) %>% 
      mutate(tempcat = "2°C") %>% 
      filter(allcat == "Default") %>% 
      mutate(allcat = paste(allcat)) %>% 
      rename(category=allcat) %>% 
      select(-tempcat, -timingcat, -techcat) %>% 
      rbind(v_data_tempTargets_world_plot %>% 
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::rename(category=tempcat) %>%
              dplyr::mutate(category = paste(category)) %>%
              dplyr::filter(category %in% u_2degcases) %>% 
              mutate(category = "Default")) %>% 
      mutate(modscen=paste(model,"-",scenario)) %>% 
      group_by(region,variable,period) %>% 
      filter(!duplicated(modscen)) %>% 
      ungroup() %>% 
      select(-modscen) %>% 
      rbind(v_data_tempTargets_world_plot %>%
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::mutate(scenario = paste(scenario)) %>% 
              dplyr::filter(scenario == "LowEnergyDemand") %>% 
              dplyr::select(-tempcat) %>% 
              dplyr::mutate(category = "LED")) %>% 
      mutate(value = value/1000) 
    
    # if (i_metric == "pathway") out <- out %>% compute_pathway_allcat()
    # if (i_metric == "grossposCO2emired") out <- out %>% compute_grossCO2emi_allcat()
    # if (i_metric == "grossnegCO2emiinc") out <- out %>% compute_grossnegCO2emi_allcat()
    # if (i_metric == "resCO2emi") out <- out %>% compute_resco2emi_allcat()
    # if (i_metric == "cumnegCO2emi") out <- out %>% compute_cumnegCO2emi_allcat()
    
  }
  if (i_case == "2C_vs_Holz") {
    out <- v_data_timeTechTemp_world_plot %>% 
      filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                             "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
      filter(tempcat %in% u_2degcases) %>% 
      mutate(tempcat = paste(tempcat)) %>% 
      mutate(tempcat = "2°C") %>% 
      filter(allcat == "Default") %>% 
      mutate(allcat = paste(allcat)) %>% 
      rename(category=allcat) %>% 
      select(-tempcat, -timingcat, -techcat) %>% 
      rbind(v_data_tempTargets_world_plot %>% 
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::rename(category=tempcat) %>%
              dplyr::mutate(category = paste(category)) %>%
              dplyr::filter(category %in% u_2degcases) %>% 
              mutate(category = "Default")) %>% 
      mutate(modscen=paste(model,"-",scenario)) %>% 
      group_by(region,variable,period) %>% 
      filter(!duplicated(modscen)) %>% 
      ungroup() %>% 
      select(-modscen) %>% 
      rbind(v_data_tempTargets_world_plot %>%
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::mutate(scenario = paste(scenario)) %>% 
              dplyr::filter(scenario == "Ratchet-1.5-noCDR-noOS") %>% 
              dplyr::select(-tempcat) %>% 
              dplyr::mutate(category = "Holz-noCDR-noOS")) %>% 
      rbind(v_data_tempTargets_world_plot %>%
              dplyr::filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                                            "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
              dplyr::mutate(scenario = paste(scenario)) %>% 
              dplyr::filter(scenario == "Ratchet-1.5-allCDR") %>% 
              dplyr::select(-tempcat) %>% 
              dplyr::mutate(category = "Holz-allCDR")) %>% 
      mutate(value = value/1000) 
    
    # if (i_metric == "pathway") out <- out %>% compute_pathway_allcat()
    # if (i_metric == "grossposCO2emired") out <- out %>% compute_grossCO2emi_allcat()
    # if (i_metric == "grossnegCO2emiinc") out <- out %>% compute_grossnegCO2emi_allcat()
    # if (i_metric == "resCO2emi") out <- out %>% compute_resco2emi_allcat()
    # if (i_metric == "cumnegCO2emi") out <- out %>% compute_cumnegCO2emi_allcat()
    
  }
  
  if (i_metric == "pathway")           out <- out %>% compute_pathway()
  if (i_metric == "grossposCO2emired") out <- out %>% compute_grossCO2emi()
  if (i_metric == "grossnegCO2emiinc") out <- out %>% compute_grossnegCO2emi()
  if (i_metric == "resCO2emi")         out <- out %>% compute_resco2emi()
  if (i_metric == "cumnegCO2emi")      out <- out %>% compute_cumnegCO2emi()
  if (i_metric == "cumCO2emi_all")     out <- out %>% compute_cumCO2emi_all()
  
  return(out)
}

plot_initialise <- function(i_xlim, i_ylim) {
  plot(0,0, type="n",
       axes=FALSE,
       xaxs="i", yaxs="i",
       xlim=i_xlim, i_ylim,
       xlab="", ylab="")
}

plot_grid <- function(i_xticks, i_yticks, i_color, XGRID=TRUE, YGRID=TRUE) {
  if (XGRID) for (k in i_yticks[2:(length(i_yticks)-1)]) lines(range(i_xticks), c(k,k), col=i_color)
  if (YGRID) for (k in i_xticks[2:(length(i_xticks)-1)]) lines(c(k,k), range(i_yticks), col=i_color)  
}

plot_emissions_gross_positive <- function(i_data, i_category, i_value, i_color, i_border) {
  polygon(
    x = c(    i_data$period[which(i_data[[i_category]] == i_value              & i_data$variable == "Emissions|CO2")], 
              rev(i_data$period[which(i_data[[i_category]] == i_value          & i_data$variable == "Emissions|CO2")])),
    y = c(    rep(0.0, length(i_data$med[which(i_data[[i_category]] == i_value & i_data$variable == "Emissions|CO2|Fossil Fuels and Industry|Gross")])), 
              rev(i_data$med[which(i_data[[i_category]] == i_value             & i_data$variable == "Emissions|CO2|Fossil Fuels and Industry|Gross")])),
    col = i_color, border=i_border, lty=3
  )
}

plot_emissions_gross_negative <- function(i_data, i_category, i_value, i_color, i_border) {
  polygon(
    x = c(    i_data$period[which(i_data[[i_category]] == i_value              & i_data$variable == "Emissions|CO2")], 
              rev(i_data$period[which(i_data[[i_category]] == i_value          & i_data$variable == "Emissions|CO2")])),
    y = c(    rep(0.0, length(i_data$med[which(i_data[[i_category]] == i_value & i_data$variable == "Emissions|CO2|Carbon Capture and Storage|Biomass")])), 
              rev(i_data$med[which(i_data[[i_category]] == i_value             & i_data$variable == "Emissions|CO2|Carbon Capture and Storage|Biomass")])),
    col = i_color, border=i_border, lty=3
  )
}

plot_emissions_net <- function(i_data, i_category, i_value, i_color) {
  lines(
    i_data$period[which(i_data[[i_category]] == i_value & i_data$variable == "Emissions|CO2")],
    i_data$med[which(i_data[[i_category]] == i_value    & i_data$variable == "Emissions|CO2")],
    col=i_color, lwd=2
  )
  points(
    i_data$period[which(i_data[[i_category]] == i_value & i_data$variable == "Emissions|CO2")],
    i_data$med[which(i_data[[i_category]] == i_value    & i_data$variable == "Emissions|CO2")],
    col=i_color, bg="#ffffffff", pch=21
  )
}

plot_emissions_finalise <- function() {
  # 0 line
  lines(c(2005,2100), c(0, 0), col="red", lty=2)
  
  # Axes
  axis(1)
  axis(2)
  
  # Box
  box()
}

plot_emission_pathways <- function(i_data, i_category, i_value, i_color, i_border) {

  # Gross positive emissions
  plot_emissions_gross_positive(i_data, i_category, i_value, i_color, i_border)
  # Gross negative emissions
  plot_emissions_gross_negative(i_data, i_category, i_value, i_color, i_border)
  # Net Emissions
  plot_emissions_net(i_data, i_category, i_value, i_border)
  
}

plot_stat_initialise <- function(i_nbcases, i_ylim) {
  plot(0,0, type="n",
       xaxs="i", yaxs="i",
       xlim=c(0.5,i_nbcases+0.5), ylim=i_ylim,
       xlab="", ylab="", axes=F)  
}

plot_stat_boxplot <- function(i_position, i_data, i_category, i_case, i_color, OFFSET=0.45) {

  lines(c(i_position, i_position), c(i_data$min[which(i_data[[i_category]] == i_case)], i_data$max[which(i_data[[i_category]] == i_case)]), col=i_color)
  rect(
    i_position-OFFSET, i_data$p15[which(i_data[[i_category]] == i_case)], 
    i_position+OFFSET, i_data$p85[which(i_data[[i_category]] == i_case)], 
    col=i_color, border=NA
  )
  lines(c(i_position-OFFSET, i_position+OFFSET), c(i_data$med[which(i_data[[i_category]] == i_case)], i_data$med[which(i_data[[i_category]] == i_case)]), col="#ffffff")
  points(c(i_position, i_position), c(i_data$mean[which(i_data[[i_category]] == i_case)], i_data$mean[which(i_data[[i_category]] == i_case)]), pch=21, col=i_color, bg="#ffffff")
}

plot_stat_finalise <- function() {
  axis(2)
  
  box()  
}

plot_stackstat_boxplot <- function(i_position, i_data, i_category, i_case, OFFSET1=0.45, OFFSET2=0.49) {
  
  # Positive part
  # Net Land-use emissions
  rect(
    i_position-OFFSET1, 0.0,
    i_position+OFFSET1, i_data$med[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Land Use|Cumulative")],
    col="#008000ff", border=NA)
  # Gross FFI
  rect(
    i_position-OFFSET1, i_data$med[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Land Use|Cumulative")],
    i_position+OFFSET1, i_data$med[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Land Use|Cumulative")] +
      i_data$med[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative")],
    col="#424242ff", border=NA)
  # Negative part
  # Gross BECCS
  rect(
    i_position-OFFSET1, 0.0,
    i_position+OFFSET1, i_data$med[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative")],
    col="#a02c2cff", border=NA)
  # 
  rect(
    i_position-OFFSET2, i_data$p15[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Cumulative")],
    i_position+OFFSET2, i_data$p85[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Cumulative")],
    col="#2270d43c", border=NA
  )
  lines(
    c(i_position-OFFSET2, i_position+OFFSET2),
    c(
      i_data$med[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Cumulative")],
      i_data$med[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Cumulative")]
    ),
    col="#000000"
  )
  points(
    c(i_position, i_position),
    c(
      i_data$mean[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Cumulative")],
      i_data$mean[which(i_data[[i_category]] == i_case & i_data$period == "2011-2100" & i_data$variable == "Emissions|CO2|Cumulative")]
    ),
    pch=21, bg="#ffffff", col="#000000"
  )  
}

#==== Plot data ===============

#---- Panel A: Emission pathways ----------------------
data_plot_2C            <- get_data("2C", "pathway")
data_plot_2C_vs_15C     <- get_data("2C_vs_15C", "pathway")
data_plot_2C_vs_NoBECCS <- get_data("2C_vs_NoBECCS", "pathway")
data_plot_2C_vs_Delay   <- get_data("2C_vs_Delay", "pathway")
data_plot_2C_vs_SSP5    <- get_data("2C_vs_SSP5", "pathway")
data_plot_2C_vs_LED     <- get_data("2C_vs_LED", "pathway")
data_plot_2C_vs_Holz    <- get_data("2C_vs_Holz", "pathway")
#par(plt=c(0.10,0.50,0.15,0.95), las=1)
par(las=1, mfrow=c(2,3))

#---- 2°C pathways -------------
plot_initialise(c(2005, 2100), c(-25, 50))
plot_grid(u_tick_x, u_tick_y, "#eeeeeeff")
plot_emission_pathways(data_plot_2C, "category", "2°C", u_col_case$`2C`[2], u_col_case$`2C`[1])
plot_emissions_finalise()

#---- 2°C vs 1.5°C pathways -------------
plot_initialise(c(2005, 2100), c(-25, 50))
plot_grid(u_tick_x, u_tick_y, "#eeeeeeff")
plot_emission_pathways(data_plot_2C_vs_15C, "category", "2°C", u_col_case$`2C`[2], u_col_case$`2C`[1])
plot_emission_pathways(data_plot_2C_vs_15C, "category", "1.5°C", u_col_case$`15C`[2], u_col_case$`15C`[1])
plot_emissions_finalise()

#---- 2°C vs 2°C-No BECCS pathways -------------
plot_initialise(c(2005, 2100), c(-25, 50))
plot_grid(u_tick_x, u_tick_y, "#eeeeeeff")
plot_emission_pathways(data_plot_2C_vs_NoBECCS, "category", "Default", u_col_case$`2C`[2], u_col_case$`2C`[1])
plot_emission_pathways(data_plot_2C_vs_NoBECCS, "category", "No CCS/BECCS", u_col_case$`NoBECCS`[2], u_col_case$`NoBECCS`[1])
plot_emissions_finalise()

#---- 2°C vs 2°C-delay pathways -------------
plot_initialise(c(2005, 2100), c(-25, 50))
plot_grid(u_tick_x, u_tick_y, "#eeeeeeff")
plot_emission_pathways(data_plot_2C_vs_Delay, "category", "Default", u_col_case$`2C`[2], u_col_case$`2C`[1])
plot_emission_pathways(data_plot_2C_vs_Delay, "category", "Delayed action until 2030", u_col_case$`Delay`[2], u_col_case$`Delay`[1])
plot_emissions_finalise()

# #---- 2°C vs 2°C-SSP5 pathways -------------
# plot_initialise(c(2005, 2100), c(-25, 50))
# plot_grid(u_tick_x, u_tick_y, "#eeeeeeff")
# plot_emission_pathways(data_plot_2C_vs_SSP5, "category", "Default", u_col_case$`2C`[2], u_col_case$`2C`[1])
# plot_emission_pathways(data_plot_2C_vs_SSP5, "category", "SSP5", u_col_case$`SSP5`[2], u_col_case$`SSP5`[1])
# plot_emissions_finalise()

# #---- 2°C vs 1.5-LED pathway -------------
# plot_initialise(c(2005, 2100), c(-25, 50))
# plot_grid(u_tick_x, u_tick_y, "#eeeeeeff")
# plot_emission_pathways(data_plot_2C_vs_LED, "category", "Default", u_col_case$`2C`[2], u_col_case$`2C`[1])
# plot_emission_pathways(data_plot_2C_vs_LED, "category", "LED", u_col_case$`LED`[2], u_col_case$`LED`[1])
# plot_emissions_finalise()

#---- 2°C vs 1.5-Holz pathway -------------
plot_initialise(c(2005, 2100), c(-25, 50))
plot_grid(u_tick_x, u_tick_y, "#eeeeeeff")
plot_emission_pathways(data_plot_2C_vs_Holz, "category", "Default", u_col_case$`2C`[2], u_col_case$`2C`[1])
plot_emission_pathways(data_plot_2C_vs_Holz, "category", "Holz-noCDR-noOS", u_col_case$`LED`[2], u_col_case$`LED`[1])
plot_emissions_finalise()

#---- 2°C vs 1.5-Holz pathway -------------
plot_initialise(c(2005, 2100), c(-25, 50))
plot_grid(u_tick_x, u_tick_y, "#eeeeeeff")
plot_emission_pathways(data_plot_2C_vs_Holz, "category", "Default", u_col_case$`2C`[2], u_col_case$`2C`[1])
plot_emission_pathways(data_plot_2C_vs_Holz, "category", "Holz-allCDR", u_col_case$`LED`[2], u_col_case$`LED`[1])
plot_emissions_finalise()


#---- Panel B: Key metrics and stats ----------------------
#---- Gross positive CO2 emission reductions -------------
data_plot_2C_vs_15C     <- get_data("2C_vs_15C", "grossposCO2emired")
data_plot_2C_vs_NoBECCS <- get_data("2C_vs_NoBECCS", "grossposCO2emired")
data_plot_2C_vs_Delay   <- get_data("2C_vs_Delay", "grossposCO2emired")
data_plot_2C_vs_SSP5    <- get_data("2C_vs_SSP5", "grossposCO2emired")
data_plot_2C_vs_LED     <- get_data("2C_vs_LED", "grossposCO2emired")
par(las=1, mfrow=c(1,1), plt=c(0.20,0.95,0.1,0.9))
plot_stat_initialise(5, c(0.0, 2.0))
plot_stat_boxplot(1, data_plot_2C_vs_15C, "category", "2°C", u_col_case$`2C`[1])
plot_stat_boxplot(2, data_plot_2C_vs_15C, "category", "1.5°C", u_col_case$`15C`[1])
plot_stat_boxplot(3, data_plot_2C_vs_NoBECCS, "category", "No CCS/BECCS", u_col_case$`NoBECCS`[1])
plot_stat_boxplot(4, data_plot_2C_vs_Delay, "category", "Delayed action until 2030", u_col_case$`Delay`[1])
plot_stat_boxplot(5, data_plot_2C_vs_SSP5, "category", "SSP5", u_col_case$`SSP5`[1])
#plot_stat_boxplot(6, data_plot_2C_vs_SSP5, "category", "LED", u_col_case$`LED`[1])
plot_stat_finalise()

#---- Gross negative CO2 emission increase -------------
data_plot_2C_vs_15C     <- get_data("2C_vs_15C", "grossnegCO2emiinc")
data_plot_2C_vs_NoBECCS <- get_data("2C_vs_NoBECCS", "grossnegCO2emiinc")
data_plot_2C_vs_Delay   <- get_data("2C_vs_Delay", "grossnegCO2emiinc")
data_plot_2C_vs_SSP5    <- get_data("2C_vs_SSP5", "grossnegCO2emiinc")
par(las=1, mfrow=c(1,1), plt=c(0.20,0.95,0.1,0.9))
plot_stat_initialise(5, c(0.0, 2.0))
plot_stat_boxplot(1, data_plot_2C_vs_15C, "category", "2°C", u_col_case$`2C`[1])
plot_stat_boxplot(2, data_plot_2C_vs_15C, "category", "1.5°C", u_col_case$`15C`[1])
plot_stat_boxplot(3, data_plot_2C_vs_NoBECCS, "category", "No CCS/BECCS", u_col_case$`NoBECCS`[1])
plot_stat_boxplot(4, data_plot_2C_vs_Delay, "category", "Delayed action until 2030", u_col_case$`Delay`[1])
plot_stat_boxplot(5, data_plot_2C_vs_SSP5, "category", "SSP5", u_col_case$`SSP5`[1])
plot_stat_finalise()

#---- Residual CO2 emissions -------------
data_plot_2C_vs_15C     <- get_data("2C_vs_15C", "resCO2emi")
data_plot_2C_vs_NoBECCS <- get_data("2C_vs_NoBECCS", "resCO2emi")
data_plot_2C_vs_Delay   <- get_data("2C_vs_Delay", "resCO2emi")
data_plot_2C_vs_SSP5    <- get_data("2C_vs_SSP5", "resCO2emi")
par(las=1, mfrow=c(1,1), plt=c(0.20,0.95,0.1,0.9))
plot_stat_initialise(5, c(0.0, 15))
plot_stat_boxplot(1, data_plot_2C_vs_15C, "category", "2°C", u_col_case$`2C`[1])
plot_stat_boxplot(2, data_plot_2C_vs_15C, "category", "1.5°C", u_col_case$`15C`[1])
plot_stat_boxplot(3, data_plot_2C_vs_NoBECCS, "category", "No CCS/BECCS", u_col_case$`NoBECCS`[1])
plot_stat_boxplot(4, data_plot_2C_vs_Delay, "category", "Delayed action until 2030", u_col_case$`Delay`[1])
plot_stat_boxplot(5, data_plot_2C_vs_SSP5, "category", "SSP5", u_col_case$`SSP5`[1])
plot_stat_finalise()

#---- Cumulative negative CO2 emission -------------
data_plot_2C_vs_15C     <- get_data("2C_vs_15C", "cumnegCO2emi")
data_plot_2C_vs_NoBECCS <- get_data("2C_vs_NoBECCS", "cumnegCO2emi")
data_plot_2C_vs_Delay   <- get_data("2C_vs_Delay", "cumnegCO2emi")
data_plot_2C_vs_SSP5    <- get_data("2C_vs_SSP5", "cumnegCO2emi")
par(las=1, mfrow=c(1,1), plt=c(0.20,0.95,0.1,0.9))
plot_stat_initialise(5, c(0.0, 1200))
plot_stat_boxplot(1, data_plot_2C_vs_15C, "category", "2°C", u_col_case$`2C`[1])
plot_stat_boxplot(2, data_plot_2C_vs_15C, "category", "1.5°C", u_col_case$`15C`[1])
plot_stat_boxplot(3, data_plot_2C_vs_NoBECCS, "category", "No CCS/BECCS", u_col_case$`NoBECCS`[1])
plot_stat_boxplot(4, data_plot_2C_vs_Delay, "category", "Delayed action until 2030", u_col_case$`Delay`[1])
plot_stat_boxplot(5, data_plot_2C_vs_SSP5, "category", "SSP5", u_col_case$`SSP5`[1])
plot_stat_finalise()

#---- Cumulative negative CO2 emission -------------
data_plot_2C_vs_15C     <- get_data("2C_vs_15C", "cumCO2emi_all")
data_plot_2C_vs_NoBECCS <- get_data("2C_vs_NoBECCS", "cumCO2emi_all")
data_plot_2C_vs_Delay   <- get_data("2C_vs_Delay", "cumCO2emi_all")
data_plot_2C_vs_SSP5    <- get_data("2C_vs_SSP5", "cumCO2emi_all")
par(las=1, mfrow=c(1,1), plt=c(0.20,0.95,0.1,0.9))
plot_stat_initialise(5, c(-1000.0, 2500))
plot_stackstat_boxplot(1, data_plot_2C_vs_15C, "category", "2°C")
plot_stackstat_boxplot(2, data_plot_2C_vs_15C, "category", "1.5°C")
plot_stackstat_boxplot(3, data_plot_2C_vs_NoBECCS, "category", "No CCS/BECCS")
plot_stackstat_boxplot(4, data_plot_2C_vs_Delay, "category", "Delayed action until 2030")
plot_stackstat_boxplot(5, data_plot_2C_vs_SSP5, "category", "SSP5")
plot_stat_finalise()
