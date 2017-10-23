p_sresCorrector <- 0
p_rcpCorrector  <- 0
dat_ar5_corr    <- 0

#== INITIALISE ============================
# Load libraries
library(readxl)
library(ar5data)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load functions
dump <- lapply(list.files(path="functions", pattern=".R", full.names=TRUE), source)


#== READ IN DATA ==========================
# CDIAC
source("scripts/readin_cdiac.R")
# SSP historic
source("scripts/readin_historicData.R")
# IS92
source("scripts/readin_is92.R")
# SRES
source("scripts/readin_sres.R")
# RCP
source("scripts/readin_rcp.R")
# AR5
dat_ar5 = ar5data %>% 
  mutate(period = as.numeric(format(period, "%Y"))) %>%
  filter(period %in% c(2005, seq(2010, 2100, 10)))
# Rogelj et al 2015
# SSP
dat_ssp <- read.csv("SSP_IAM_World_5Regs_2015-12-03.csv")

# Rogelj et al 2017


#== PLOT DATA =============================
#-- Total CO2 emissions -----------
p <- ggplot() +
  #--- CDIAC 
  geom_line(aes(x=period, y=value*1e-3), data=dat_cdiac %>% filter(variable == "total"), size=1) +
  #--- SSP historical data
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "IEA"), size=1) +
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "RCP"), size=1) +
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "MAGICC6"), size=1) +
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "EDGAR"), size=1) +
  geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "CDIAC"), size=1) +
  #--- IS92 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_is92 %>% 
                dplyr::filter(region == "World", variable == "CO2") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="IS92 (1992)"),
              fill="#ffaaaa66") +
  #--- SRES scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_sres %>% 
                dplyr::mutate(value = value + p_sresCorrector) %>% 
                dplyr::filter(region == "World", variable == "Anthropogenic Emissions (standardized)|TotalCO2") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="SRES (2000)"),
              fill="#aaaaff66") +
  #--- RCP scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_rcp %>% 
                dplyr::mutate(value = value + p_rcpCorrector) %>% 
                dplyr::filter(region == "World", variable == "CO2 emissions - Total") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="RCP (2009)"),
              fill="#aaffaa99") +
  geom_line(aes(x=period, y=value, group=model), 
            data=dat_rcp %>% 
              dplyr::mutate(value = value + p_rcpCorrector) %>% 
              dplyr::filter(region == "World", variable == "CO2 emissions - Total") %>% 
              dplyr::select(-unit, -region, -variable) %>% 
              dplyr::mutate(project="RCP (2009)")) +
  #--- AR5 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa33") +
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_ar5_corr %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="AR5"),
  #             fill="#aaaaaa66") +
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa99") +
  # #--- SSP scenarios
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_SSP %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="SSP"),
  #             fill="#aaaaaa33") +
# geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
#             data=dat_SSP %>% 
#               dplyr::mutate(value = value/1000*12/44) %>% 
#               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
#               dplyr::select(-unit, -region, -variable) %>% 
#               dplyr::group_by(period) %>% 
#               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
#               dplyr::ungroup() %>% 
#               dplyr::mutate(project="SSP"),
#             fill="#aaaaaa66") +
# geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
#             data=dat_SSP %>% 
#               dplyr::mutate(value = value/1000*12/44) %>% 
#               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
#               dplyr::select(-unit, -region, -variable) %>% 
#               dplyr::group_by(period) %>% 
#               dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
#               dplyr::ungroup() %>% 
#               dplyr::mutate(project="SSP"),
#             fill="#aaaaaa99") +
geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax), 
             data=data.frame(
               project = c("IS92 (1992)", "SRES (2000)", "RCP (2009)", "AR5 (2015)"),
               xmin    = rep(2000,4), 
               xmax    = rep(2100,4),
               ymin    = rep(0,4),
               ymax    = rep(0,4)
             ),
             color="red", linetype=2, lwd=1.1) +
  facet_wrap(~project, ncol=2) +
  theme_bw() +
  xlab("") +
  ylab("CO2 emissions [Gt C]") +
  ggtitle("Total CO2 emissions") +
  xlim(c(2000,2100)) +
  ylim(c(-5,45))

print(p)

#-- Fossil fuels and Industry CO2 emissions -----------
p <- ggplot() +
  #--- CDIAC 
  geom_line(aes(x=period, y=value*1e-3), data=dat_cdiac %>% filter(variable == "total"), size=1) +
  #--- SSP historical data
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "IEA"), size=1) +
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "RCP"), size=1) +
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "MAGICC6"), size=1) +
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "EDGAR"), size=1) +
  geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "CDIAC"), size=1) +
  #--- IS92 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
            data=dat_is92 %>% 
              dplyr::filter(region == "World", variable == "Emissions from Combustion|CO2") %>% 
              dplyr::select(-unit, -region, -variable) %>% 
              dplyr::group_by(period) %>% 
              dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
              dplyr::ungroup() %>% 
              dplyr::mutate(project="IS92 (1992)"),
            fill="#ffaaaa66") +
  #--- SRES scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_sres %>% 
                dplyr::mutate(value = value + p_sresCorrector) %>% 
                dplyr::filter(region == "World", variable == "Anthropogenic Emissions (standardized)|FossilFuelCO2") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="SRES (2000)"),
              fill="#aaaaff66") +
  #--- RCP scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_rcp %>% 
                dplyr::mutate(value = value + p_rcpCorrector) %>% 
                dplyr::filter(region == "World", variable == "CO2 emissions - Fossil fuels and Industry") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="RCP (2009)"),
              fill="#aaffaa99") +
  geom_line(aes(x=period, y=value, group=model), 
            data=dat_rcp %>% 
              dplyr::mutate(value = value + p_rcpCorrector) %>% 
              dplyr::filter(region == "World", variable == "CO2 emissions - Fossil fuels and Industry") %>% 
              dplyr::select(-unit, -region, -variable) %>% 
              dplyr::mutate(project="RCP (2009)")) +
  #--- AR5 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa33") +
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_ar5_corr %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="AR5"),
  #             fill="#aaaaaa66") +
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa99") +
  # #--- SSP scenarios
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_SSP %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="SSP"),
  #             fill="#aaaaaa33") +
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_SSP %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="SSP"),
  #             fill="#aaaaaa66") +
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_SSP %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="SSP"),
  #             fill="#aaaaaa99") +
  geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax), 
               data=data.frame(
                 project = c("IS92 (1992)", "SRES (2000)", "RCP (2009)", "AR5 (2015)"),
                 xmin    = rep(2000,4), 
                 xmax    = rep(2100,4),
                 ymin    = rep(0,4),
                 ymax    = rep(0,4)
               ),
               color="red", linetype=2, lwd=1.1) +
  facet_wrap(~project, ncol=2) +
  theme_bw() +
  xlab("") +
  ylab("CO2 emissions [Gt C]") +
  ggtitle("CO2 emissions from FF&I") +
  xlim(c(2000,2100)) +
  ylim(c(-5,45))
  
print(p)




p <- ggplot() +
  #--- IS92 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_is92 %>% 
                dplyr::filter(region == "World", variable == "CO2 from Deforestation") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="IS92 (1992)"),
              fill="#ffaaaa66") +
  #--- SRES scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_sres %>% 
                dplyr::mutate(value = value + p_sresCorrector) %>% 
                dplyr::filter(region == "World", variable == "Carbon Sequestraction") %>% 
                dplyr::mutate(value = ifelse(value>0, -value, value)) %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="SRES (2000)"),
              fill="#aaaaff66") +
  #--- RCP scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_rcp %>% 
                dplyr::mutate(value = value + p_rcpCorrector) %>% 
                dplyr::filter(region == "World", variable == "CO2 emissions - Fossil fuels and Industry") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="RCP (2009)"),
              fill="#aaffaa99") +
  geom_line(aes(x=period, y=value, group=model), 
            data=dat_rcp %>% 
              dplyr::mutate(value = value + p_rcpCorrector) %>% 
              dplyr::filter(region == "World", variable == "CO2 emissions - Fossil fuels and Industry") %>% 
              dplyr::select(-unit, -region, -variable) %>% 
              dplyr::mutate(project="RCP (2009)")) +
  #--- AR5 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa33") +
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_ar5_corr %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="AR5"),
  #             fill="#aaaaaa66") +
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa99") +
  # #--- SSP scenarios
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_SSP %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="SSP"),
  #             fill="#aaaaaa33") +
# geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
#             data=dat_SSP %>% 
#               dplyr::mutate(value = value/1000*12/44) %>% 
#               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
#               dplyr::select(-unit, -region, -variable) %>% 
#               dplyr::group_by(period) %>% 
#               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
#               dplyr::ungroup() %>% 
#               dplyr::mutate(project="SSP"),
#             fill="#aaaaaa66") +
# geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
#             data=dat_SSP %>% 
#               dplyr::mutate(value = value/1000*12/44) %>% 
#               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
#               dplyr::select(-unit, -region, -variable) %>% 
#               dplyr::group_by(period) %>% 
#               dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
#               dplyr::ungroup() %>% 
#               dplyr::mutate(project="SSP"),
#             fill="#aaaaaa99") +
geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax), 
             data=data.frame(
               project = c("IS92 (1992)", "SRES (2000)", "RCP (2009)", "AR5 (2015)"),
               xmin    = rep(2000,4), 
               xmax    = rep(2100,4),
               ymin    = rep(0,4),
               ymax    = rep(0,4)
             ),
             color="red", linetype=2, lwd=1.1) +
  facet_wrap(~project, ncol=2) +
  theme_bw() +
  xlab("") +
  ylab("CO2 emissions [Gt C]") +
  ggtitle("CO2 emissions from FF&I") +
  xlim(c(2000,2100)) +
  ylim(c(-5,45))

print(p)



#-- Fossil fuels and Industry CO2 emissions -----------
p <- ggplot() +
  #--- CDIAC 
  geom_line(aes(x=period, y=value*1e-3), data=dat_cdiac %>% filter(variable == "total"), size=1) +
  #--- SSP historical data
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "IEA"), size=1) +
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "RCP"), size=1) +
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "MAGICC6"), size=1) +
  #geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "EDGAR"), size=1) +
  geom_line(aes(x=period, y=value*1e-3*12/44), data=dat_histSSP %>% filter(variable == "Emissions|CO2|Fossil Fuels and Industry", region == "World", scenario == "CDIAC"), size=1) +
  #--- IS92 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_is92 %>% 
                dplyr::filter(region == "World", variable == "Emissions from Combustion|CO2") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="IS92 (1992)"),
              fill="#ffaaaa66") +
  #--- SRES scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_sres %>% 
                dplyr::mutate(value = value + p_sresCorrector) %>% 
                dplyr::filter(region == "World", variable == "Anthropogenic Emissions (standardized)|FossilFuelCO2") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="SRES (2000)"),
              fill="#aaaaff66") +
  #--- RCP scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_rcp %>% 
                dplyr::mutate(value = value + p_rcpCorrector) %>% 
                dplyr::filter(region == "World", variable == "CO2 emissions - Fossil fuels and Industry") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="RCP (2009)"),
              fill="#aaffaa99") +
  geom_line(aes(x=period, y=value, group=model), 
            data=dat_rcp %>% 
              dplyr::mutate(value = value + p_rcpCorrector) %>% 
              dplyr::filter(region == "World", variable == "CO2 emissions - Fossil fuels and Industry") %>% 
              dplyr::select(-unit, -region, -variable) %>% 
              dplyr::mutate(project="RCP (2009)")) +
  #--- AR5 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa33") +
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_ar5_corr %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="AR5"),
  #             fill="#aaaaaa66") +
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa99") +
  # #--- SSP scenarios
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_SSP %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="SSP"),
  #             fill="#aaaaaa33") +
# geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
#             data=dat_SSP %>% 
#               dplyr::mutate(value = value/1000*12/44) %>% 
#               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
#               dplyr::select(-unit, -region, -variable) %>% 
#               dplyr::group_by(period) %>% 
#               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
#               dplyr::ungroup() %>% 
#               dplyr::mutate(project="SSP"),
#             fill="#aaaaaa66") +
# geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
#             data=dat_SSP %>% 
#               dplyr::mutate(value = value/1000*12/44) %>% 
#               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
#               dplyr::select(-unit, -region, -variable) %>% 
#               dplyr::group_by(period) %>% 
#               dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
#               dplyr::ungroup() %>% 
#               dplyr::mutate(project="SSP"),
#             fill="#aaaaaa99") +
geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax), 
             data=data.frame(
               project = c("IS92 (1992)", "SRES (2000)", "RCP (2009)", "AR5 (2015)"),
               xmin    = rep(2000,4), 
               xmax    = rep(2100,4),
               ymin    = rep(0,4),
               ymax    = rep(0,4)
             ),
             color="red", linetype=2, lwd=1.1) +
  facet_wrap(~project, ncol=2) +
  theme_bw() +
  xlab("") +
  ylab("CO2 emissions [Gt C]") +
  ggtitle("CO2 emissions from FF&I") +
  xlim(c(2000,2100)) +
  ylim(c(-5,45))

print(p)



#-- Land Use CO2 emissions -----------
p <- ggplot() +
  #--- IS92 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_is92 %>% 
                dplyr::filter(region == "World", variable == "CO2 from Deforestation") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="IS92 (1992)"),
              fill="#ffaaaa66") +
  #--- SRES scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_sres %>% 
                dplyr::mutate(value = value + p_sresCorrector) %>% 
                dplyr::filter(region == "World", variable == "Carbon Sequestraction") %>% 
                dplyr::mutate(value = ifelse(value>0, -value, value)) %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="SRES (2000)"),
              fill="#aaaaff66") +
  #--- RCP scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_rcp %>% 
                dplyr::mutate(value = value + p_rcpCorrector) %>% 
                dplyr::filter(region == "World", variable == "CO2 emissions - Land-use change") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="RCP (2009)"),
              fill="#aaffaa99") +
  geom_line(aes(x=period, y=value, group=model), 
            data=dat_rcp %>% 
              dplyr::mutate(value = value + p_rcpCorrector) %>% 
              dplyr::filter(region == "World", variable == "CO2 emissions - Land-use change") %>% 
              dplyr::select(-unit, -region, -variable) %>% 
              dplyr::mutate(project="RCP (2009)")) +
  #--- AR5 scenarios
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2|Land Use") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa33") +
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_ar5_corr %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry") %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="AR5"),
  #             fill="#aaaaaa66") +
  geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
              data=dat_ar5 %>% 
                P0() %>% 
                dplyr::mutate(value = value/1000*12/44) %>% 
                dplyr::filter(region == "World", variable == "Emissions|CO2|Land Use") %>% 
                dplyr::select(-unit, -region, -variable) %>% 
                dplyr::group_by(period) %>% 
                dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(project="AR5 (2015)"),
              fill="#aaaaaa99") +
  # #--- SSP scenarios
  # geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
  #             data=dat_SSP %>% 
  #               dplyr::mutate(value = value/1000*12/44) %>% 
  #               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
  #               dplyr::select(-unit, -region, -variable) %>% 
  #               dplyr::group_by(period) %>% 
  #               dplyr::summarise(valmin = min(value, na.rm=TRUE), valmax=max(value, na.rm=TRUE)) %>% 
  #               dplyr::ungroup() %>% 
  #               dplyr::mutate(project="SSP"),
  #             fill="#aaaaaa33") +
# geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
#             data=dat_SSP %>% 
#               dplyr::mutate(value = value/1000*12/44) %>% 
#               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
#               dplyr::select(-unit, -region, -variable) %>% 
#               dplyr::group_by(period) %>% 
#               dplyr::summarise(valmin = quantile(value, 0.05, na.rm=TRUE), valmax=quantile(value, 0.95, na.rm=TRUE)) %>% 
#               dplyr::ungroup() %>% 
#               dplyr::mutate(project="SSP"),
#             fill="#aaaaaa66") +
# geom_ribbon(aes(x=period, ymin=valmin, ymax=valmax), 
#             data=dat_SSP %>% 
#               dplyr::mutate(value = value/1000*12/44) %>% 
#               dplyr::filter(region == "World", variable == "Emissions|CO2|Fossil Fuels and Industry", grepl("Ref", scenario)) %>% 
#               dplyr::select(-unit, -region, -variable) %>% 
#               dplyr::group_by(period) %>% 
#               dplyr::summarise(valmin = quantile(value, 0.25, na.rm=TRUE), valmax=quantile(value, 0.75, na.rm=TRUE)) %>% 
#               dplyr::ungroup() %>% 
#               dplyr::mutate(project="SSP"),
#             fill="#aaaaaa99") +
geom_segment(aes(x=xmin, xend=xmax, y=ymin, yend=ymax), 
             data=data.frame(
               project = c("IS92 (1992)", "SRES (2000)", "RCP (2009)", "AR5 (2015)"),
               xmin    = rep(2000,4), 
               xmax    = rep(2100,4),
               ymin    = rep(0,4),
               ymax    = rep(0,4)
             ),
             color="red", linetype=2, lwd=1.1) +
  facet_wrap(~project, ncol=2) +
  theme_bw() +
  xlab("") +
  ylab("CO2 emissions [Gt C]") +
  ggtitle("CO2 emissions from Land-Use changes") +
  xlim(c(2000,2100)) +
  ylim(c(-10,5))

print(p)