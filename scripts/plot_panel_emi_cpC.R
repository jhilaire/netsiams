f_grid_x = TRUE
f_grid_y = TRUE

u_tick_x <- seq(2010,2100,10)
u_tick_y <- seq(-20,50,10)

u_col_case1 <- "#3f91c5"
u_col_case2 <- RColorBrewer::brewer.pal(10, "Set1")[5]


#svg size 976 x 320 pixels

# #==== Emission pathways ========================================
# #---- Emission pathways: data ----------------------------------
# data_plot <- v_data_tempTargets_world_plot %>% 
#   filter(
#     #period >= 2010,
#     variable == "Emissions|CO2",
#     tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
#   mutate(tempcat = paste(tempcat)) %>% 
#   mutate(tempcat = ifelse(tempcat == "1.5°C scenario", "1.5°C", "2°C")) %>% 
#   #filter(allcat == "Default") %>% 
#   mutate(value=value/1000) %>% 
#   group_by(tempcat, period) %>% 
#   summarise(
#     min=min(value, na.rm=T),
#     p01=quantile(value, 0.01, na.rm=T),
#     p05=quantile(value, 0.05, na.rm=T),
#     p10=quantile(value, 0.10, na.rm=T),
#     p15=quantile(value, 0.15, na.rm=T),
#     mean=mean(value, na.rm=T),
#     med=median(value, na.rm=T),
#     p85=quantile(value, 0.85, na.rm=T),
#     p90=quantile(value, 0.90, na.rm=T),
#     p95=quantile(value, 0.95, na.rm=T),
#     p99=quantile(value, 0.99, na.rm=T),
#     max=max(value, na.rm=T)
#   ) %>% 
#   ungroup()
# ar5data_plot <- ar5data %>% 
#   filter(region == "World", variable == "Emissions|CO2") %>% 
#   mutate(period=as.numeric(format(period, "%Y"))) %>% 
#   filter(period %in% c(2005, seq(2010,2100,10))) %>% 
#   ar5join()  %>% 
#   mutate(value = value/1000) %>% 
#   group_by(period) %>% 
#   summarise(
#     min=min(value, na.rm=T),
#     p01=quantile(value, 0.01, na.rm=T),
#     p05=quantile(value, 0.05, na.rm=T),
#     p10=quantile(value, 0.10, na.rm=T),
#     p15=quantile(value, 0.15, na.rm=T),
#     mean=mean(value, na.rm=T),
#     med=median(value, na.rm=T),
#     p85=quantile(value, 0.85, na.rm=T),
#     p90=quantile(value, 0.90, na.rm=T),
#     p95=quantile(value, 0.95, na.rm=T),
#     p99=quantile(value, 0.99, na.rm=T),
#     max=max(value, na.rm=T)
#   ) %>% 
#   ungroup()
#   
# 
#   
# #---- Emission pathways: plot ----------------------------------
# par(plt=c(0.10,0.50,0.15,0.95), las=1)
# plot(0,0, type="n",
#      xaxs="i", yaxs="i",
#      xlim=c(2005,2100), ylim=c(-20,50),
#      xlab="", ylab="")
# 
# # Grid
# if (f_grid_x) for (k in u_tick_y) lines(c(2005,2100), c(k,k), col="#eeeeee")
# if (f_grid_y) for (k in u_tick_x) lines(c(k,k), c(-20,50), col="#eeeeee")
# 
# # 2030-2050 zone
# rect(
#   2030, -20,
#   2050, 50,
#   col="#cccccc99", border=NA
# )
# lines(c(2030,2030), c(-20, 50), lty=2)
# lines(c(2050,2050), c(-20, 50), lty=2)
# 
# # Data
# # AR5 data:
# polygon(
#   x = c(ar5data_plot$period, rev(ar5data_plot$period)),
#   y = c(ar5data_plot$p15, ar5data_plot$p85),
#   col="#cccccc33", border=NA
# )
# # Case 1: 2°C
# cur_case = "2°C"
# polygon(
#   x = c(    data_plot$period[which(data_plot$tempcat == cur_case)], 
#             rev(data_plot$period[which(data_plot$tempcat == cur_case)])),
#   y = c(    data_plot$p15[which(data_plot$tempcat == cur_case)], 
#             rev(data_plot$p85[which(data_plot$tempcat == cur_case)])),
#   col=paste0(u_col_case1, "33"), border=NA
# )
# lines(
#   data_plot$period[which(data_plot$tempcat == cur_case)],
#   data_plot$med[which(data_plot$tempcat == cur_case)],
#   col=u_col_case1, lwd=2
# )
# points(
#   data_plot$period[which(data_plot$tempcat == cur_case)],
#   data_plot$med[which(data_plot$tempcat == cur_case)],
#   col=u_col_case1, bg="#ffffffff", pch=21
# )
# 
# # Case 2: 1.5°C
# cur_case = "1.5°C"
# polygon(
#   x = c(    data_plot$period[which(data_plot$tempcat == cur_case)], 
#             rev(data_plot$period[which(data_plot$tempcat == cur_case)])),
#   y = c(    data_plot$p15[which(data_plot$tempcat == cur_case)], 
#             rev(data_plot$p85[which(data_plot$tempcat == cur_case)])),
#   col=paste0(u_col_case2, "33"), border=NA
# )
# lines(
#   data_plot$period[which(data_plot$tempcat == cur_case)],
#   data_plot$med[which(data_plot$tempcat == cur_case)],
#   col=u_col_case2, lwd=2
# )
# points(
#   data_plot$period[which(data_plot$tempcat == cur_case)],
#   data_plot$med[which(data_plot$tempcat == cur_case)],
#   col=u_col_case2, bg="#ffffffff", pch=21
# )
# 
# # 0 line
# lines(c(2005,2100), c(0, 0), col="red")
# 
# 
# 
# # Axes
# axis(1)
# axis(2)
# 
# # Box
# box()
# 
# 
# 

#==== Emission pathways 2 ========================================
#---- Emission pathways: data ----------------------------------
data_plot <- v_data_timeTechTemp_world_plot %>% 
  filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                         "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
  filter(tempcat %in% c("Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  mutate(tempcat = paste(tempcat)) %>% 
  mutate(tempcat = "2°C") %>% 
  filter(allcat %in% c("Default", "Delayed action until 2030")) %>% 
  select(-tempcat,-timingcat,-techcat) %>% 
  mutate(value = value/1000) %>% 
  group_by(model,scenario,region,allcat) %>% 
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
  mutate(period > 2005) %>% 
  filter(keep == TRUE) %>% 
  select(-keep, -check) %>% 
  gather(variable, value, -model, -scenario, -region, -period, -allcat) %>% 
  group_by(allcat,period,variable)  %>%  
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

datarcp_plot <- data_rcp %>% 
  filter(region == "World", variable == "CO2 emissions - Total", scenario == "IMAGE - RCP3-PD (2.6)") %>% 
  mutate(value = value*44/12)

datassp_plot <- v_data5 %>% 
  filter(region == "World", variable == "Emissions|CO2", scenario == "SSP2-26", model == "MESSAGE-GLOBIOM") %>% 
  mutate(value=value/1000)

#---- Emission pathways: plot ----------------------------------
par(plt=c(0.10,0.50,0.15,0.95), las=1)
plot(0,0, type="n",
     xaxs="i", yaxs="i",
     xlim=c(2005,2100), ylim=c(-25,50),
     xlab="", ylab="")

# Grid
if (f_grid_x) for (k in u_tick_y) lines(c(2005,2100), c(k,k), col="#eeeeee")
if (f_grid_y) for (k in u_tick_x) lines(c(k,k), c(-25,50), col="#eeeeee")

# # 2030-2050 zone
# rect(
#   2030, -20,
#   2050, 50,
#   col="#cccccc99", border=NA
# )
# lines(c(2030,2030), c(-20, 50), lty=2)
# lines(c(2050,2050), c(-20, 50), lty=2)

# Data
# # AR5 data:
# polygon(
#   x = c(ar5data_plot$period, rev(ar5data_plot$period)),
#   y = c(ar5data_plot$p15, ar5data_plot$p85),
#   col="#cccccc33", border=NA
# )

# Gross positive
cur_case = "Default"
polygon(
  x = c(    data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")], 
            rev(data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")])),
  y = c(    rep(0.0, length(data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2|Fossil Fuels and Industry|Gross")])), 
            rev(data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2|Fossil Fuels and Industry|Gross")])),
  #col=paste0(u_col_case1, "33"), border=u_col_case1, lty=3
  col="#cfdfeaff", border=u_col_case1, lty=3
)
cur_case = "Delayed action until 2030"
polygon(
  x = c(    data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")], 
            rev(data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")])),
  y = c(    rep(0.0, length(data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2|Fossil Fuels and Industry|Gross")])), 
            rev(data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2|Fossil Fuels and Industry|Gross")])),
  #col=paste0(u_col_case2, "33"), border=u_col_case2, lty=3
  col="#ffe6d5ff", border=u_col_case2, lty=3
)
# Gross negative
cur_case = "Delayed action until 2030"
polygon(
  x = c(    data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")], 
            rev(data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")])),
  y = c(    rep(0.0, length(data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2|Carbon Capture and Storage|Biomass")])), 
            rev(data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2|Carbon Capture and Storage|Biomass")])),
  #col=paste0(u_col_case2, "33"), border=u_col_case2, lty=3
  col="#ffe6d5ff", border=u_col_case2, lty=3
)
cur_case = "Default"
polygon(
  x = c(    data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")], 
            rev(data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")])),
  y = c(    rep(0.0, length(data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2|Carbon Capture and Storage|Biomass")])), 
            rev(data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2|Carbon Capture and Storage|Biomass")])),
  #col=paste0(u_col_case1, "33"), border=u_col_case1, lty=3
  col="#cfdfeaff", border=u_col_case1, lty=3
)


# lines(c(2000, 2005, seq(2010,2100,10)), datarcp_plot$value, col="#999999", lwd=2)
# lines(datassp_plot$period, datassp_plot$value, col="#999999", lwd=2)


# Case 1: 2°C
cur_case = "Default"
lines(
  data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")],
  data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")],
  col=u_col_case1, lwd=2
)
points(
  data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")],
  data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")],
  col=u_col_case1, bg="#ffffffff", pch=21
)
# Case 2: 1.5°C
cur_case = "Delayed action until 2030"
lines(
  data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")],
  data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")],
  col=u_col_case2, lwd=2
)
points(
  data_plot$period[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")],
  data_plot$med[which(data_plot$allcat == cur_case & data_plot$variable == "Emissions|CO2")],
  col=u_col_case2, bg="#ffffffff", pch=21
)

# 0 line
lines(c(2005,2100), c(0, 0), col="red", lty=2)



# Axes
axis(1)
axis(2)

# Box
box()





#==== Short-term emission reductions ========================================
#---- Short-term emission reductions: data ----------------------------------
data_plot <- v_data_timeTechTemp_world_plot %>% 
  filter(
    #period >= 2010,
    variable == "Emissions|CO2",
    tempcat %in% c("Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  mutate(tempcat = paste(tempcat)) %>% 
  mutate(tempcat = "2°C") %>% 
  filter(allcat %in% c("Default", "Delayed action until 2030")) %>% 
  select(-tempcat) %>% 
  mutate(value=value/1000) %>% 
  filter(period %in% c(2030,2050)) %>% 
  spread(period, value) %>% 
  mutate(value = (`2050`-`2030`)/20) %>% 
  group_by(allcat) %>% 
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

#---- Short-term emission reductions: plot ----------------------------------
par(plt=c(0.55,0.735,0.555,0.95), las=1, new=T)
plot(0,0, type="n",
     xaxs="i", yaxs="i",
     xlim=c(0.5,2.5), ylim=c(-2.5,0),
     xlab="", ylab="", axes=F)

pos = 1
offset = 0.45
color = u_col_case1
cur_case = "Default"
lines(c(pos, pos), c(data_plot$min[which(data_plot$allcat == cur_case)], data_plot$max[which(data_plot$allcat == cur_case)]), col=color)
rect(
  pos-offset, data_plot$p15[which(data_plot$allcat == cur_case)], 
  pos+offset, data_plot$p85[which(data_plot$allcat == cur_case)], 
  col=color, border=NA
)
lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#ffffff")
points(c(pos, pos), c(data_plot$mean[which(data_plot$allcat == cur_case)], data_plot$mean[which(data_plot$allcat == cur_case)]), pch=21, col=color, bg="#ffffff")


pos = 2
offset = 0.45
color = u_col_case2
cur_case = "Delayed action until 2030"
lines(c(pos, pos), c(data_plot$min[which(data_plot$allcat == cur_case)], data_plot$max[which(data_plot$allcat == cur_case)]), col=color)
rect(
  pos-offset, data_plot$p15[which(data_plot$allcat == cur_case)], 
  pos+offset, data_plot$p85[which(data_plot$allcat == cur_case)], 
  col=color, border=NA
)
lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#ffffff")
points(c(pos, pos), c(data_plot$mean[which(data_plot$allcat == cur_case)], data_plot$mean[which(data_plot$allcat == cur_case)]), pch=21, col=color, bg="#ffffff")

axis(2)

box()

#==== BECCS upscaling =======================================================
#---- BECCS upscaling: data -------------------------------------------------
data_plot <- v_data_timeTechTemp_world_plot %>% 
  filter(
    #period >= 2010,
    variable == "Emissions|CO2|Carbon Capture and Storage|Biomass",
    tempcat %in% c("Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  mutate(tempcat = paste(tempcat)) %>% 
  mutate(tempcat = "2°C") %>% 
  filter(allcat %in% c("Default", "Delayed action until 2030")) %>%
  select(-tempcat) %>% 
  mutate(value=value/1000)  %>% 
  filter(period %in% c(2030,2050)) %>% 
  spread(period, value) %>% 
  mutate(value = (`2050`-`2030`)/20) %>% 
  group_by(allcat) %>%  
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
#---- BECCS upscaling: plot -------------------------------------------------
par(plt=c(0.55,0.735,0.15,0.50), las=1, new=T)
plot(0,0, type="n",
     xaxs="i", yaxs="i",
     xlim=c(0.5,2.5), ylim=c(0,1),
     xlab="", ylab="", axes=F)

pos = 1
offset = 0.45
color = u_col_case1
cur_case = "Default"
lines(c(pos, pos), c(data_plot$min[which(data_plot$allcat == cur_case)], data_plot$max[which(data_plot$allcat == cur_case)]), col=color)
rect(
  pos-offset, data_plot$p15[which(data_plot$allcat == cur_case)], 
  pos+offset, data_plot$p85[which(data_plot$allcat == cur_case)], 
  col=color, border=NA
)
lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#ffffff")
points(c(pos, pos), c(data_plot$mean[which(data_plot$allcat == cur_case)], data_plot$mean[which(data_plot$allcat == cur_case)]), pch=21, col=color, bg="#ffffff")


pos = 2
offset = 0.45
color = u_col_case2
cur_case = "Delayed action until 2030"
lines(c(pos, pos), c(data_plot$min[which(data_plot$allcat == cur_case)], data_plot$max[which(data_plot$allcat == cur_case)]), col=color)
rect(
  pos-offset, data_plot$p15[which(data_plot$allcat == cur_case)], 
  pos+offset, data_plot$p85[which(data_plot$allcat == cur_case)], 
  col=color, border=NA
)
lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#ffffff")
points(c(pos, pos), c(data_plot$mean[which(data_plot$allcat == cur_case)], data_plot$mean[which(data_plot$allcat == cur_case)]), pch=21, col=color, bg="#ffffff")

axis(2)

box()

# #==== Carbon prices =========================================================
# #---- Carbon prices: data ---------------------------------------------------
# data_plot <- v_data_timeTechTemp_world_plot %>% 
#   filter(
#     #period >= 2010,
#     variable == "Price|Carbon",
#     tempcat %in% c("Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
#   mutate(tempcat = paste(tempcat)) %>% 
#   mutate(tempcat = "2°C") %>% 
#   filter(allcat %in% c("Default", "Delayed action until 2030")) %>% 
#   select(-tempcat) %>% 
#   filter(period == 2030) %>% 
#   group_by(allcat) %>%  
#   summarise(
#     min=min(value, na.rm=T),
#     p01=quantile(value, 0.01, na.rm=T),
#     p05=quantile(value, 0.05, na.rm=T),
#     p10=quantile(value, 0.10, na.rm=T),
#     p15=quantile(value, 0.15, na.rm=T),
#     mean=mean(value, na.rm=T),
#     med=median(value, na.rm=T),
#     p85=quantile(value, 0.85, na.rm=T),
#     p90=quantile(value, 0.90, na.rm=T),
#     p95=quantile(value, 0.95, na.rm=T),
#     p99=quantile(value, 0.99, na.rm=T),
#     max=max(value, na.rm=T)
#   ) %>% 
#   ungroup()
# #---- Carbon prices: plot ---------------------------------------------------
# par(plt=c(0.80,0.99,0.555,0.95), las=1, new=T)
# plot(0,0, type="n",
#      xaxs="i", yaxs="i",
#      xlim=c(0.5,2.5), ylim=c(0,300),
#      xlab="", ylab="", axes=F)
# 
# pos = 1
# offset = 0.45
# color = u_col_case1
# cur_case = "Default"
# lines(c(pos, pos), c(data_plot$min[which(data_plot$allcat == cur_case)], data_plot$max[which(data_plot$allcat == cur_case)]), col=color)
# rect(
#   pos-offset, data_plot$p15[which(data_plot$allcat == cur_case)], 
#   pos+offset, data_plot$p85[which(data_plot$allcat == cur_case)], 
#   col=color, border=NA
# )
# lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#ffffff")
# points(c(pos, pos), c(data_plot$mean[which(data_plot$allcat == cur_case)], data_plot$mean[which(data_plot$allcat == cur_case)]), pch=21, col=color, bg="#ffffff")
# 
# 
# pos = 2
# offset = 0.45
# color = u_col_case2
# cur_case = "Delayed action until 2030"
# lines(c(pos, pos), c(data_plot$min[which(data_plot$allcat == cur_case)], data_plot$max[which(data_plot$allcat == cur_case)]), col=color)
# rect(
#   pos-offset, data_plot$p15[which(data_plot$allcat == cur_case)], 
#   pos+offset, data_plot$p85[which(data_plot$allcat == cur_case)], 
#   col=color, border=NA
# )
# lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#ffffff")
# #lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#0000ff")
# points(c(pos, pos), c(data_plot$mean[which(data_plot$allcat == cur_case)], data_plot$mean[which(data_plot$allcat == cur_case)]), pch=21, col=color, bg="#ffffff")
# 
# axis(2)
# 
# box()

#==== Cumulative gross negative emissions =========================================================
#---- Cumulative gross negative emissions: data ---------------------------------------------------
data_plot <- v_data_timeTechTemp_world_plot %>% 
  filter(variable %in% c("Emissions|CO2|Carbon Capture and Storage|Biomass"), 
         tempcat %in% c("Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  mutate(tempcat = paste(tempcat)) %>% 
  mutate(tempcat = "2°C") %>% 
  filter(allcat %in% c("Default", "Delayed action until 2030")) %>% 
  mutate(value = value/1000) %>%
  mutate(period = as.numeric(period)) %>% 
  filter(period > 2005) %>% 
  group_by(model, scenario, region, allcat) %>% 
  arrange(period) %>% 
  mutate(dt  = (period   - lag(period, default = 2010))) %>% 
  mutate(tmp = dt*(value + lag(value, default = 0))/2) %>% 
  summarise(value = sum(tmp)) %>% 
  ungroup() %>% 
  group_by(allcat) %>%  
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
#---- Cumulative gross negative emissions: plot ---------------------------------------------------
par(plt=c(0.80,0.99,0.555,0.95), las=1, new=T)
plot(0,0, type="n",
     xaxs="i", yaxs="i",
     xlim=c(0.5,2.5), ylim=c(0,1200),
     xlab="", ylab="", axes=F)

pos = 1
offset = 0.45
color = u_col_case1
cur_case = "Default"
lines(c(pos, pos), c(data_plot$min[which(data_plot$allcat == cur_case)], data_plot$max[which(data_plot$allcat == cur_case)]), col=color)
rect(
  pos-offset, data_plot$p15[which(data_plot$allcat == cur_case)], 
  pos+offset, data_plot$p85[which(data_plot$allcat == cur_case)], 
  col=color, border=NA
)
lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#ffffff")
points(c(pos, pos), c(data_plot$mean[which(data_plot$allcat == cur_case)], data_plot$mean[which(data_plot$allcat == cur_case)]), pch=21, col=color, bg="#ffffff")


pos = 2
offset = 0.45
color = u_col_case2
cur_case = "Delayed action until 2030"
lines(c(pos, pos), c(data_plot$min[which(data_plot$allcat == cur_case)], data_plot$max[which(data_plot$allcat == cur_case)]), col=color)
rect(
  pos-offset, data_plot$p15[which(data_plot$allcat == cur_case)], 
  pos+offset, data_plot$p85[which(data_plot$allcat == cur_case)], 
  col=color, border=NA
)
lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#ffffff")
#lines(c(pos-offset, pos+offset), c(data_plot$med[which(data_plot$allcat == cur_case)], data_plot$med[which(data_plot$allcat == cur_case)]), col="#0000ff")
points(c(pos, pos), c(data_plot$mean[which(data_plot$allcat == cur_case)], data_plot$mean[which(data_plot$allcat == cur_case)]), pch=21, col=color, bg="#ffffff")

axis(2)

box()

#==== Cumulative emissions ==================================================
#---- Cumulative emissions: data --------------------------------------------
data_plot <- v_data_timeTechTemp_world_plot %>% 
  filter(variable %in% c("Emissions|CO2", "Emissions|CO2|Fossil Fuels and Industry", 
                         "Emissions|CO2|Land Use", "Emissions|CO2|Carbon Capture and Storage|Biomass"), 
         tempcat %in% c("Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
  mutate(tempcat = paste(tempcat)) %>% 
  mutate(tempcat = "2°C") %>% 
  filter(allcat %in% c("Default", "Delayed action until 2030")) %>% 
  mutate(value = value/1000) %>% 
  group_by(model,scenario,region,allcat) %>% 
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
  group_by(model, scenario, region, allcat) %>% 
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
  select(model,scenario,region,period,allcat, keep, check,
         `Emissions|CO2`, `Emissions|CO2|Fossil Fuels and Industry`, `Emissions|CO2|Fossil Fuels and Industry|Gross`, 
         `Emissions|CO2|Carbon Capture and Storage|Biomass`, `Emissions|CO2|Land Use`, 
         `Emissions|CO2|Cumulative`, `Emissions|CO2|Fossil Fuels and Industry|Cumulative`, `Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative`, 
         `Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative`, `Emissions|CO2|Land Use|Cumulative`) %>% 
  ungroup() %>% 
  filter(keep == TRUE) %>% 
  select(-keep, -check) %>% 
  gather(variable, value, -model, -scenario, -region, -period, -allcat) %>% 
  filter(period %in% c(2050, 2100)) %>% 
  spread(period, value) %>% 
  mutate(`2051-2100`=`2100`-`2050`) %>% 
  rename(`2011-2050`=`2050`) %>% 
  rename(`2011-2100`=`2100`) %>% 
  gather(period, value, -model,-scenario,-region,-allcat,-variable) %>% 
  spread(variable, value) %>% 
  mutate(`Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative` = `Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative` + `Emissions|CO2|Land Use|Cumulative`) %>%
  gather(variable, value, -model, -scenario, -region, -period, -allcat) %>% 
  group_by(allcat,period,variable)  %>%  
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

#---- Cumulative emissions: plot --------------------------------------------
par(plt=c(0.80,0.99,0.15,0.50), las=1, new=T)
plot(0,0, type="n",
     xaxs="i", yaxs="i",
     xlim=c(0.5, 2.5), ylim=c(-1000,2000),
     xlab="", ylab="", axes=F)


pos = 1
offset = 0.45
offset2 = 0.49
cur_case = "Default"
# Positive part
# Net Land-use emissions
rect(
  pos-offset, 0.0,
  pos+offset, data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Land Use|Cumulative")],
  col="#008000ff", border=NA)
# Gross FFI
rect(
  pos-offset, data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Land Use|Cumulative")],
  pos+offset, data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Land Use|Cumulative")] +
    data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative")],
  col="#424242ff", border=NA)
# Negative part
# Gross BECCS
rect(
  pos-offset, 0.0,
  pos+offset, data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative")],
  col="#a02c2cff", border=NA)
# 
rect(
  pos-offset2, data_plot$p15[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")],
  pos+offset2, data_plot$p85[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")],
  col="#2270d43c", border=NA
)
lines(
  c(pos-offset2, pos+offset2),
  c(
    data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")],
    data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")]
  ),
  col="#000000"
)
points(
  c(pos, pos),
  c(
    data_plot$mean[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")],
    data_plot$mean[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")]
  ),
  pch=21, bg="#ffffff", col="#000000"
)

pos = 2
offset = 0.45
offset2 = 0.49
cur_case = "Delayed action until 2030"
# Positive part
# Net Land-use emissions
rect(
  pos-offset, 0.0,
  pos+offset, data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Land Use|Cumulative")],
  col="#008000ff", border=NA)
# Gross FFI
rect(
  pos-offset, data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Land Use|Cumulative")],
  pos+offset, data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Land Use|Cumulative")] +
    data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Fossil Fuels and Industry|Gross|Cumulative")],
  col="#424242ff", border=NA)
# Negative part
# Gross BECCS
rect(
  pos-offset, 0.0,
  pos+offset, data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Carbon Capture and Storage|Biomass|Cumulative")],
  col="#a02c2cff", border=NA)
# 
rect(
  pos-offset2, data_plot$p15[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")],
  pos+offset2, data_plot$p85[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")],
  col="#2270d43c", border=NA
)
lines(
  c(pos-offset2, pos+offset2),
  c(
    data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")],
    data_plot$med[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")]
  ),
  col="#000000"
)
points(
  c(pos, pos),
  c(
    data_plot$mean[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")],
    data_plot$mean[which(data_plot$allcat == cur_case & data_plot$period == "2011-2100" & data_plot$variable == "Emissions|CO2|Cumulative")]
  ),
  pch=21, bg="#ffffff", col="#000000"
)


axis(2)

box()
