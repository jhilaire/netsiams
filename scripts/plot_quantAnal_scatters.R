plot_scenario_var_scatter(v_data_tempTargets_world_plot, 
                          i_x="Emissions|CO2", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", 
                          i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)

p <- plot_scenario_var_scatter(v_data_tempTargets_world_plot %>% 
                            compute_grossCO2emiFFI(), 
                          i_x="Emissions|CO2|Fossil Fuels and Industry|Gross", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", 
                          i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)

p2 <- p + 
  #geom_polygon(aes(x, y), data=data.frame(x=c(0,500,1500,0,0), y=c(0, 0, 1000, 1000, 0)), fill="#ff000033", col=NA) +
  geom_polygon(aes(x, y), data=data.frame(x=c(300,1550,300,300), y=c(0, 1000, 1000, 0)), fill="#ff000033", col=NA) +
  geom_polygon(aes(x, y), data=data.frame(x=c(300,1100,2100,1550,300), y=c(0, 0, 1000, 1000, 0)), fill="#00ff0033", col=NA) +
  geom_polygon(aes(x, y), data=data.frame(x=c(1100,2200,2200,2100,1100), y=c(0, 0, 1000, 1000, 0)), fill="#0000ff33", col=NA) +
  geom_abline(intercept = 100-(1200-1100)/(100-0)*1200, slope = (1200-1100)/(100-0), lty=2) +
  geom_abline(intercept = 100-(1200-1100)/(100-0)*1200 + 600, slope = (100-0)/(1200-1100), lty=2, color="green") +
  geom_abline(intercept = -(1000-0)/(1550-300)*300, slope = (1000-0)/(1550-300), lty=2) +
  xlim(300,2200) + ylim(0,1000)

ggExtra::ggMarginal(
  p = p2,
  type = 'histogram',
  margins = 'both',
  size = 5,
  col = 'white',
  fill = '#E6E6E6'
)

plot_scenario_var_scatter_TTT(v_data_timeTechTemp_world_plot, 
                              i_x="Emissions|CO2", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", 
                              i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)

plot_scenario_var_scatter_wCP(v_data_tempTargets_world_plot %>% compute_grossCO2emiFFI() %>% add_cp2030("tempcat"), i_x="Emissions|CO2|Fossil Fuels and Industry|Gross", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)

tmp <- v_data_tempTargets_world_plot %>% add_allLevers("tempcat")

p <- ggplot(tmp %>% filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario"))) +
  geom_point(aes(x=beccsDep, y=cp2030, color=emi2030)) +
  facet_wrap(~tempcat, ncol=3) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  ylim(0,500)
print(p)

p <- ggplot(tmp %>% 
              filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
              mutate(ln_cp2030=log(cp2030))) +
  geom_point(aes(x=beccsDep, y=avgEmiRed20302050, color=ln_cp2030)) +
  facet_wrap(~tempcat, ncol=3) +
  theme_bw() + 
  theme(legend.position = "bottom")
print(p)

p <- ggplot(tmp %>% 
              filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
              mutate(ln_cp2030=log(cp2030))) +
  geom_point(aes(x=ln_cp2030, y=avgEmiRed20302050, color=beccsDep)) +
  facet_wrap(~tempcat, ncol=3) +
  theme_bw() + 
  theme(legend.position = "bottom")
print(p)

p <- ggplot(tmp %>% 
              filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
              mutate(ln_cp2030=log(cp2030))) +
  geom_point(aes(x=ln_cp2030, y=avgEmiRed20302050, color=cumNE)) +
  facet_wrap(~tempcat, ncol=3) +
  theme_bw() + 
  theme(legend.position = "bottom")
print(p)

p <- ggplot(tmp %>% 
              filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario", "Likely 3.0°C scenario")) %>% 
              mutate(ln_cp2030=log(cp2030)) %>% 
              mutate(label = paste0(model, " - ", scenario, "\n",
                                    "BECCS deployment in 2100: ", round(beccsDep, digits=1), "\n",
                                    "Cumulative neg. emissions: ", round(cumNE, digits=0)))) +
  geom_point(aes(x=ln_cp2030, y=RavgEmiRed20302050, color=cumNE, text=label)) +
  facet_wrap(~tempcat, ncol=4) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  xlab("Logarithm of carbon price in 2030") +
  ylab("R Average emissions reductions 2030-2050 [Gt CO2/yr]")
print(p)

p <- ggplot(tmp %>% 
              filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario", "Likely 3.0°C scenario")) %>% 
              mutate(ln_cp2050=log(cp2050)) %>% 
              mutate(label = paste0(model, " - ", scenario, "\n",
                                    "BECCS deployment in 2100: ", round(beccsDep, digits=1), "\n",
                                    "Cumulative neg. emissions: ", round(cumNE, digits=0)))) +
  geom_point(aes(x=cp2050, y=RavgEmiRed20302050, color=cumNE, text=label)) +
  facet_wrap(~tempcat, ncol=4) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  xlab("Logarithm of carbon price in 2050") +
  ylab("R Average emissions reductions 2030-2050 [Gt CO2/yr]") +
  xlim(0,500)
print(p)

#ggplotly(p, tooltip=c("label"))