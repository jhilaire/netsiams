# Cum. net CO2 emissions VS. Cum gross negative CO2 emissions
plot_scenario_var_scatter(v_data_tempTargets_world_plot, 
                          i_x="Emissions|CO2", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", 
                          i_xlab="Cumulative net CO2 emissions [Gt CO2]", i_ylab="Cumulative gross negative CO2 emissions [Gt CO2]",
                          i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)

# Cum. net negative CO2 emissions VS. Cum gross negative CO2 emissions
plot_scenario_var_scatter(v_data_tempTargets_world_plot, 
                          i_x="Emissions|CO2", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", 
                          i_xlab="Cumulative net negative CO2 emissions [Gt CO2]", i_ylab="Cumulative gross negative CO2 emissions [Gt CO2]",
                          i_factorX=-1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T, i_negX=TRUE)

# Cum. gross positive CO2 emissions from FFI VS. Cum gross negative CO2 emissions
p <- plot_scenario_var_scatter(v_data_tempTargets_world_plot %>% 
                                 compute_grossCO2emiFFI(), 
                               i_x="Emissions|CO2|Fossil Fuels and Industry|Gross", i_y="Emissions|CO2|Carbon Capture and Storage|Biomass", 
                               i_xlab="Cumulative gross positive CO2 emissions (FFI) [Gt CO2]", i_ylab="Cumulative gross negative CO2 emissions [Gt CO2]",
                               i_factorX=1e-3, i_factorY=1e-3, i_cumX=T, i_cumY=T)

p2 <- p + 
  #geom_polygon(aes(x, y), data=data.frame(x=c(0,500,1500,0,0), y=c(0, 0, 1000, 1000, 0)), fill="#ff000033", col=NA) +
  geom_polygon(aes(xx, yy), data=data.frame(xx=c(300,1550,300,300), yy=c(0, 1000, 1000, 0)), fill="#ff000033", col=NA) +
  geom_polygon(aes(x, y), data=data.frame(x=c(300,1100,2100,1550,300), y=c(0, 0, 1000, 1000, 0)), fill="#00ff0033", col=NA) +
  geom_polygon(aes(x, y), data=data.frame(x=c(1100,2200,2200,2100,1100), y=c(0, 0, 1000, 1000, 0)), fill="#0000ff33", col=NA) +
  geom_abline(intercept = 100-(1200-1100)/(100-0)*1200, slope = (1200-1100)/(100-0), lty=2) +
  geom_abline(intercept = 100-(1200-1100)/(100-0)*1200 + 600, slope = (100-0)/(1200-1100), lty=2, color="green") +
  geom_abline(intercept = -(1000-0)/(1550-300)*300, slope = (1000-0)/(1550-300), lty=2) +
  xlim(300,2200) + ylim(0,1000)
print(p2)
ggsave(p2, file="plots/cum_gross_pe_vs_cum_gross_ne.pdf", width=10, height=9)