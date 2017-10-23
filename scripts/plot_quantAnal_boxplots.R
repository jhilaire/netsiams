plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Emissions|CO2", i_factor=1e-3, i_cumulate=T)
plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Emissions|CO2|Carbon Capture and Storage|Biomass", i_factor=1e-3, i_cumulate=T)
plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Emissions|CO2|Land Use", i_factor=1e-3, i_cumulate=T)
plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Emissions|CO2|Fossil Fuels and Industry", i_factor=1e-3, i_cumulate=T)

plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Emissions|CO2", i_factor=1e-3, i_period=2030)
plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Emissions|CO2|Carbon Capture and Storage|Biomass", i_factor=1e-3, i_period=2030)
plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Emissions|CO2|Land Use", i_factor=1e-3, i_period=2030)
plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Emissions|CO2|Fossil Fuels and Industry", i_factor=1e-3, i_period=2030)

plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Price|Carbon", i_ylim=c(0,1e3), i_period=2030)

plot_scenario_var_boxplot(v_data_timeTechTemp_world_plot, "Primary Energy|Biomass|w/ CCS", i_period=2050)

