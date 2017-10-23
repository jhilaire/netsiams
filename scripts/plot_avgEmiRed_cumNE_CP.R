tmp <- v_data_tempTargets_world_plot %>% 
  compute_grossCO2emiFFI() %>% 
  add_allLevers("tempcat")

p <- ggplot(tmp %>% filter(tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario"))) +
  geom_point(aes(x=beccsDep, y=cp2030, color=emi2030)) +
  facet_wrap(~tempcat, ncol=3) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  ylim(0,500)
print(p)

p <- ggplot(tmp %>% 
              filter(
                model == "REMIND",
                tempcat %in% c("1.5°C scenario", "Likely 2.0°C scenario", "Medium 2.0°C scenario")) %>% 
              mutate(log10_cp2030=log(cp2030, base = 10))) +
  geom_point(aes(x=cumNE, y=avgEmiRed20302050, color=log10_cp2030)) +
  facet_wrap(~tempcat, ncol=3) +
  theme_bw() + 
  xlab("Cumulative gross negative CO2 emissions [Gt CO2]") +
  ylab("Averaged annual CO2 emissions reductions [Mt CO2/yr]") +
  theme(legend.position = "bottom") +
  scale_color_gradientn(name="Carbon price in 2030",
                        colours=c('black', 'darkblue', 'blue', 'orange', 'red'),
                        values   = c(-2,0,1,2,3,10),
                        breaks   = c(0,1,2,3),
                        labels   = c("1", "10", "100", "1000"),
                        rescaler = function(x,...) x,
                        oob      = identity)
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

ggplotly(p, tooltip=c("label"))
