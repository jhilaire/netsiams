#== Section: History =================
#-- SRES data on carbon sequestration -----
dat_sres %>% 
  filter(variable == "Carbon Sequestraction", region == "World", model %in% c("ASF", "IMAGE")) %>% 
  select(-variable, -unit, -region) %>% 
  group_by(model,scenario) %>% 
  mutate(value=value*44/12) %>% 
  mutate(dt=0.5*(lead(period, default=2100)-period) + 0.5*(period - lag(period, default=2000))) %>% 
  summarize(
    min=min(value, na.rm=T), 
    max=max(value, na.rm=T), 
    cum=sum(value*dt, na.rm=T)) %>% 
  ungroup()