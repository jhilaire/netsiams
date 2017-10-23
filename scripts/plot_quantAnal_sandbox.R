#== Carbon prices ==================================================
# Carbon prices (differences)
ggplot(v_data_timeTechTemp_world_plot %>% 
         filter(!scenario %in% c(
           "AMPERE2-450-FullTech-OPT-policy",
           "AMPERE2-550-FullTech-OPT-policy",
           "AMPERE2-450-NoCCS.-OPT-policy",
           "AMPERE2-550-NoCCS.-OPT-policy"
         )) %>% 
         filter(model != "IMACLIM v1.1") %>% 
         filter(
           variable == "Price|Carbon",
           grepl("AMPERE2-450-NoCCS.*|AMPERE2-450-FullTech.*|AMPERE2-550-NoCCS.*|AMPERE2-550-FullTech.*", scenario)) %>%
         separate(scenario, into=c("mip", "target", "tech", "pol"), sep="-") %>% 
         unite("mip.target", mip, target, sep="-") %>% 
         select(model, mip.target, tech, pol, period, tempcat,value) %>% 
         filter(pol == "OPT", model %in% c("REMIND 1.5", "IMACLIM v1.1", "MESSAGE V.4", "MERGE_EMF27")) %>%
         arrange(model, mip.target, tech, period) %>% 
         spread(tech, value) %>% 
         mutate(value=(FullTech-NoCCS))) +
  geom_line(aes(x=period, y=value, color=model, lty=mip.target)) +
  ggtitle("Effect of BECCS on carbon prices (absolute)") + 
  xlab("") + ylab("Differences in carbon price [US$]") +
  theme_bw()

# Carbon prices (relative differences)
ggplot(v_data_timeTechTemp_world_plot %>% 
         filter(!scenario %in% c(
           "AMPERE2-450-FullTech-OPT-policy",
           "AMPERE2-550-FullTech-OPT-policy",
           "AMPERE2-450-NoCCS.-OPT-policy",
           "AMPERE2-550-NoCCS.-OPT-policy"
         )) %>% 
         filter(model != "IMACLIM v1.1") %>% 
         filter(
           variable == "Price|Carbon",
           grepl("AMPERE2-450-NoCCS.*|AMPERE2-450-FullTech.*|AMPERE2-550-NoCCS.*|AMPERE2-550-FullTech.*", scenario)) %>%
         separate(scenario, into=c("mip", "target", "tech", "pol"), sep="-") %>% 
         unite("mip.target", mip, target, sep="-") %>% 
         select(model, mip.target, tech, pol, period, tempcat,value) %>% 
         filter(pol == "OPT", model %in% c("REMIND 1.5", "IMACLIM v1.1", "MESSAGE V.4", "MERGE_EMF27")) %>%
         arrange(model, mip.target, tech, period) %>% 
         spread(tech, value) %>% 
         mutate(value=(FullTech-NoCCS)/NoCCS*100)) +
  geom_line(aes(x=period, y=value, color=model, lty=mip.target)) +
  ggtitle("Effect of BECCS on carbon prices (relative)") + 
  xlab("") + ylab("Differences in carbon price [%]") +
  theme_bw()


#== Mitigation costs ==================================================
ggplot(v_data_timeTechTemp_world_plot %>% 
         filter(variable == "Policy Cost|GDP Loss", allcat == "Default", tempcat == "1.5°C scenario")) +
  geom_line(aes(x=period, y=value, group=scenario))

tmp <- v_data_timeTechTemp_world_plot %>% 
  filter(variable == "Policy Cost|GDP Loss", 
         timingcat == "immediate", techcat %in% c("default", "limBio", "noCCS", "noBECCS"),
         period %in% seq(2010,2100,10)) %>% 
  filter(!is.na(value)) %>% 
  mutate(value=ifelse(grepl("Scen", scenario) & model == "REMIND", -value, value)) %>% 
  #mutate(techcat=ifelse(techcat %in% c("noCCS", "noBECCS"), "noCCS/BECCS", techcat)) %>% 
  dplyr::select(-allcat,-region,-variable) %>% 
  group_by(model,scenario,tempcat,techcat,timingcat) %>% 
  arrange(period) %>% 
  mutate(allinfo=paste0(period,"-",lead(period, default=2100),",",value,"#",lead(value))) %>% 
  filter(period != 2100) %>% 
  mutate(period=paste0(period+1,"-",lead(period, default=2100))) %>% 
  mutate(sum_disc=sapply(allinfo, function(x) {

    info_period <- strsplit(x, ",")[[1]][1]
    info_value  <- strsplit(x, ",")[[1]][2]
    
    period_start <- as.numeric(strsplit(info_period, "-")[[1]][1]) +1
    period_end   <- as.numeric(strsplit(info_period, "-")[[1]][2])
    
    value_start <- as.numeric(strsplit(info_value, "#")[[1]][1])
    value_end   <- as.numeric(strsplit(info_value, "#")[[1]][2])
    
    res_interp <- approx(c(period_start, period_end), c(value_start, value_end), seq(period_start,period_end,1))
    
    out <- sum(res_interp$y*1/(1+0.5)^(seq(period_start,period_end,1) - 2011))
    
    return(out)
  })) %>% 
  dplyr::select(-allinfo) %>% 
  ungroup()

tmp2 <- tmp %>% 
  filter(techcat %in% c("default", "noCCS")) %>% 
  group_by(model,scenario,tempcat,techcat,timingcat) %>% 
  summarise(value=sum(sum_disc)) %>% 
  ungroup() %>% 
  mutate(mod.scen=paste0(model,".",scenario)) %>% 
  mutate()

p <- ggplot(tmp %>% 
         filter(techcat %in% c("default", "noCCS")) %>% 
         group_by(model,scenario,tempcat,techcat,timingcat) %>% 
         summarise(value=sum(sum_disc)) %>% 
         ungroup() %>% 
         mutate(mod.scen=paste0(model,".",scenario))) +
  geom_point(aes(x=techcat,y=value,colour=tempcat,group=mod.scen))+
  facet_wrap(~model, ncol=4) +
  theme_bw()
print(p)
ggplotly(p)


# u_path_ampere <-  "../../bitbucket/beccs/data/AmperePublic_WP2+3_2014-10-09.csv"
# v_RData_AMPERE <- paste0(substr(u_path_ampere, 1, nchar(u_path_ampere)-3), "RData")
# if (!file.exists(v_RData_AMPERE) && u_readOriginalData) {
#   data_ampere <- read.csv(u_path_ampere) %>% 
#     select(-`X2000`, -`X2065`, -`X2075`, -`X2085`, -`X2095`)
#   names(data_ampere) <- c("model", "scenario", "region", "variable", "unit", paste(c(seq(2005,2055,5), seq(2060,2100,10))))
#   data_ampere <- data_ampere %>% 
#     gather(period, value, -model, -scenario, -region, -variable, -unit)
#   
#   save(data_ampere, file = v_RData_AMPERE)
#   
# } else {
#   load(v_RData_AMPERE)
# }

tmp3 <- data_ampere %>% 
  filter(variable == "Policy Cost|GDP Loss", grepl("AMPERE2.*FullTech-OPT|AMPERE2.*NoCCS-OPT", scenario),
         region == "World", period %in% seq(2010,2100,10)) %>% 
  filter(!grepl(".*OPT-policy", scenario)) %>% 
  separate(scenario, into=c("project","target","technology","policytiming"), sep="-") %>% 
  select(-project,-policytiming,-unit,-region,-variable) %>% 
  filter(target != "Base") %>% 
  mutate(period=as.numeric(period)) %>% 
  spread(technology, value) %>% 
  filter(!is.na(NoCCS)) %>% 
  gather(technology, value, -model,-target,-period) %>% 
  group_by(model,target,technology) %>% 
  arrange(period) %>% 
  mutate(allinfo=paste0(period,"-",lead(period, default=2100),",",value,"#",lead(value))) %>% 
  filter(period != 2100) %>% 
  mutate(period=paste0(period+1,"-",lead(period, default=2100))) %>% 
  mutate(value=sapply(allinfo, function(x) {
    
    info_period <- strsplit(x, ",")[[1]][1]
    info_value  <- strsplit(x, ",")[[1]][2]
    
    period_start <- as.numeric(strsplit(info_period, "-")[[1]][1]) +1
    period_end   <- as.numeric(strsplit(info_period, "-")[[1]][2])
    
    value_start <- as.numeric(strsplit(info_value, "#")[[1]][1])
    value_end   <- as.numeric(strsplit(info_value, "#")[[1]][2])
    
    res_interp <- approx(c(period_start, period_end), c(value_start, value_end), seq(period_start,period_end,1))
    
    out <- sum(res_interp$y*1/(1+0.5)^(seq(period_start,period_end,1) - 2011))
    
    return(out)
  })) %>% 
  dplyr::select(-allinfo) %>% 
  ungroup()


ggplot(tmp3) +
  geom_bar(aes(x=period, y=value, fill=technology), stat="identity") +
  facet_grid(target~model, scales="free_y") +
  scale_y_log10() +
  theme_bw()

ggplot(tmp3 %>% 
         mutate(target=factor(target)) %>% 
         mutate(technology=factor(technology)) %>% 
         mutate(period=factor(period, levels=sort(unique(tmp3$period)), ordered=TRUE)) %>% 
         mutate(xaxis=as.numeric(period))) +
  geom_path(aes(x=xaxis, y=value, colour=technology)) +
  facet_grid(target~model, scales="free_y") +
  scale_y_log10() +
  theme_bw()

plot(0,0,type="n",xlim=c(2010,2100),ylim=c(0,1),xlab="",ylab="Discount rate")
lines(seq(2011,2100), 1/(1+0.05)^(seq(2011,2100)-2011))
lines(seq(2011,2100), 1/(1+0.02)^(seq(2011,2100)-2011))
lines(seq(2011,2100), 1/(1+0.00)^(seq(2011,2100)-2011))
lines(c(2050,2050),c(0,1),lty=2)
text(2050, 1/(1+0.02)^(seq(2011,2100)-2011)[which(seq(2011,2100) == 2050)], paste(round(1/(1+0.02)^(seq(2011,2100)-2011)[which(seq(2011,2100) == 2050)], digits = 2)))
text(2050, 1/(1+0.05)^(seq(2011,2100)-2011)[which(seq(2011,2100) == 2050)], paste(round(1/(1+0.05)^(seq(2011,2100)-2011)[which(seq(2011,2100) == 2050)], digits = 2)))

ggplot(tmp3 %>% group_by(model,target,technology) %>% summarise(value=sum(value)) %>% ungroup()) +
  geom_bar(aes(x=target, y=value, fill=technology), stat="identity") +
  facet_wrap(~model, scales="free_y") +
  theme_bw()