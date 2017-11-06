data_edmonds <- read.csv("data/Edmonds2013-scenarios.csv", 
                         stringsAsFactors = FALSE,
                         header=F, col.names = c("scenario.technology", "value")) %>% 
  separate(scenario.technology, c("scenario", "technology"), sep="-")


data_plot <- data_edmonds

ggplot(data_plot) +
  geom_bar(aes(x=scenario, y=value, fill=technology), stat="identity") +
  coord_flip()+
  theme_bw() +
  scale_fill_manual(values = c("BECCS"=RColorBrewer::brewer.pal(9, "Set1")[2], "AR"=RColorBrewer::brewer.pal(9, "Set1")[3])) +
  xlab("") + ylab("Cumulative negative CO2 emissions [GtCO2]") +
  guides(fill="none")
