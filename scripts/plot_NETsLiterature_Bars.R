library(readxl)
library(tidyverse)
library(RColorBrewer)

dat_netLit    <- read_xlsx("data/all_considered_docs.xlsx")
dat_iamnetLit <- read_xlsx("data/NETs - part2 - section 2 IAM.xlsx")

# Filter out unecessary data
dat_netLit <- dat_netLit %>% 
  filter(!is.na(Technologies)) %>% 
  select(Publication_Year, Technologies) %>% 
  rename(year=Publication_Year) %>% 
  mutate(nb_tech=sapply(Technologies, function(x) length(strsplit(x, ";")[[1]])))

# Get unique technologies
technologies <- unique(trimws(unlist(strsplit(unique(dat_netLit$Technologies), ";"))))

# Get unique years
years <- sort(unique(dat_netLit$year))

# Reconstruct data
list <- lapply(years,
       function(x) {
         list <- lapply(technologies,
                function(y) {
                  data.frame(year=x, technology=y, value=length(grep(y, dat_netLit$Technologies[which(dat_netLit$year == x)])))
                })
         out <- do.call("rbind", list)
       })
dat_netLit <- do.call("rbind", list)

# Plot AR

k_tech <- "Afforestation/reforestation"

techs <- c("Afforestation/reforestation", "Direct Air Capture", "BECCS", "Enhanced Weathering", "Soil Carbon Sequestration", "Biochar", "Ocean fertilisation", "Ocean Alkalinisation")

cols_set1 <- paste0(tolower(RColorBrewer::brewer.pal(8, "Set1")), "ff")

#220
colmin <- c("#cfeaceff", "#ffdcb9ff", "#c9deefff", "#e5d0e8ff", "#e9e9e9ff", "#e9e9e9ff", "#e9e9e9ff", "#e9e9e9ff")
#240
colmin <- c("#eaf6eaff", "#fff0e1ff", "#e8f1f8ff", "#f4ebf5ff", "#e9e9e9ff", "#e9e9e9ff", "#e9e9e9ff", "#e9e9e9ff")
colmax <- c(cols_set1[3], cols_set1[5], cols_set1[2], cols_set1[4], "#3c3c3cff", "#3c3c3cff", "#3c3c3cff", "#3c3c3cff")


svglite::svglite("plots/NETs_literature_history.svg")

plot(0,0,type="n",xlim=c(1980,2020),ylim=c(0,length(techs)+1),axes=FALSE,xlab="",ylab="")

axis(1, seq(1980,2020,10))


for (kt in 1:length(techs)) {
  
  k_tech <- techs[kt]
  
  x <- dat_netLit$year[which(dat_netLit$technology == k_tech)]
  y <- dat_netLit$value[which(dat_netLit$technology == k_tech)]
  
  freq_max <- max(y)
  
  f_colramp <- colorRampPalette(c(colmin[kt], colmax[kt]))
  cols <- f_colramp(1000)
  
  xapprox <- seq(1980,2017,1)
  yapprox <- approx(x,y,xapprox)
  
  text(1980, 0.5 + (kt-1), techs[kt], adj = c(1,0.5))
  
  for(k in 1:(length(xapprox)-1)) {
    
    rect(
      xapprox[k],   0.2 + (kt-1),
      xapprox[k+1], 0.8 + (kt-1),
      col= cols[round(yapprox$y[k]/freq_max*1000, digits=0)],
      border=NA
    )
  }
}

dev.off()

# 
# 
# library(ggplot2)
# library(ggrepel)
# 
# tmp <- dat_iamnetLit %>% 
#   filter(`negative emission` > 1 | `carbon dioxide removal` > 1) %>% 
#   group_by(PY) %>% 
#   arrange(MIP,AU) %>% 
#   mutate(id=row_number()) %>% 
#   ungroup() %>%
#   mutate(ti=tolower(TI)) %>% 
#   left_join(v_data_wos %>% 
#               mutate(ti=tolower(TI)) %>% 
#               select(ti, TC),
#             by=c("ti")) %>% 
#   mutate(TC=as.numeric(paste(TC))) %>% 
#   mutate(TC=ifelse(is.na(TC), 1, TC))
# 
# p <- ggplot(tmp) +
#   geom_point(aes(x=PY, y=id, color=MIP, size=TC)) +
#   #geom_label_repel(aes(x=PY, y=id, label=paste0(AU, " (", PY, ")"))) +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   xlim(1980,2020) +
#   xlab("") + ylab("")
# print(p)
# 
# library()
