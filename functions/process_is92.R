process_IS92 <- function(data, scenario) {
  
  #-- Parameters ----------------------------------------
  nbrow_world  = 99  # World data contains 99 lines
  nbrow_region = 80  # Regional data contains 80 lines
  time_is92 <- c(1985,1990,1995,2000,2005,2010,2015,2020,2025,2050,2075,2100)
  
  #-- Process world data --------------------------------
  world <- cbind(data.frame(
    scenario=rep(scenario, nbrow_world), 
    region=rep("World", nbrow_world)), 
    data[1:nbrow_world,])
  
  #Energy content
  energy_content_beg = c(5,10,16,24,27,30,39)
  energy_content_end = c(9,15,23,26,29,38,47)
  for (k in 1:length(energy_content_beg)) {
    sector = world[energy_content_beg[k],3]
    unit   = world[energy_content_beg[k],4]
    
    world[(energy_content_beg[k]+1):energy_content_end[k],3] = paste0(sector,"|", gsub(" ", "", world[(energy_content_beg[k]+1):energy_content_end[k],3]))
    world[(energy_content_beg[k]+1):energy_content_end[k],4] = rep(unit, energy_content_end[k]-energy_content_beg[k])
  }
  
  #Emission content
  emission_content_beg = c(48,54,57,61,67,71)
  emission_content_end = c(53,56,60,66,69,76)
  for (k in 1:length(emission_content_beg)) {
    sector = world[emission_content_beg[k],3]
    
    world[(emission_content_beg[k]+1):emission_content_end[k],3] = paste0(sector,"|", gsub(" ", "", world[(emission_content_beg[k]+1):emission_content_end[k],3]))
  }
  world <- world[-c(1,energy_content_beg, emission_content_beg),] # remove also first line (header)
  
  # Rename columns
  colnames(world) <- c(c("scenario","region","variable","unit"), paste(time_is92))
  
  #-- Process regional data -----------------------------
  #Note: the function read.xlsx remove any empty lines
  data <- data[-(1:nbrow_world),]
  region  <- data.frame()
  for (k in 1:(dim(data)[1]/nbrow_region)) {
    
    regionname = data[nbrow_region*(k-1)+1,1]
    
    tmp <- cbind(data.frame(scenario = rep(scenario, nbrow_region-1), 
                            region   = rep(regionname, nbrow_region-1)), 
                 data[(nbrow_region*(k-1)+2):(nbrow_region*(k-1)+nbrow_region),])
    
    #Energy content
    energy_content_beg = c(4, 9,15,23,26,29,38)
    energy_content_end = c(8,14,22,25,28,37,46)
    for (k2 in 1:length(energy_content_beg)) {
      sector = tmp[energy_content_beg[k2],3]
      unit   = tmp[energy_content_beg[k2],4]
      
      tmp[(energy_content_beg[k2]+1):energy_content_end[k2],3] = paste0(sector,"|", gsub(" ", "", tmp[(energy_content_beg[k2]+1):energy_content_end[k2],3]))
      tmp[(energy_content_beg[k2]+1):energy_content_end[k2],4] = rep(unit, energy_content_end[k2]-energy_content_beg[k2])
    }
    
    #Emission content
    emission_content_beg = c(47,53,56,60,66,70)
    emission_content_end = c(52,55,59,65,68,75)
    for (k2 in 1:length(emission_content_beg)) {
      sector = tmp[emission_content_beg[k2],3]
      
      tmp[(emission_content_beg[k2]+1):emission_content_end[k2],3] = paste0(sector,"|", gsub(" ", "", tmp[(emission_content_beg[k2]+1):emission_content_end[k2],3]))
    }
    tmp <- tmp[-c(energy_content_beg, emission_content_beg),]
    
    # Rename columns
    colnames(tmp) <- c(c("scenario","region","variable","unit"), paste(time_is92))
    
    # Save to region data.frame
    region = rbind(region, tmp)
    
  }
  
  # Bind world and region data.frames
  out = rbind(world, region)
  
  # Switch from long-format to short-format
  out <- out %>% gather(period, value, -scenario, -region, -variable, -unit)
  
  return(out)
  
}