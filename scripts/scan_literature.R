library(tidyverse)
library(pdftools)
library(stringr)

source()

extract_title <- function(i_pdf) {
  
  title <- ""
  
  toc <- pdftools::pdf_toc(i_pdf)
  toc <- paste(unlist(toc))
  
  if(length(toc) != 0) {
    
    while (toc[1] == "") {
      toc <- toc[c(-1)]
    }
    
    title <- toc[1]
  }
  
  return(title)
}

search_kw_in_text <- function(i_pdf, i_kw) {
  freq <- rep(0, length(i_kw))
  
  txt  <- pdftools::pdf_text(i_pdf)
  
  for (kw in i_kw) {
    for (page in 1:length(txt)) {
      if (kw %in% c("BECS", "BECCS", "AR", "DAC", "EW")) {
        res <- gregexpr(kw, txt[page])[[1]]
      } else {
        res <- gregexpr(kw, tolower(txt[page]))[[1]]
      }
      
      if (res[1] != -1) freq[which(i_kw == kw)] <- freq[which(i_kw == kw)] + length(res)
    }
  }
  
  freq <- data.frame(t(freq))
  names(freq) <- i_kw
  
  freq <- cbind(data.frame(fullpath=i_pdf, stringsAsFactors = FALSE), freq)
  
  return(freq)
  
}

search_doi_in_text <- function(i_pdf) {
  
  doi = ""
  
  print(paste(i_pdf))
  
  txt  <- pdftools::pdf_text(paste(i_pdf))
  
  doipat <- "[dD][oO][iI](?!ng) {0,1}:{0,1} {0,1}(\\S*)"

  p <- strsplit(txt,"\r\n")[[1]]
  txt_doi <- str_match(paste0(p,collapse=" ;"),doipat)[,2]
  if (is_empty(txt_doi)) {
    p <- strsplit(txt,"\r\n")[[2]]
    txt_doi <- str_match(paste0(p,collapse=" ;"),doipat)[,2]
  }
  
  if (is.na(txt_doi)) cat("warning: doi could not be found in this document\n")
  
  return(txt_doi)
  
}

dirname_iam <- "Selected literature/"

#urls <- data.frame(url=list.files("C:/Users/hilj/ownCloud/IAM Special Issues/", recursive = TRUE))
urls <- data.frame(url=list.files(paste0("../", dirname_iam, "/"), recursive = TRUE))

get_prism_doi <- function(i_pdf) {
  
  doi = NA
  
  tmp <- pdf_info(i_pdf)$metadata
  start = regexpr("<prism:doi>", tmp)
  end   = regexpr("</prism:doi>", tmp)
  
  if (!length(start)==0 && !length(end) == 0) {
    doi = substr(tmp,start+11,end-1)
  }
  if (nchar(doi) < 5) doi <- NA
  
  return(doi)
  
}

df <- urls %>% 
  separate(url, into=c("MIP", "filename"), "/", drop=FALSE, remove=FALSE) %>% 
  mutate(AU=sapply(filename, function(x) trimws(substr(x, 1, nchar(x)-9)))) %>% 
  mutate(PY=sapply(filename, function(x) substr(trimws(substr(x, nchar(x)-8, nchar(x)-4)),1,4))) %>%
  mutate(doi1 = sapply(file.path("../", dirname_iam, url), get_prism_doi)) %>% 
  mutate(doi2 = sapply(file.path("../", dirname_iam, url), search_doi_in_text)) %>% 
  mutate(fullpath=file.path("../", dirname_iam, url)) %>% 
  mutate(TI=sapply(fullpath, extract_title)) %>% 
  data.frame()

# Search keywords
my_kw <- c("integrate", "model", "IAM", "negative emission", "NETs", "carbon dioxide removal", "CDR", "biomass with carbon capture", "bioenergy with carbon capture", "carbon capture", "sequestration", "BECS", "BECCS", "afforestation", "AR", "air capture", "DAC", "enhanced weathering", "EW")

kw_res <- do.call("rbind", lapply(df$fullpath, function(x) search_kw_in_text(x, my_kw)))

kw_res$relevant <- rowSums(kw_res[,2:ncol(kw_res)])

all <- df %>% 
  left_join(kw_res, by=c("fullpath")) %>% 
  select(-url, -fullpath)

all$TI <- gsub("&newnbsp;", "", all$TI, fixed = TRUE)
all$TI <- gsub("&compfn;", "", all$TI, fixed = TRUE)
all$TI <- gsub("&lowast;", "", all$TI, fixed = TRUE)
all$TI <- gsub("&x26;", "", all$TI, fixed = TRUE)
all$TI <- gsub("&rsquo;", "", all$TI, fixed = TRUE)

# Update information
all$TI[which(all$TI == "1. Introduction")] <- ""
all$TI[which(all$TI == "Introduction")] <- ""
all$TI[which(all$TI == "Abstract")] <- ""
all$TI[which(all$TI == "Article Contents")] <- ""

all$TI[which(all$filename == "Mazzotti et al 2013.pdf")] <- "Direct air capture of CO2 with chemicals: optimization of a two-loop hydroxide carbonate system using a countercurrent air-liquid contactor"
all$TI[which(grepl("Chaturvedi", all$filename))] <- "Role of energy efficiency in climate change mitigation policy for India: assessment of co-benefits and opportunities within an integrated assessment modeling framework"
all$TI[which(all$filename == "Kriegler et al 2014.pdf")] <- "The role of technology for achieving climate policy objectives: overview of the EMF 27 study on global technology and climate policy strategies"
all$TI[which(all$filename == "Popp et al 2014.pdf")] <- "Land-use transition for bioenergy and climate stabilization: model comparison of drivers, impacts and interactions with other land use based mitigation options"
all$TI[which(all$filename == "Sano et al 2014.pdf")] <- "Impacts of different diffusion scenarios for mitigation technology options and of model representations regarding renewables intermittency on evaluations of CO2 emissions reductions"  
all$TI[which(all$filename == "Schaeffer et al 2016.pdf")] <- "Exploring the use of dynamic linear panel data models for evaluating energy/economy/environment models — an application for the transportation sector"
all$TI[which(all$filename == "Azar 2001.pdf")] <- "Carbon sequestration from fossil fuels and biomass - long-term potentials"
all$TI[which(all$filename == "Azar et al 2006.pdf")] <- "CARBON CAPTURE AND STORAGE FROM FOSSIL FUELS AND BIOMASS – COSTS AND POTENTIAL ROLE IN STABILIZING THE ATMOSPHERE"  
all$TI[which(all$filename == "Canadell and Raupach 2008.pdf")] <- "Managing Forests for Climate Change Mitigation"
all$TI[which(all$filename == "Daioglou et al 2014.pdf")] <- "Competing uses of biomass for energy and chemicals: implications for long-term global CO2 mitigation potential"
all$TI[which(all$filename == "den Elzen and van Vuuren 2007.pdf")] <- "Peaking profiles for achieving long-term temperature targets with more likelihood at lower costs"  
all$TI[which(all$filename == "DOE 1999.pdf")] <- ""  
all$TI[which(all$filename == "Gambhir et al 2017.pdf")] <- "Assessing the Feasibility of Global Long-Term Mitigation Scenarios"
all$TI[which(all$filename == "Humpenöder et al 2014.pdf")] <- "Investigating afforestation and bioenergy CCS as climate change mitigation strategies"
all$TI[which(all$filename == "Keith 2001.pdf")] <- "SINKS, ENERGY CROPS AND LAND USE: COHERENT CLIMATE POLICY DEMANDS AN INTEGRATED ANALYSIS OF BIOMASS"
all$TI[which(all$filename == "Keith et al 2006.pdf")] <- ""  
all$TI[which(all$filename == "Klein et al 2011")] <- ""  
all$TI[which(all$filename == "Kreidenweis et al 2016.pdf")] <- "Afforestation to mitigate climate change: impacts on food prices under consideration of albedo effects" 
all$TI[which(all$filename == "Mashikira et al 2003")] <- ""  
all$TI[which(all$filename == "Möllersten and Yan 2001.pdf")] <- "Economic evaluation of of biomass-based energy system with CO2 capture and sequestration in kraft pulp mills" 
all$TI[which(all$filename == "Muratori et al 2016.pdf")] <- "Global economic consequences of deploying bioenergy with carbon capture and storage (BECCS)" 
all$TI[which(all$filename == "NAS 2001.pdf")] <- ""  
all$TI[which(all$filename == "Obersteiner et al 2001.pdf")] <- "Managing Climate Risk " 
all$TI[which(all$filename == "Obersteiner et al 2002.pdf")] <- ""  
all$TI[which(all$filename == "Rao and Riahi 2006.pdf")] <- "The Role of Non-CO2 Greenhouse Gases in Climate Change Mitigation: Long-term scenarios for the 21st century" 
all$TI[which(all$filename == "Reilly et al 2012.pdf")] <- "Using Land To Mitigate Climate Change: Hitting the Target, Recognizing the Trade-oﬀs" 
all$TI[which(all$filename == "Robiou du Pont 2016.pdf")] <- "National contributions for decarbonizing the world economy in line with the G7 agreement" 
all$TI[which(all$filename == "Schlamadinger et al 2001.pdf")] <- ""  
all$TI[which(all$filename == "Tavoni et al 2017.pdf")] <- "Challenges and opportunities for integrated modelling of climate engineering" 
all$TI[which(all$filename == "Vaughan 2016.pdf")] <- "Expert assessment concludes negative emissions scenarios may not deliver" 
all$TI[which(all$filename == "Wise et al 2009.pdf")] <- "Implications of Limiting CO2 Concentrations for Land Use and Energy" 

# all$doi3 <- sapply(all$doi2, function(x) {
#   
#   doi = ""
#   
#   x = trimws(x)
#   
#   pos_doi   <- regexpr("10.1", x, fixed=TRUE)[1]
#   pos_space <- gregexpr("\\s", x)[[1]]
#   
#   if (pos_space[1] != -1) {
#     id_space  <- which(abs(pos_space - pos_doi) == min (abs(pos_space - pos_doi)))
#     
#     if (pos_space[id_space] < pos_doi) id_space <- id_space + 1
#     
#     if (!is.na(pos_space[id_space])) {
#       doi = substr(x, pos_doi, pos_space[id_space])
#     } else {
#       doi = substr(x, pos_doi, nchar(x))  
#     }
#   } else {
#     doi = substr(x, pos_doi, nchar(x))
#   }
#   
#   return(doi)
# }) %>% paste()

all$doi <- all$doi1
all$doi[is.na(all$doi1)] <- all$doi2[is.na(all$doi1)]

all$doi[which(all$filename == "Griffin et al 2014.pdf")] <- "10.1007/s10584-013-0963-5" 
all$doi[which(all$filename == "McCollum et al 2013.pdf")] <- "10.1142/S2010007813400101" 
all$doi[which(all$filename == "Azar et al 2013.pdf")] <- "10.1088/1748-9326/8/3/034004" 
all$doi[which(all$filename == "Canadell and Raupach 2008.pdf")] <- "10.1126/science.1155458" 
all$doi[which(all$filename == "den Elzen and van Vuuren 2007.pdf")] <- "10.1073/pnas.0701598104" 
all$doi[which(all$filename == "Humpenöder et al 2014.pdf")] <- " 10.1088/1748-9326/9/6/064029" 
all$doi[which(all$filename == "Keith 2001.pdf")] <- "10.1023/A:1010617015484 " 
all$doi[which(all$filename == "Kreidenweis et al 2016.pdf")] <- "10.1088/1748-9326/11/8/085001" 
all$doi[which(all$filename == "Muratori et al 2016.pdf")] <- " 10.1088/1748-9326/11/9/095004" 
all$doi[which(all$filename == "Obersteiner et al 2001.pdf")] <- "10.1126/science.294.5543.786b " 
all$doi[which(all$filename == "Rao and Riahi 2006.pdf")] <- "10.5547/ISSN0195-6574-EJ-VolSI2006-NoSI3-9" 
all$doi[which(all$filename == "Robiou du Pont 2016.pdf")] <- "10.1088/1748-9326/11/5/054005" 
all$doi[which(all$filename == "Vaughan 2016.pdf")] <- "10.1088/1748-9326/11/9/095003" 
all$doi[which(all$filename == "Wise et al 2009.pdf")] <- "10.1126/science.1168475" 

docs <- all %>%
  filter(!is.na(doi) & nchar(doi) > 5) %>%
  filter(`negative emission` != 0)

nodoi <- all %>%
  filter(is.na(doi) | nchar(doi) < 5) %>%
  filter(`negative emission` != 0)

cdocs <- c(docs$doi,"10.1038/nclimate2572")

w_query <- paste(paste0('DO=("',cdocs, '")'), collapse=' OR ')

write(w_query, file = "wos_query.txt")

s_query <- paste(paste0('DOI("',cdocs, '")'), collapse=' OR ')

write(s_query, file = "s_query.txt")

# Write CSV
write.csv(all, file = "NETsinIAMstudies_new.csv")

