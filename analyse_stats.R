library(dplyr)
library(tidyr)

# Categories
cat <- list()
cat[["broad"]]    <- c("Z2", "Z3", "Z4", "Z5", "Z6")
cat[["bound"]]    <- c("H1", "H2", "H3", "H4", "H5", "H6")
cat[["dynamics"]] <- c("D1", "D2", "D3", "D4", "D5")
cat[["costs"]]    <- c("C1", "C2", "C3", "C4")
cat[["tech"]]     <- c("T1", "T2", "T3")
cat[["climate"]]  <- c("O1", "O2")
cat[["other"]]    <- c("R1")

# Get data
pars       <- read.csv("data/paragraphs.csv", stringsAsFactors = FALSE)
note_stats <- read.csv("data/notes_statements.csv", stringsAsFactors = FALSE)
alldocs    <- readxl::read_excel("all_docs_20180529_edited_new.xlsx")

# Process data
pars       <- pars %>% select(-X) %>% 
  mutate(doc__PY=ifelse(grepl("Challenges and Opportunities for Integrated Modeling", doc__title), 2017, doc__PY)) %>% 
  mutate(doc__authors=ifelse(grepl("Challenges and Opportunities for Integrated Modeling", doc__title), "Tavoni et al", doc__authors))
# Add missing authors
pars       <- pars %>% 
  mutate(doc__authors = ifelse(doc__authors == "", unlist(sapply(doc__title, function(x) {
    sd <- stringdist::stringdist(x,alldocs$TI, method="jaccard", q=2)
    out <- alldocs$AU[which(sd == min(sd, na.rm=TRUE)[1])]
  })), doc__authors))
  
note_stats <- note_stats %>% select(-X)
all <- note_stats %>% 
  left_join(pars, by=c("id")) %>% 
  rename(AU=doc__authors) %>% 
  rename(PY=doc__PY) %>% 
  rename(TI=doc__title) %>% 
  rename(par=text) %>% 
  rename(stat=docstatement__text) %>% 
  rename(note=note__text) %>% 
  rename(tech=docstatement__technology__name) %>% 
  rename(user=docownership__user__username) %>% 
  rename(relevant=docownership__relevant)
all_simple <- all %>% 
  select(AU,PY,TI,par,stat,note,tech,user,relevant) %>% 
  mutate(c1=unlist(sapply(tech,
                                 function(x) {
                                   techs <- sapply(strsplit(names(unlist(cat)), ".", fixed=TRUE), function(y) y[2])
                                   out <- paste(unlist(cat))[which(substr(x,1,2) == techs)]
                                   out <- ifelse(length(out)== 0, "", out)
                                   return(out)
                                 }))) %>% 
  mutate(c2=unlist(sapply(tech,
                                 function(x) {
                                   techs <- sapply(strsplit(names(unlist(cat)), ".", fixed=TRUE), function(y) y[2])
                                   out <- names(unlist(cat))[which(substr(x,1,2) == techs)]
                                   out <- ifelse(length(out)== 0, "", out)
                                   tryCatch(
                                     out <- strsplit(out, ".", fixed=TRUE)[[1]][1],
                                     error=function(e){
                                       out <- ""
                                     }  
                                   )
                                   return(out)
                                 }))) 


# Compute total number of statements
nb_stat <- note_stats %>% 
  filter(!duplicated(docstatement__text)) %>% 
  summarise(count=n())

# Compute number of statements by paragraph relevance
nb_stat_by_rel <- all %>% 
  group_by(docownership__relevant) %>% 
  filter(!duplicated(docstatement__text)) %>% 
  summarise(count=n()) %>% 
  ungroup()

# Compute number of statement by category
nb_stat_by_cat <- note_stats %>% 
  group_by(docstatement__technology__name) %>% 
  filter(!duplicated(docstatement__text)) %>% 
  summarise(count=n()) %>% 
  ungroup()

#
nb_stat_by_cat_and_yr <- all %>% 
  group_by(docstatement__technology__name, doc__PY) %>% 
  filter(!duplicated(docstatement__text)) %>% 
  summarise(count=n()) %>% 
  ungroup()

ggplot(nb_stat_by_cat_and_yr) +
  geom_line(aes(doc__PY, count)) + 
  facet_wrap(~docstatement__technology__name, scale="free_y") +
  xlim(2000, 2020)

# Find all statements where D1_Dynamics is selected
ns_docs <- note_stats %>% 
  dplyr::filter(
    grepl("D1", docstatement__technology__name),
    !duplicated(docstatement__text)) %>% 
  dplyr::left_join(pars, by =c("id")) %>% 
  tidyr::spread(``)

View(ns_d1)

docs_d1 <- unique(pars$doc__title[which(pars$id %in% ns_d1$id)])



