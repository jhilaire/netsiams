process_statement_review_data <- function(i_fpath_par, i_fpath_notestat, i_fpath_docall) {
  # Get data
  pars       <- read.csv(i_fpath_par, stringsAsFactors = FALSE)
  note_stats <- read.csv(i_fpath_notestat, stringsAsFactors = FALSE)
  alldocs    <- readxl::read_excel(i_fpath_docall)
  
  # Process data
  pars       <- pars %>% select(-X) %>% 
    mutate(doc__PY=ifelse(grepl("Challenges and Opportunities for Integrated Modeling", doc__title), 2017, doc__PY)) %>% 
    mutate(doc__authors=ifelse(grepl("Challenges and Opportunities for Integrated Modeling", doc__title), "Tavoni et al", doc__authors))
  # Add missing authors
  pars       <- pars %>% 
    mutate(doc__authors = ifelse(doc__authors == "", unlist(sapply(doc__title, function(x) {
      sd <- stringdist::stringdist(x,alldocs$TI, method="jaccard", q=2)
      out <- alldocs$AU[which(sd == min(sd, na.rm=TRUE)[1])]
    })), doc__authors)) %>% 
    rename(docid=doc__id) %>% 
    rename(au=doc__authors) %>% 
    rename(ti=doc__title) %>% 
    rename(py=doc__PY) %>% 
    rename(parid=id) %>% 
    rename(par=text) %>% 
    rename(user=docownership__user__username) %>% 
    rename(relevant=docownership__relevant)
  
  # Remove row id column in note_stats
  note_stats <- note_stats %>% 
    select(-X, -note__user__username) %>% 
    rename(parid=id) %>% 
    rename(note=note_text) %>% 
    rename(statid=docstatement__id) %>% 
    rename(stat=docstatement__text) %>% 
    rename(tech=docstatement__technology__name)
  
  # Merge data 
  all <- note_stats %>% 
    left_join(pars, by=c("parid"))
  
  # Simplify table
  all_simple <- all %>% 
    select(au,py,ti,par,note,stat,tech,user,relevant) %>% 
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
  
  out <- list(
    pars = pars,
    note_stats = note_stats,
    all=all,
    all_simple=all_simple
  )
  
  return(out)
  
}