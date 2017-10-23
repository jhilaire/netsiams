#==== USER SECTION =======================
u_fpath_wosdata    <- "data/wos_69refs.txt"
u_fpath_scopusdata <- "data/scopus_69refs.bib"
u_ignorewords <- c("the","however","this","that","and")
  
#==== INITIALISE =========================
#---- Load libraries ---------------------
library(tidyverse)
library(bibliometrix)
library(scimetrix)
library(wordcloud)
library(slam)
library(topicmodels)
library(LDAvis)

#==== GET DATA ===========================
source("scripts/convert_wos_data.R", encoding = "UTF-8")
#source("scripts/convert_scopus_data.R", encoding = "UTF-8")


#==== PROCESS DATA =======================
#---- Generate corpus --------------------
corpus           <- corporate(v_data_wos, ignorewords=u_ignorewords)

#---- Create DTM -------------------------
dtm              <- scimetrix::makeDTM(corpus, 0.95, v_data_wos$UT, 0.05, 0)

#---- Filter data (TD-IDF) ---------------
rem              <- filter(v_data_wos, UT %in% dtm$removed)
data_WoSfiltered <- subset(v_data_wos, !(UT %in% dtm$removed))

#---- Re-generate corpus -----------------
corpus           <- scimetrix::refresh_corp(dtm$dtm)


#==== PLOT DATA ==========================
wordcloud(corpus, max.words = 100, random.order=FALSE)


#==== TOPIC MODELING =====================
#---- Initialise TM ----------------------
k    <- 10   # Number of topics
SEED <- 2016 # Seed for repoducibility
meta.tm <-
  list(
    Gibbs= LDA(dtm$dtm, k=k, method="Gibbs",
               control=list(seed=SEED,burnin=1000,
                            thin=100,iter=1000))
  )

# Generate json data for interactive plot
json_gibbs     <- topicmodels_json_ldavis(meta.tm[["Gibbs"]], corpus, dtm$dtm)

# Interactive plot
unlink("gibbs", recursive=TRUE)
serVis(json_gibbs, out.dir = 'tmp_TM-Gibbs', open.browser = TRUE)
