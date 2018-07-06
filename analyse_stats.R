u_path_pars     <- "data/paragraphs.csv"
u_path_notestat <- "data/notes_statements.csv"
u_path_alldoc   <- "all_docs_20180529_edited_new.xlsx"

#==== Initialise =============================
library(dplyr)
library(tidyr)
library(ggplot2)
source("get_statement_categories.R")
source("process_statement_review_data.R")


#==== Get data ===============================
# Get categories
cat <- get_statement_categories()

# Get data
data_all   <- process_statement_review_data(u_path_pars, u_path_notestat, u_path_alldoc)
pars       <- data_all[["pars"]]
note_stats <- data_all[["note_stats"]]
all        <- data_all[["all"]]
all_simple <- data_all[["all_simple"]]


#==== Analyse data ===========================
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



