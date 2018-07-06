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
#---- Basic statistics -----------------------
# Compute total number of unique statements
nb_stat <- note_stats %>% 
  filter(!duplicated(stat)) %>% 
  summarise(count=n())

# Compute number of statements by paragraph relevance
nb_stat_by_rel <- all_simple %>%
  filter(!relevant %in% c(0,2), stat != "") %>% 
  mutate(relevant=ifelse(relevant == 1, "True", ifelse(relevant == 2, "False", ifelse(relevant == 3, "Maybe", "Parsing error")))) %>% 
  group_by(relevant) %>% 
  filter(!duplicated(stat)) %>% 
  summarise(count=n()) %>% 
  ungroup()

# Compute number of statement by category
nb_stat_by_cat <- all_simple %>%
  filter(!relevant %in% c(0,2), stat != "") %>% 
  group_by(tech) %>% 
  filter(!duplicated(stat)) %>% 
  summarise(count=n()) %>% 
  ungroup()

# Compute number of statement by category and year
nb_stat_by_cat_and_yr <- all_simple %>% 
  filter(!relevant %in% c(0,2), stat != "") %>%
  mutate(c1c2=paste0(c2,"-",c1)) %>% 
  group_by(c1c2, py) %>% 
  filter(!duplicated(stat)) %>% 
  summarise(count=n()) %>% 
  ungroup()

ggplot(nb_stat_by_cat_and_yr) +
  geom_line(aes(py, count)) + 
  facet_wrap(~c1c2, scale="free_y") +
  xlim(2000, 2020) +
  theme_bw()

#---- Analysis of statements by category ----------------------
# Find all statements where D1_Dynamics is selected
all_simple %>% 
  filter(!relevant %in% c(0,2), stat != "") %>%
  dplyr::filter(
    grepl("D1", tech),
    !duplicated(stat)) %>% 
  openxlsx::write.xlsx(file="test_D1.xlsx")

View(ns_d1)

docs_d1 <- unique(pars$doc__title[which(pars$id %in% ns_d1$id)])



