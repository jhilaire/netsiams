library(quanteda)
library(pdftools)
library(jsonlite)
#library(tm)

mypdf <- "C:/Users/hilj/ownCloud/IAM Special Issues/AMPERE Special issue/Iyer et al 2013.pdf"

txt <- pdf_text(mypdf)

grep("BECCS", txt)

toc <- pdf_toc(mypdf)
toc_list <- paste(unlist(toc))
if (toc_list[1] == "") toc_list <- toc_list[c(-1)]
title <- toc_list[1]
#jsonlite::toJSON(toc, auto_unbox = TRUE, pretty = TRUE)

#info <- pdf_info(mypdf)
#jsonlite::toJSON(info, auto_unbox = TRUE, pretty = TRUE)

search_kw_negativeemission <- list()
for (kpage in 1:length(txt)) {
  
  txt_by_sentence <- gsub("\\s+", " ", unlist(strsplit(txt[kpage], '(?<=\\.)\\s+[A-Z]', perl=TRUE)))
  
  search_kw_negativeemission[[kpage]] <- txt_by_sentence[grep("negative emission", tolower(txt_by_sentence))]
}


journal <- trimws(gsub("<[^>]*>", "", grep("publicationName", strsplit(info$metadata, "\n")[[1]], value=TRUE))) %>% 
  gsub("&amp;", "&")
doi     <- trimws(gsub("<[^>]*>", "", grep("doi", strsplit(info$metadata, "\n")[[1]], value=TRUE)))
url     <- trimws(gsub("<[^>]*>", "", grep("url", strsplit(info$metadata, "\n")[[1]], value=TRUE)))

result <- list()
#for (i in 1:length(txt)) { 
for (i in 1:12) { 
  cat("============================================================\n")
  cat(paste0("Page ", i, "\n"))
  cat("============================================================\n")
  page <- txt[i]
  if (i == 1) {
    page <- substr(txt[1], unlist(gregexpr(toc_list[2], txt[1])), nchar(txt[1]))
  }
  
  
  if (i != 1) page <- remove_header(page)
  page <- remove_footer(page)
  
  t1 <- unlist(strsplit(page, "\n"))      
  maxSize <- max(nchar(t1))
  t1 <- paste0(t1,strrep(" ", maxSize-nchar(t1)))
  
  res <- read_text(t1)
  
  if (i == 1) {
    splitpos <- round(length(res)/2)
    start <- grep("Corresponding author", res)
    end <- grep("^http.*doi", res)
    res <- res[c(1:(start-1), c((end+1):length(res)))]
  }
  
  # Remove footnotes
  id <- grep("^([0-9]){1,2}$", res)
  if (length(id) != 0) res <- res[1:(id-1)]
  
  if (res[1] == "") res <- res[c(-1)]
  
  # Find section positions
  sections <- find_sections(res, toc_list)
  
  # Sort text by sections, paragraphs and sentences
  df_text <- sort_text_by_sec_par_sen(sections)
  
  # Add page number
  df_text$page <- i
  
  # Save
  result[[i]] <- df_text
  cat("\n")
}

# Combine list of data frames into a single one df
result <- do.call("rbind", result)