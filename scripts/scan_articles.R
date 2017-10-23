remove_header <- function(text) {
  ids <- gregexpr("\r\n", text)[[1]]
  text <- substr(text, (ids[1]+2), nchar(text))
  print(substr(text, 1, 100)) 
  return(text)
}

remove_footer <- function(text) {
  ids <- gregexpr("\r\n", text)[[1]]
  not_found <- TRUE
  iter <- 1
  while(not_found && iter <= 5) {
    test <- grep("Please cite this article", substr(text, rev(ids)[iter], nchar(text)))
    if (length(test) != 0) {
      not_found <- FALSE
    } else {
      iter <- iter + 1
    }
  }
  text <- substr(text, 1, rev(ids)[iter]-1)
  print(substr(text, nchar(text)-100, nchar(text)))
  return(text)
}


src <- ""
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

QTD_COLUMNS <- 2
read_text <- function(text) {
  result <- ''
  
  # Get all index of " " from page.
  lstops <- gregexpr(pattern =" ", text)
  
  # Puts the index of the most frequents ' ' in a vector.
  stops <- as.integer(names(sort(table(unlist(lstops)),decreasing=TRUE)[1:2]))
  
  # Slice based in the specified number of colums (this can be improved)
  for(i in seq(1, QTD_COLUMNS, by=1))
  {
    temp_result <- sapply(text, function(x){
      start <- 1
      stop <-stops[i] 
      if(i > 1)            
        start <- stops[i-1] + 1
      if(i == QTD_COLUMNS) #last column, read until end.
        stop <- nchar(x)+1
      substr(x, start=start, stop=stop)
    }, USE.NAMES=FALSE)
    temp_result <- trim(temp_result)
    result <- append(result, temp_result)
  }
  result
}


find_matching_section <- function(i_sec, i_tocList) {
  
  if (length(grep(i_sec, i_tocList, value=TRUE)) == 0) {
    a <- i_sec
    b <- toc_list
    res_jaccard <- stringdist::stringdist(a, b, method="jaccard", q=2)
    
    cur_sec <- toc_list[which(res_jaccard == min(res_jaccard))]
  } else {
    cur_sec <- toc_list[grep(i_sec, toc_list)]
  }
  
  return(cur_sec)
  
}

get_previous_section <- function(i_sec, i_tocList) {
  
  if (length(grep(i_sec, i_tocList, value=TRUE)) == 0) {
    a <- i_sec
    b <- toc_list
    res_jaccard <- stringdist::stringdist(a, b, method="jaccard", q=2)
    
    cur_sec <- toc_list[which(res_jaccard == min(res_jaccard))-1]
  } else {
    cur_sec <- toc_list[grep(i_sec, toc_list)-1]
  }
  
  return(cur_sec)
  
}

append_text_lines <- function(i_lines) {
  return(paste(i_lines, collapse=" "))
}

find_sections <- function(i_text, i_tocList) {
  # Find section positions
  id_sec    <- grep("^[0-9]\\.", i_text)
  sections  <- list()
  sec_start <- 1
  sec_end   <- 2
  # Loop over sections
  for (k_sec in id_sec) {
    
    print(paste0("[",k_sec,"] ", i_text[k_sec]))
    
    # Last section identified
    if (k_sec == id_sec[length(id_sec)]) {
      print("  > This is the last section...")
      if (length(id_sec) == 1) {
        if (k_sec == 1) {
          cur_sec <- find_matching_section(i_text[k_sec], i_tocList)
          sections[[cur_sec]] <- i_text[(sec_start+2):length(i_text)]
        } else {
          cur_sec <- get_previous_section(i_text[k_sec], i_tocList)
          sections[[cur_sec]] <- i_text[sec_start:(k_sec-1)]
          
          cur_sec <- find_matching_section(i_text[k_sec], i_tocList)
          sections[[cur_sec]] <- i_text[(k_sec+2):length(i_text)]
        }
      } else {
        
        # Was the previous section located at the line just above this one?
        if (k_sec - 1 == id_sec[which(id_sec == k_sec)-1]) {
          print("      > Previous section located at the line just above...")
          print(paste0("        - ", find_matching_section(i_text[k_sec-1], i_tocList)))
          sections[[find_matching_section(i_text[k_sec-1], i_tocList)]] <- ""
          
          cur_sec <- find_matching_section(i_text[k_sec], i_tocList)
          print(paste0("        - ", cur_sec))
          print(cur_sec)
          
          if (i_text[k_sec+1] == "" & i_text[k_sec+3] == "") {
            sections[[cur_sec]] <- i_text[(sec_start+4):length(i_text)]
          } else {
            sections[[cur_sec]] <- i_text[sec_start:length(i_text)]
          }
          
          # Update starting position
          sec_start <- k_sec + 2
          
          # If not,
        } else {
          # Update end position
          sec_end <- k_sec-2 #ifelse(k_sec == id_sec[length(id_sec)], length(i_text), k_sec-2)
          
          print(paste0(sec_start, " - ", sec_end))
          
          cur_sec <- get_previous_section(i_text[k_sec], i_tocList)
          
          sections[[cur_sec]] <- i_text[sec_start:sec_end]
          
          # Update starting position
          sec_start <- k_sec + 2
          cur_sec <- find_matching_section(i_text[k_sec], i_tocList)
          sections[[cur_sec]] <- i_text[sec_start:length(i_text)]
          
        }
      }
      # Other sections
    } else {
      if (k_sec == id_sec[1]) {
        print("  > This is the first section...")
        # Update end position
        sec_end <- k_sec-2
        
        print(paste0(sec_start, " - ", sec_end))
        
        cur_sec <- get_previous_section(i_text[k_sec], i_tocList)
        
        if (cur_sec %in% names(sections)) {
          sections[[cur_sec]] <- c(sections[[cur_sec]], i_text[sec_start:sec_end])
        } else {
          sections[[cur_sec]] <- i_text[sec_start:sec_end]
        }
        # Update starting position
        sec_start <- k_sec + 2
      } else {
        print("  > This is a middle section...")
        if (k_sec - 1 == id_sec[which(id_sec == k_sec)-1]) {
          print("      > Previous section located at the line just above...")
          cur_sec <- get_previous_section(i_text[k_sec], i_tocList)
          
          sections[[cur_sec]] <- NULL
          
          # Update starting position
          sec_start <- k_sec + 2
        } else {
          # Update end position
          sec_end <- k_sec-2
          
          print(paste0(sec_start, " - ", sec_end))
          
          cur_sec <- get_previous_section(i_text[k_sec], i_tocList)
          
          if (cur_sec %in% names(sections)) {
            sections[[cur_sec]] <- c(sections[[cur_sec]], i_text[sec_start:sec_end])
          } else {
            sections[[cur_sec]] <- i_text[sec_start:sec_end]
          }
          
          # Update starting position
          sec_start <- k_sec + 2
        }
      }
    }
  }
  
  return(sections)
  
}

sort_text_by_sec_par_sen <- function(i_sections) {
  data_text <- list()
  id_dt <- 1
  
  for(k_sec in names(i_sections)) {
    print(k_sec)
    # Find end of paragraph positions
    id_eop <- grep(".*\\.$", i_sections[[k_sec]])
    if (length(id_eop) != 0) {
      paragraphs <- list()
      for (k_eop in 1:length(id_eop)) {
        if (k_eop != length(id_eop)) {
          paragraphs[[k_eop]] <- i_sections[[k_sec]][lag(id_eop+1, default=1)[k_eop]:id_eop[k_eop]]        
        } else {
          paragraphs[[k_eop]]   <- i_sections[[k_sec]][lag(id_eop+1, default=1)[k_eop]:id_eop[k_eop]]        
          paragraphs[[k_eop+1]] <- i_sections[[k_sec]][(id_eop[k_eop]+1):length(i_sections[[k_sec]])]        
        }
        
      }
      
      for (k_par in 1:length(paragraphs)) {
        # Find end of sentence positions
        id_eos <- grep('(?<=\\.)\\s+[A-Z]', paragraphs[[k_par]], perl=TRUE)
        sentences <- list()
        
        if (length(id_eos) == 0) {
          tmp_sentence <- paragraphs[[k_par]][1:length(paragraphs[[k_par]])]
          sentences[[1]] <- append_text_lines(tmp_sentence)
          data_text[[id_dt]] <- data.frame(
            section   = k_sec,
            paragraph = k_par,
            sentence  = sentences[[1]] )
          id_dt <- id_dt+1
        } else {
          for (k_eos in 1:length(id_eos)) {
            if (k_eos != length(id_eos) ) {
              tmp_sentence <- paragraphs[[k_par]][lag(id_eos, default=1)[k_eos]:id_eos[k_eos]]
            } else {
              tmp_sentence <- paragraphs[[k_par]][lag(id_eos, default=1)[k_eos]:length(paragraphs[[k_par]])]
            }
            
            if (k_eos == 1) { 
              tmp <- strsplit(tmp_sentence[length(tmp_sentence)] , ". ", fixed=TRUE)[[1]]
              if (length(tmp) != 1) {
                id_capital <- grep('^[A-Z]', tmp)
                tmp_sentence[length(tmp_sentence)] <- paste0(paste0(tmp[1:(id_capital-1)], collapse=". "), ".")
              } else {
                tmp_sentence[length(tmp_sentence)] <- tmp
              }
            }
            
            if (k_eos == length(id_eos)) {
              tmp <- strsplit(tmp_sentence[1] , ". ", fixed=TRUE)[[1]]
              if (length(tmp) != 1) {
                id_capital <- grep('^[A-Z]', tmp)
                tmp_sentence[1] <- paste0(tmp[id_capital:length(tmp)], collapse=". ")
              } else {
                tmp_sentence[1] <- tmp # This does not make sense
              }
            } 
            
            if (k_eos != 1 && k_eos != length(id_eos)) {
              tmp <- strsplit(tmp_sentence[1] , ". ", fixed=TRUE)[[1]]
              id_capital <- grep('^[A-Z]', tmp)
              tmp_sentence[1] <- paste0(tmp[id_capital:length(tmp)], collapse=". ")
              
              tmp <- strsplit(tmp_sentence[length(tmp_sentence)] , ". ", fixed=TRUE)[[1]]
              id_capital <- grep('^[A-Z]', tmp)
              tmp_sentence[length(tmp_sentence)] <- paste0(paste0(tmp[1:id_capital], collapse=". "), ".")
            }
            
            sentences[[k_eos]] <- append_text_lines(tmp_sentence)
            data_text[[id_dt]] <- data.frame(
              section   = k_sec,
              paragraph = k_par,
              sentence  = sentences[[k_eos]] )
            id_dt <- id_dt+1
          }
        }
        # Replace lines by sentences
        paragraphs[[k_par]] <- unlist(sentences)
      }      
      #i_sections[[k_sec]] <- paragraphs
    }
  }
  data_text <- do.call("rbind", data_text)
}



