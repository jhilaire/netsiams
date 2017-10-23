fpath_xml <- "C:/Users/hilj/Documents/"

library(xml2)

#data <- xml2::read_xml(fpath_xml)

tmp <- xml_find_all(data, ".//paragraph")

extract_data_from_node <- function(node) {
  
  out <- data.frame(
    xmin           = node %>% xml_attr("minX"),
    xmax           = node %>% xml_attr("maxX"),
    ymin           = node %>% xml_attr("minY"),
    ymax           = node %>% xml_attr("maxY"),
    mostCommonFont = node %>% xml_attr("mostCommonFont"),
    mostCommonFontSize = node %>% xml_attr("mostCommonFontsize"),
    startFont = node %>% xml_attr("startFont"),
    startFontSize = node %>% xml_attr("startFontsize"),
    endFont = node %>% xml_attr("endFont"),
    endFontSize = node %>% xml_attr("endFontsize"),
    mostCommonColor = node %>% xml_attr("mostCommonColor"),
    startColor = node %>% xml_attr("startColor"),
    endColor = node %>% xml_attr("endColor"),
    role = node %>% xml_attr("role"),
    page = as.numeric(node %>% xml_attr("page")),
    text = node %>% xml_text(),
    stringsAsFactors = FALSE
  )
  
  return(out)
}

xml2df <- function(data) {
  
  tmp <- xml_find_all(data, ".//paragraph")
  
  nodeList <- list()
  
  for (k in 1:length(tmp)) {
    nodeList[[k]] <- extract_data_from_node(tmp[k])
  }
  
  #nodeList <- lapply(, extract_data_from_node)
  
  out <- cbind(
    data.frame(id=1:length(tmp)),
    do.call("rbind", nodeList))
}

# Extract data as df
tmp_df <- xml2df(data)

# Get title
title <- tmp_df$text[which(tmp_df$role == "title")] 
if (length(title) == 0) title = ""

# Get keywords
keywords <- tmp_df$text[which(tmp_df$role == "keywords")] 
if (length(keywords) == 0) keywords = ""

# Get Acknowledgment id
id_ack <- tmp_df$id[which(grepl("acknowledgment", tolower(tmp_df$text)))]

# Get Reference id
id_ref <- tmp_df$id[which(tmp_df$role == "reference-heading")]

# Get sections
tmp_sections <- tmp_df[which(tmp_df$role == "section-heading"),]
tmp_sections_mostCommonFont_l1 <- tmp_sections %>% 
  filter(grepl("^([0-9]\\.){1}\\s.*", text)) %>% 
  group_by(mostCommonFont) %>% 
  summarize(count=n()) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  select(mostCommonFont) %>% 
  paste()
tmp_sections_mostCommonFont_l2 <- tmp_sections %>% 
  filter(grepl("^([0-9]\\.){2}\\s.*", text)) %>% 
  group_by(mostCommonFont) %>% 
  summarize(count=n()) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  select(mostCommonFont) %>% 
  paste()
tmp_sections_mostCommonFont_l3 <- tmp_sections %>% 
  filter(grepl("^([0-9]\\.){3}\\s.*", text)) %>% 
  group_by(mostCommonFont) %>% 
  summarize(count=n()) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  select(mostCommonFont) %>% 
  paste()
tmp_sections_mostCommonFontSize_l1 <- tmp_sections %>% 
  filter(grepl("^([0-9]\\.){1}\\s.*", text)) %>% 
  group_by(mostCommonFontSize) %>%
  summarize(count=n()) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  select(mostCommonFontSize) %>% 
  paste()
tmp_sections_mostCommonFontSize_l2 <- tmp_sections %>% 
  filter(grepl("^([0-9]\\.){2}\\s.*", text)) %>% 
  group_by(mostCommonFontSize) %>%
  summarize(count=n()) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  select(mostCommonFontSize) %>% 
  paste()
tmp_sections_mostCommonFontSize_l3 <- tmp_sections %>% 
  filter(grepl("^([0-9]\\.){3}\\s.*", text)) %>% 
  group_by(mostCommonFontSize) %>%
  summarize(count=n()) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  select(mostCommonFontSize) %>% 
  paste()

tmp_bodytext <- tmp_df[which(tmp_df$role == "body-text"),]
tmp_bodyText_mostCommonFont <- tmp_bodytext %>% 
  group_by(mostCommonFont) %>% 
  summarize(count=n()) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  select(mostCommonFont) %>% 
  paste()
tmp_bodyText_mostCommonFontSize <- tmp_bodytext %>% 
  group_by(mostCommonFontSize) %>% 
  summarize(count=n()) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  select(mostCommonFontSize) %>% 
  paste()
  

# Filter out non section data
tmp_sections <- tmp_df %>% 
  filter(
    mostCommonFont     %in% c(tmp_sections_mostCommonFont_l1, tmp_sections_mostCommonFont_l2, tmp_sections_mostCommonFont_l3), 
    mostCommonFontSize %in% c(tmp_sections_mostCommonFontSize_l1, tmp_sections_mostCommonFontSize_l2, tmp_sections_mostCommonFontSize_l3)) %>% 
  filter(grepl("^([0-9]\\.)+\\s.*", text))


sections <- tmp_sections %>% 
  mutate(section_level = ifelse(grepl("^([0-9]\\.){1}\\s.*", text), 1, 
                                ifelse(grepl("^([0-9]\\.){2}\\s.*", text), 2, 
                                      ifelse(grepl("^([0-9]\\.){3}\\s.*", text), 3, NA))))


# Get paragraphs
id_start <- sections$id[1]
id_end   <- id_ack-1

tmp_paragraphs <- tmp_df[id_start:id_end,] %>% 
  filter(mostCommonFontSize == tmp_bodyText_mostCommonFontSize)


# Search KWs