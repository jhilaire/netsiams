corporate <- function (df, col = "AB", ignorewords = c("the", "however", "this", "and")) 
{
  
  corpus <- tm::Corpus(tm::VectorSource(df[[col]])) %>% 
    #tm::tm_map(tm::PlainTextDocument) %>% 
    tm::tm_map(tm::removePunctuation) %>% 
    tm::tm_map(tm::removeWords, tm::stopwords()) %>% 
    tm::tm_map(tm::removeWords, ignoreWords) %>% 
    tm::tm_map(tm::stemDocument)
  
  return(corpus)
}
