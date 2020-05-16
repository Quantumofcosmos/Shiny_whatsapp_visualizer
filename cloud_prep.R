library(tm)
cloud_prep <- function(chat) {
oldw <- getOption("warn")
options(warn = -1)

docs <-  chat %>% drop_na(author) %>% select(text) %>% filter(text != "<Media omitted>") %>% str_replace_all("<U\\+0001\\w{4}>","") %>% VectorSource() %>% Corpus()
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(tolower)) 

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
options(warn = oldw)
return(df)
}

lengthprep <- function(chat,users) {
avglen <- chat %>% drop_na() %>%  filter(author %in% users) %>% mutate(asd=str_replace_all(text, "[\\U0001f001-\\U0001ffff]"," <emoji> ")) %>% 
  mutate(asd=str_replace_all(asd, "[\\u2000-\\u2fff]","")) %>%
  mutate(wrdlen=str_count(.$asd,"\\S+")) %>% group_by(author) %>% summarise(avglen=mean(wrdlen))
avglen$author <- as.character(avglen$author) 
asd <- c("<br/>")
emojilst <- chat %>% drop_na(emoji_name)  %>% select(author,emoji_name) %>% unnest() %>% group_by(author,emoji_name) %>% tally() %>% top_n(n = 1, wt = n)
for (i in 1:nrow(avglen)) {
  asd <- c(asd,paste("<strong>",avglen[i,1], "</strong> on an averages writes <strong>",signif(avglen[i,2], digits = 2),"</strong> words in a message and the most used emoji is <strong>",
                     emojilst %>% filter(author==users[i]) %>% select(emoji_name) %>% pull(),
                     "</strong> with <strong>",emojilst %>% filter(author==users[i]) %>% select(n) %>% pull(),"</strong> usages <br/>  <br/>"))
}

return(asd)
}
