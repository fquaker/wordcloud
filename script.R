source('settings.R')

#getdata
library(twitteR)
library(base64enc)
setup_twitter_oauth(consumer_key=c.key, consumer_secret=c.sec, access_token=a.tok, access_secret=a.sec)
rstats<-searchTwitter("#FeriaNacPop", n=9999, since="2018-06-01")
feria.libro<- do.call("rbind", lapply(rstats, as.data.frame))

#corpus
feria.libro_text<- sapply(rstats, function (x) x$getText())
str(feria.libro_text)

library(tm)
feria_corpus<- Corpus (VectorSource(feria.libro_text))
feria_corpus
inspect(feria_corpus[1])

#limpieza
feria_clean<- tm_map(feria_corpus, removePunctuation)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
feria_clean <- tm_map(feria_clean, removeSpecialChars)
feria_clean<- tm_map(feria_clean, content_transformer(tolower))
feria_clean <- tm_map(feria_clean, removeNumbers)
feria_clean <- tm_map(feria_clean, removeWords, stopwords("spanish"))
feria_clean <- tm_map(feria_clean, stripWhitespace)

library(wordcloud)
#wordcloud(feria_clean)

#otra forma de armar el data frame
dtm <- TermDocumentMatrix(feria_clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(words=d$word, freq=d$freq, max.words = 100, random.order = FALSE, colors = rainbow(400))
