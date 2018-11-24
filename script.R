source('settings.R')

#getdata
library(twitteR)
library(base64enc)
setup_twitter_oauth(consumer_key=c.key, consumer_secret=c.sec, access_token=a.tok, access_secret=a.sec)
rstats<-searchTwitter(hashtag, n=numero, since=since)
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

library(wordcloud2)
#wordcloud(feria_clean)

#otra forma de armar el data frame
dtm <- TermDocumentMatrix(feria_clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

library(webshot)
my_graph <- wordcloud2(head(d, 100), backgroundColor = "white", size=2, fontFamily = "cambria", shape="square", color= "random-light")

library("htmlwidgets")
saveWidget(my_graph,"tmp.html",selfcontained = F)
webshot("tmp.html","fig.pdf", delay =5, vwidth = 480, vheight=480)