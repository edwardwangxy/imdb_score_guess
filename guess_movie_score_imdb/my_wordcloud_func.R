require(rvest)
require(RColorBrewer)
require(SnowballC)
require(tm)
require(wordcloud)
require(XML)
myfunc.wordcloud <- function(input_words, remove_words = c("%null"))
{
  pre_word = VCorpus(VectorSource(input_words))
  pre_word = tm_map(pre_word, stripWhitespace)
  pre_word = tm_map(pre_word, tolower)
  pre_word = tm_map(pre_word, removeWords, stopwords("english"))
  if(remove_words[1] != "%null")
  {
    pre_word = tm_map(pre_word, removeWords, remove_words) 
  }
  pre_word <- tm_map(pre_word, PlainTextDocument)
  pre_word <- tm_map(pre_word, stemDocument)
  wordcloud(pre_word, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
}