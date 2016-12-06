require(rvest)
require(RColorBrewer)
require(SnowballC)
require(tm)
require(wordcloud)
require(XML)
myfunc.wordcloud <- function(input_words, remove_words = c(NULL))
{
  pre_word = VCorpus(VectorSource(input_words))
  pre_word = tm_map(pre_word, stripWhitespace)
  pre_word = tm_map(pre_word, tolower)
  pre_word = tm_map(pre_word, removeWords, remove_words) 
  pre_word <- tm_map(pre_word, PlainTextDocument)
  pre_word <- tm_map(pre_word, stemDocument)
  wordcloud(pre_word, scale=c(2,0.1), max.words=50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
  testpic = recordPlot()
  plot.new()
  return(testpic)
}

myfunc.wordcloud(clean_reviews)
testpic
