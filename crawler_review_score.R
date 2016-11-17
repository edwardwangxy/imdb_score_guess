library(rvest)
library(RColorBrewer)
library(SnowballC)
library(tm)
library(wordcloud)
library(XML)

myimdb.revscore <- function(web_url, filter = "best", page = 2){
  page = (page-1)*10
  web_url = sapply(strsplit(web_url, split='?', fixed=TRUE), function(x) (x[1]))
  web_url = paste(web_url, "reviews?filter=", filter, ";spoiler=hide;start=", page, sep = "", collapse = NULL)
  lego_review = read_html(web_url)
  reviews = html_nodes(lego_review, xpath = '//*[@id="tn15content"]//p')
  reviews = html_text(reviews)
  reviews = reviews[-length(reviews)]
 # reviews = reviews[- grep("review may contain spoilers", reviews)]
  scores = html_attr(html_nodes(lego_review, "#tn15content img"), "alt")
  scores = scores[!is.na(scores)]
  scores = scores[-1]
  scores = scores[-length(scores)]
  scores = sapply(strsplit(scores, split='/', fixed=TRUE), function(x) (x[1]))
  #get rid of mismatch scores and review pages
  if(length(scores) != length(reviews))
  {
  #  warning("Scores and Reviews not match ignore this page")
    return() 
  }
  revscor = data.frame(scores, reviews)
  return(revscor)
}

myimdb.reviews <- function(web_url, filter = "best", page = 1)
{
  page = (page-1)*10
  web_url = sapply(strsplit(web_url, split='?', fixed=TRUE), function(x) (x[1]))
  web_url = paste(web_url, "reviews?filter=", filter, ";spoiler=hide;start=", page, sep = "", collapse = NULL)
  lego_review = read_html(web_url)
  reviews = html_nodes(lego_review, xpath = '//*[@id="tn15content"]//p')
  reviews = html_text(reviews)
  reviews = reviews[-length(reviews)]
  return(reviews)
}

myimdb.rangereviews <- function(weburl_r, range, filter_r = "best", start = 1)
{
  first <- myimdb.reviews(weburl_r, filter = filter_r, page = start)
  for(i in (start+1):range)
  {
    addup = myimdb.reviews(weburl_r, filter = filter_r, page = i)
    first = c(first, addup)
  }
  return(first)
}


myfunc.wordcloud <- function(input_words, remove_words = c(NULL))
{
  test_pure = paste(input_words, collapse = ". ")
  test1 = VCorpus(VectorSource(test_pure))
  test1 = tm_map(test1, stripWhitespace)
  test1 = tm_map(test1, tolower)
  test1 = tm_map(test1, removeWords, stopwords("english"))
  test1 = tm_map(test1, removeWords, remove_words)
  test1 <- tm_map(test1, PlainTextDocument)
  test1 <- tm_map(test1, stemDocument)
  wordcloud(test1, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
}

#testout function
testfn = myimdb.revscore("http://www.imdb.com/title/tt1490017/?ref_=tt_urv")
View(testfn)

#web_url = "http://www.imdb.com/title/tt1490017/?ref_=tt_urv"
web_url = "http://www.imdb.com/title/tt0111161/"
web_url = sapply(strsplit(web_url, split='?', fixed=TRUE), function(x) (x[1]))
web_url = paste(web_url, "reviews?start=", sep = "", collapse = NULL)
web_url

testnum=5
newurl = paste(web_url, (testnum*10), sep = "", collapse = NULL)
testreview = myimdb.reviews(web_url)

testfnrange = myimdb.rangereviews(web_url, filter_r = "hate", range = 30, start = 1)
#read and bind multiple pages

for(i in 2:10)
{
    addingdata = myimdb.reviews("http://www.imdb.com/title/tt1490017/?ref_=tt_urv", filter = "love", page = i)
    testreview = c(testreview, addingdata) 
}

#raw function for retrieveing website
lego_review = read_html("http://www.imdb.com/title/tt1490017/reviews?start=0")
reviews = html_nodes(lego_review, xpath = '//*[@id="tn15content"]//p')
reviews = html_text(reviews)
reviews = reviews[-length(reviews)]
reviews = reviews[- grep("review may contain spoilers", reviews)]
scores = html_attr(html_nodes(lego_review, "#tn15content img "), "alt")
scores = scores[!is.na(scores)]
scores = scores[-1]
scores = scores[-length(scores)]
scores = sapply(strsplit(scores, split='/', fixed=TRUE), function(x) (x[1]))
revscor = data.frame(scores, reviews)
myimdb.wordcloud(testfnrange)
#wordcloud test
#test_pure = paste(testfnrange, collapse = ". ")
test1 = VCorpus(VectorSource(reviews))
test1 = tm_map(test1, stripWhitespace)
test1 = tm_map(test1, tolower)
test1 = tm_map(test1, removeWords, stopwords("english"))
test1 = tm_map(test1, removeWords, c("lego","movie","movi","will","film", "one", "just", "like", "can", "use","set","anim","story"))
test1 <- tm_map(test1, PlainTextDocument)
test1 <- tm_map(test1, stemDocument)
wordcloud(test1, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
