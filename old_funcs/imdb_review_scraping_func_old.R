require(rvest)
require(RColorBrewer)
require(SnowballC)
require(tm)
require(wordcloud)
require(XML)

myimdb.rangereviews <- function(weburl_r, range, filter_r = "best", start = 1, progress_bar=FALSE)
{
  if(progress_bar)
  {
    pb <- txtProgressBar(min = 0, max = range, char = "=", style = 3) 
  }
  first <- myimdb.reviews(weburl_r, filter = filter_r, page = start)
  for(i in (start+1):range)
  {
    addup = myimdb.reviews(weburl_r, filter = filter_r, page = i)
    first = c(first, addup)
    if(progress_bar)
    {
      setTxtProgressBar(pb, i) 
    }
  }
  if(progress_bar)
  {
    close(pb) 
  }
  return(first)
}


myimdb.reviews <- function(web_url, filter = "best", page = 1)
{
  page = (page-1)*10
  web_url = sapply(strsplit(web_url, split='?', fixed=TRUE), function(x) (x[1]))
  web_url = paste(web_url, "reviews?filter=", filter, ";spoiler=hide;start=", page, sep = "", collapse = NULL)
  grabe_page = read_html(web_url)
  reviews = html_nodes(grabe_page, xpath = '//*[@id="tn15content"]//p')
  reviews = html_text(reviews)
  reviews = reviews[-length(reviews)]
  return(reviews)
}