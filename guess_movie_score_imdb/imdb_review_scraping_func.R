require(rvest)
require(RColorBrewer)
require(SnowballC)
require(tm)
require(wordcloud)
require(XML)

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

myimdb.rating <- function(web_url)
{
  web_url = sapply(strsplit(web_url, split='?', fixed=TRUE), function(x) (x[1]))
  grabe_page = read_html(web_url)
  rating = html_nodes(grabe_page, xpath = '//*[@id="title-overview-widget"]/div[2]/div[2]/div/div[1]/div[1]/div[1]/strong/span')
  rating = html_text(rating)
  if(length(rating)==0)
  {
    rating="NO Score"
  }
  return(rating)
}

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

myimdb.search <- function(search_key_words)
{
  key_word = gsub(" ","+",search_key_words)
  web_url = paste("http://www.imdb.com/find?ref_=nv_sr_fn&q=", key_word,"&s=all", sep="")
  grabe_page = read_html(web_url)
  result_title = html_nodes(grabe_page, xpath = '//*[@class="findHeader"]')
  result_title = html_text(result_title)
  if(grepl("No results found for ", result_title, fixed = TRUE))
  {
    return(NULL)
  }
  search_list = html_nodes(grabe_page, xpath = '//*[@id="main"]/div/div[2]/table//*[@class="result_text"]')
  search_list2 = html_nodes(grabe_page, xpath = '//*[@id="main"]/div/div[2]/table//*[@class="result_text"]//a')
  search_list_title = html_text(search_list)
  search_list_link = html_attr(search_list2, "href")
  search_list_link = paste("http://www.imdb.com",search_list_link,sep = "")
  movie_rating=c(NULL)
  for(i in 1:length(search_list_link))
  {
    movie_rating = c(movie_rating, myimdb.rating(search_list_link[i]))
  }
  search_result = cbind(search_list_title, search_list_link, movie_rating)
  return(search_result)
}

#if(length(myimdb.search("operation"))==0){cat("notok")}else{cat("ok")}


