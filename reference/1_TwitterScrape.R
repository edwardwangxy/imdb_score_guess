# load libraries
library(twitteR)
library(ROAuth)

# directory to save the output
ofdir   <- "/Users/lenssen/Dropbox/TianWork/ElectionTeaching"

# setup the scraper
consumer_key <- "*****"
consumer_secret <- "*****"
access_token <- "*****"
access_secret <- "*****"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


# get donald tweets
N=1302  # tweets to request from each query
S=200  # radius in miles
lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,
       46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)

lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,
       -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)

#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul

donald=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Donald+Trump',
              lang="en",n=N,resultType="recent",
              geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

ofnameD <- sprintf("Donald%s%s.Rda",substring(Sys.time(),12,13),substring(Sys.time(),15,16))


save(donald,file=sprintf("%s/%s", ofdir,ofnameD))

# get hillary tweets
hillary=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Hillary+Clinton',
              lang="en",n=N,resultType="recent",
              geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

ofnameH <- sprintf("Hillary%s%s.Rda",substring(Sys.time(),12,13),substring(Sys.time(),15,16))

# save the output
save(hillary,file=sprintf("%s/%s", ofdir,ofnameH))





