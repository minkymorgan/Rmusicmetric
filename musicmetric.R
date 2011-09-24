######### Rmusicmetric: some functions for music metric's beta api #######################################
#
# Copyright (c) 2011 Andrew Morgan, minkymorgan at gmail dot com
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and 
# associated documentation files (the "Software"), to deal in the Software without restriction, including 
# without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
# sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject 
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial 
# portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT 
# NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#
######################################################################################################
# history:
# Sept 24 2011: restructed all the calls, delivered generic api fetcher, built full artist profile chart


 library(quantmod)
 library(RJSONIO)

# note: replace <addyourtokenhere> with your own developer key. 

semetric.chart <- function(CHART="15903a3e868342d982196271b3c12ca4", TOKEN="<addyourtokenhere>"
  ) {
  
  # Notes. In finanical timeseries, a core concept is the "watchlist", a list of stocks you track. 
  # In the music industry, there's a concept of a chart, which is like a watchlist in many respects.
  # I think in terms of nomenclature, I'll refer in the code to a "chartlist" which is "a list of artists"
  # If we have lists like that, a dashboard illustrating comparitive/aggregate stats for it makes sense... :-)

  # Based on that, this function's purpose is to extract a festival based chartlist from the api exposed ones,
  # that later can feed directly into an R dashboard. User defined chartlists would have the same format as this output.
  # It means dashboards we develop can be festival based now, or user defined later (say by record label for example?)
 
  uri <<- paste("http://apib2.semetric.com/chart/",CHART,"?token=",TOKEN, sep = "")
  mmc <<- fromJSON(paste("http://apib2.semetric.com/chart/",CHART,"?token=",TOKEN, sep = "")) 
  echoID <- function(Entry=1){mmc$response$entities[[Entry]]$id}
  echoName <- function(Entry=1){mmc$response$entities[[Entry]]$name}
  # echoMuz <- function(Entry=1){mmc$response$entities[[Entry]]$musicbrainz}
  ID <-sapply(1:length(mmc$response$entities),echoID)
  NAME <-sapply(1:length(mmc$response$entities),echoName)
  # MUSICBRAINZ <-sapply(1:length(mmc$response$entities),echoMuz)
  chartlist <<- as.data.frame(cbind(ID,NAME), 1:length(mmc$response$entities))  
  return(chartlist)
   
}

semetric.lastfm <- function(ID="b5eccd4e8ae24cc49b80fedfe74581d1", TOKEN="<addyourtokenhere>"
) {
  # depreciated. Left as an example to others.
  uri <<- paste("http://apib2.semetric.com/artist/",ID,"/plays/lastfm?token=",TOKEN, sep = "")
  mmlastfm <<- fromJSON(paste("",uri,sep = "")) 
  mmlastfmdf <- as.data.frame(mmlastfm$response$data)
  xtslastfm <<- xts(mmlastfm$response$data,(ISOdate(1970,1,1)+mmlastfm$response$start_time)+((as.numeric(rownames(mmlastfmdf))-1)*mmlastfm$response$period))
  uri2 <<- paste("http://apib2.semetric.com/artist/",ID,"?token=",TOKEN, sep = "")
  mmid <<- fromJSON(paste("",uri2,sep = "")) 
  chartSeries(xtslastfm, name=mmid$response$name, theme="white")
  }

semetric.uri.artist <- function(GUID="b5eccd4e8ae24cc49b80fedfe74581d1", DATASET="plays", NETWORK="lastfm", #TOKEN="<addyourtokenhere>"
    TOKEN="1b2eb802b24f437a916f4e2154fe7c77"
     ) {
	 # I have broken out the Artist URI builder into it's own topline function, to incl in new functions...
     # to fetch json, just use: fromJSON(semetric.uri.artist(DATASET="fans",NETWORK="youtube")) 
     uri <- paste("http://apib2.semetric.com/artist/",GUID,"/",DATASET,"/",NETWORK,"?token=",TOKEN, sep = "")
     return(uri)
  }

semetric.ts.artist <- function(ID="b5eccd4e8ae24cc49b80fedfe74581d1", DATASET="plays", NETWORK="lastfm", TOKEN="<addyourtokenhere>"
     ) {
	 # Greg was after a more generic api access function that allowed the dataset and network to be params. here it is
	 # build the url
	 uri <- paste("http://apib2.semetric.com/artist/",ID,"/",DATASET,"/",NETWORK,"?token=",TOKEN, sep = "")
     # I've left the variable names tagged with lastfm, but it is now dynamic off the params above.
	 # below we fetch the JSON from music metric
     mmlastfm <<- fromJSON(paste("",uri,sep = ""))
     # below a trick is to convert it to a dataframe, so we can list the row numbers out explicitly	 
     mmlastfmdf <- as.data.frame(mmlastfm$response$data)
	 # below is the magic. xts is defined with a list of values and a list of matching timestamps.
	 # so we feed it the response timeseries data, and then convert the epoch start/end and period values into a 
	 # runlength of reconstituted timestamps that match the raw timseries values. These timestamps effectively index the timeseries in an xts object
     xtslastfm <<- xts(mmlastfm$response$data,(ISOdate(1970,1,1)+mmlastfm$response$start_time)+((as.numeric(rownames(mmlastfmdf))-1)*mmlastfm$response$period))
	 # working with IDs is not human friendly. fetch the artist name...
     uri2 <<- paste("http://apib2.semetric.com/artist/",ID,"?token=",TOKEN, sep = "")
     mmid <<- fromJSON(paste("",uri2,sep = "")) 
	 # now we have an xts chartable object, and the artist name. call the quantmod chart to plot it
     chartSeries(xtslastfm, name=paste(mmid$response$name,": ",NETWORK," ",DATASET,sep=""), theme="white")
    }

  
semetric.ts <- function(ID="b5eccd4e8ae24cc49b80fedfe74581d1", TOKEN="<addyourtokenhere>"
) {
  # notes:
  # Greg enhancement my lastfm prototype function, making it generic for any api parameters
  # I like his approach: Make the api call function generic. Obvious really. 
  # But, the sad truth is actually users tell me they love excel, and like small multiples in dashboards. 
  # Boo.
  # So rather than fetching lots of little bits of ts in lots of calls, lets grab everything, and table-ize it,
  # to deliver not a generic function, but a simple-to-use one-size-fits-all function, called by users once.
  # Leverage this by plotting the master artist combo chart at the end

  # note to self. Table-ize is possible as XTS can merge on timestamp gracefully if the data has ragged edges.
  # which I see that much of it does. XTS is great.

  # first fetch the artists name, working with IDs alone is for robots, not me.  
  uri2 <<- paste("http://apib2.semetric.com/artist/",ID,"?token=",TOKEN, sep = "")
  mmid <<- fromJSON(paste("",uri2,sep = "")) 

  # I'm too sleepy to figure out fancy list driven looping through the params...
  # so bear with my non-DRY way to fetch the data and bind it together. One day I'll look at fixing it.
  
  # Define the various api metadata. Types of data exist on different channels, so list channels for a dataset.
  fans <- c("lastfm", "facebook", "youtube")
  plays <- c("lastfm", "youtube")
  # comments # the "comments" api dataset is omitted for the moment

  # get lastfm_plays  
  mmts  <- fromJSON(paste(semetric.uri.artist(DATASET="plays", NETWORK="lastfm"))) 
  mmtsdf <- as.data.frame(mmts$response$data)
  lastfm_plays <- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))

  # get lastfm fans
  mmts  <- fromJSON(paste(semetric.uri.artist(DATASET="fans", NETWORK="lastfm"))) 
  mmtsdf <- as.data.frame(mmts$response$data)
  lastfm_fans <- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))
  
  #get youtube plays
  mmts  <- fromJSON(paste(semetric.uri.artist(DATASET="plays", NETWORK="youtube"))) 
  mmtsdf <- as.data.frame(mmts$response$data)
  youtube_plays <- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))
  
  # get youtube fans
  mmts  <- fromJSON(paste(semetric.uri.artist(DATASET="fans", NETWORK="youtube"))) 
  mmtsdf <- as.data.frame(mmts$response$data)
  youtube_fans <- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))

  # get facebook fans
  mmts  <- fromJSON(paste(semetric.uri.artist(DATASET="fans", NETWORK="facebook"))) 
  mmtsdf <- as.data.frame(mmts$response$data)
  facebook_fans <- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))
  
  # bind all the timeseries together now. note the double arrow <<-, meaning I save this to the session globally
  # which allows chartSeries to work normally, plus means you can play with it yourself

  tstable <<- as.data.frame(cbind(lastfm_plays, lastfm_fans, youtube_plays, youtube_fans, facebook_fans))  

  # the now bound columns needs a final convert and column rename so it's suitable for charting and working with  
  x_tstable <- as.matrix(tstable)
  mode(x_tstable) <- "numeric"
  xts_tstable <- as.xts(x_tstable)
  names(xts_tstable)[1]="lastfm_plays"
  names(xts_tstable)[2]="lastfm_fans"
  names(xts_tstable)[3]="youtube_plays"
  names(xts_tstable)[4]="youtube_fans"
  names(xts_tstable)[5]="facebook_fans"
   
  # here are the calls to chart the whole dataset in a nice Artist Timeseries Profile page. 
  chartSeries(xts_tstable$lastfm_plays, name=paste(mmid$response$name,":lastfm plays"), theme="white")
  plot(addTA(xts_tstable$lastfm_fans, legend="lastfm fans", col="green"))
  plot(addTA(xts_tstable$youtube_fans, legend="youtube fans", col="blue"))
  plot(addTA(xts_tstable$youtube_plays, legend="youtube plays", col="blue"))
  plot(addTA(xts_tstable$youtube_plays, legend="facebook fans", col="red"))
  
  # I've commented out the next line, which would dump the table-ized data to the screen. uncomment if you like it
  # return(xts_tstable)

  # to test this script you can run the following from the R console:
  #     semetric.ts(ID="e1bd6911146942b88d3918d99bb0c459")
  }

  