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
# Sept 27 2011: semetric.ts() dynamically constructs the data for successful calls only, solving chart errors               
# Oct  13 2011: fixes for errs where data zeroed or missing etc, repointed default tokens, added unit test 

 # note I've added in some more dependent packages. You may need to install them. All avaialble on CRAN via your console.
 library(quantmod)
 library(RJSONIO)
 library(corrgram)
 library(lattice)


# note: replace <addyourtokenhere> with your own developer key.
token <<- "<addyourtokenhere>"

semetric.chartlist <- function(CHART="15903a3e868342d982196271b3c12ca4", TOKEN=token) {
  # semetric.chartlist()
  # Purpose: Retrieves a list of artists for a festival with CHART="<id>", outputs a "chartlist"
  # Notes:
  # In finanical timeseries, a core concept is the "watchlist", a list of stocks you track.
  # In the music industry, there's a concept of a chart, which is like a watchlist in many respects.
  # I think in terms of nomenclature, I'll refer in the code to a "chartlist" which is "a list of artists"
  # If we have lists of artists like that, a dashboard illustrating comparitive/aggregate stats for it makes sense... :-)

  # Based on the concept of a chartlist, this function's purpose is to extract a festival based chartlist from the api exposed ones,
  # Later this object could feed into a dashboard to track many artists.
  # If user defined chartlists would have the same format as this output, dashboards we develop can today be festival based
  # and later be reused to track a portfolio of artists for a user.
 
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

semetric.uri.artist <- function(GUID="lastfm:the+beatles", DATASET="plays", NETWORK="lastfm", TOKEN=token) {
     # semetric.uri.artist()
     # purpose: to build out a URL to access the musicmetric api, for any artist, dataset, and network.
     # Notes:
     # this returns a correctly formatted URI the other functions can use to retrieve data from the API
     # by making this DRY, if host of the API changes, or the url scheme changes, it is easy to remap and alter it.

     uri <- paste("http://apib2.semetric.com/artist/",GUID,"/",DATASET,"/",NETWORK,"?token=",TOKEN, sep = "")
     return(uri)
  }

semetric.ts.artist <- function(ID="b5eccd4e8ae24cc49b80fedfe74581d1", DATASET="plays", NETWORK="lastfm", TOKEN=token) {
       # semetric.ts.artist()
       # purpose: to retrieve a particular timeseries captured for an artist.
       # Notes:
       # Greg was after a more generic api access function allowing dataset, and network to be parameters. this is it!

     # build the url

       uri <- semetric.uri.artist(ID, DATASET, NETWORK, TOKEN)

     # below we fetch the JSON from music metric

       mm <<- fromJSON(paste("",uri,sep = ""))

       # below a trick is to convert it to a dataframe, so we can list the row numbers out explicitly     

       mmdf <- as.data.frame(mm$response$data)

     # below is the magic. xts is defined with a list of values and a list of matching timestamps.
     # so we feed it the response timeseries data, and then convert the epoch start/end and period values into a
     # runlength of reconstituted timestamps that match the raw timseries values. These timestamps effectively index the timeseries in an xts object

       xtsmm <<- xts(mm$response$data,(ISOdate(1970,1,1)+mm$response$start_time)+((as.numeric(rownames(mmdf))-1)*mm$response$period))

     # working with IDs is not human friendly. fetch the artist name...

       uri2 <<- paste("http://apib2.semetric.com/artist/",ID,"?token=",TOKEN, sep = "")
       mmid <<- fromJSON(paste("",uri2,sep = ""))

     # now we have an xts chartable object, and the artist name. call the quantmod chart library to plot it

       chartSeries(xtsmm, name=paste(mmid$response[3],": ",NETWORK," ",DATASET,sep=""), theme="white")
       addEMA(n=30)
    }

 
semetric.ts <- function(ID="b5eccd4e8ae24cc49b80fedfe74581d1", TOKEN=token , c=1) {
  # semetric.ts()
  # Purpose: Retrieve all the datasets for an artist, merge and table-ize it, and chart it via quantmod
  # notes:
  # This function makes several calls to the api to fetch all the datasets for all the networks we know exist.
  # The only assumption the function makes is that there are lastFM plays available. The other timeseries data
  # is tested for existence, and everything that exists is eventually charted.
  # note to self. Table-ize is possible as XTS can merge on timestamp gracefully if the data has ragged edges.
  # which I see that much of it does. XTS is great.
  # I have now added in some additional charting options. try c=1, c=2, c=3 for different views of the data.
  # I have also added in c=4, for an experimental view of plays versus fans on lastfm, using hexbin library.

  # first fetch the artists name, working with IDs alone is for robots, not me. 
  uri2 <<- paste("http://apib2.semetric.com/artist/",ID,"?token=",TOKEN, sep = "")
  mmid <<- fromJSON(paste("",uri2,sep = ""))

  # I'm too sleepy to figure out fancy list driven looping through the params...
  # so bear with my non-DRY way to fetch the data and bind it together. One day I'll look at fixing it.
 
  # Define the various api metadata. Types of data exist on different channels, so list channels for a dataset.
  fans <- c("lastfm", "facebook", "youtube")
  plays <- c("lastfm", "youtube")
  # comments # the "comments" api dataset is omitted for the moment


  ## I googled the magic way to dynamically generate the dataframe, from the list of successful data calls
  ## check out: http://stackoverflow.com/questions/5542542/using-cbind-on-an-arbitrarily-long-list-of-objects
  ## my thanks to the authors of the answers.
  # (ns <- LETTERS[1:2])
  # obj.list <- lapply(ns, get)
  # names(obj.list) <- ns
  # do.call(cbind, obj.list)

  # get lastfm_plays 
  mmts  <<- fromJSON(paste(semetric.uri.artist(GUID=ID,DATASET="plays", NETWORK="lastfm")))
 
  panel <- ""

  if (mmts$success == "TRUE") {
     mmtsdf <<- as.data.frame(mmts$response$data)
     lastfm_plays <<- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))   
     panel <- "lastfm_plays"
     }

  # get lastfm fans
  mmts  <<- fromJSON(paste(semetric.uri.artist(GUID=ID,DATASET="fans", NETWORK="lastfm")))

  if (mmts$success == "TRUE") {
     mmtsdf <<- as.data.frame(mmts$response$data)
     lastfm_fans <<- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))
     panel <- c(panel, "lastfm_fans")
    }
 
  #get youtube plays
  mmts  <<- fromJSON(paste(semetric.uri.artist(GUID=ID,DATASET="plays", NETWORK="youtube")))

  if (mmts$success == "TRUE") {
     mmtsdf <<- as.data.frame(mmts$response$data)
     youtube_plays <<- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))
     panel <- c(panel, "youtube_plays")
     }
  # get youtube fans

  mmts  <<- fromJSON(paste(semetric.uri.artist(GUID=ID,DATASET="fans", NETWORK="youtube")))

  if (mmts$success == "TRUE") {
       mmtsdf <<- as.data.frame(mmts$response$data)
       youtube_fans <<- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))
       panel <- c(panel, "youtube_fans")
    }
  # get facebook fans
  mmts  <<- fromJSON(paste(semetric.uri.artist(GUID=ID,DATASET="fans", NETWORK="facebook")))

  if (mmts$success == "TRUE") {
     mmtsdf <<- as.data.frame(mmts$response$data)
     facebook_fans <<- xts(mmts$response$data,(ISOdate(1970,1,1)+mmts$response$start_time)+((as.numeric(rownames(mmtsdf))-1)*mmts$response$period))
     panel <- c(panel, "facebook_fans")
  }
 
  # bind all the timeseries together now. note the double arrow <<-, meaning I save this to the session globally
  # which allows chartSeries to work normally, plus means you can play with it yourself

    tstable <<- is.na.data.frame(panel)
   
  obj.panel <<- lapply(panel, get)
  names(obj.panel) <<- panel
  obj.table <<- do.call(cbind, obj.panel)
  tstable <<- as.data.frame(obj.table)
 
  # the now bound columns need a final convert and column rename so it's suitable for charting and working with 
  x_tstable <<- as.matrix(tstable)
  mode(x_tstable) <<- "numeric"
  xts_tstable <<- as.xts(x_tstable)
	
  if (c==1) {
	  # here are the calls to chart the whole dataset in a nice Artist Timeseries Profile page.
	
	  chartSeries(xts_tstable$lastfm_plays, name=paste(mmid$response[3],":lastfm plays"), theme="white")
	  plot(addEMA(n=10, col="black"))
	
	  if (length(xts_tstable$lastfm_fans) >0) {
      plot(addTA(xts_tstable$lastfm_fans, legend="lastfm fans", col="green"))
	  }
	  if (length(xts_tstable$youtube_fans) >0) {
	     plot(addTA(xts_tstable$youtube_fans, legend="youtube fans", col="blue"))
	  }
	  if (length(xts_tstable$youtube_plays) >0) {
	     plot(addTA(xts_tstable$youtube_plays, legend="youtube plays", col="blue"))
	  }
	  if (length(xts_tstable$facebook_fans) >0) {
	     plot(addTA(xts_tstable$facebook_fans, legend="facebook fans", col="red"))
	  }
  } 
  if (c==2) {
         corrgram(as.data.frame(xts_tstable), main=paste(mmid$response[3]))
  }

  if (c==3) {
	  pairs(x_tstable, main=paste(mmid$response[3]) )
  }
  if (c==4)  {
  p <- lastfm_plays
  f <- lastfm_fans
  t <- paste(mmid$response[3], " lastfm plays vs fans")
             plot(hexbin(f,p, xbins=15), colramp= function(n){LinOCS(n,beg=230,end=25)}, main=t) 
  }



  # I've commented out the next line, which would dump the table-ized data to the screen. uncomment if you like it
   return(tail(xts_tstable))

  # to test this script you can run the following from the R console:
  #     semetric.chart() # to find artist IDs
  #     semetric.ts(ID="e1bd6911146942b88d3918d99bb0c459")

}

semetric.unit <- function(ID="e1bd6911146942b88d3918d99bb0c459", TOKEN=token){
  # semetric.unit()
  # purpose: this is a superquick unit test function to exercise the helper functions.
  # Notes
  # Nothing fancy done to trap errors for the moment, just makes all the calls once with defaults.

  semetric.uri.artist()
  semetric.ts.artist()
  semetric.ts()
  semetric.chartlist()
  semetric.ts.artist()
}





