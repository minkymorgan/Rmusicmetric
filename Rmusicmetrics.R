###### Rmusicmetric: some functions for music metric's beta api #######################################
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

 library(quantmod)
 library(RJSONIO)
 library(TTR)


semetric.fest <- function(CHART="15903a3e868342d982196271b3c12ca4", TOKEN="1b2eb802b24f437a916f4e2154fe7c77") {
  uri <<- paste("http://apib2.semetric.com/chart/",CHART,"?token=",TOKEN, sep = "")
  mmc <<- fromJSON(paste("http://apib2.semetric.com/chart/",CHART,"?token=",TOKEN, sep = "")) 
  return(mmc)
}

semetric.lastfm <- function(ID="b5eccd4e8ae24cc49b80fedfe74581d1", TOKEN="1b2eb802b24f437a916f4e2154fe7c77") {
  uri <<- paste("http://apib2.semetric.com/artist/",ID,"/plays/lastfm?token=",TOKEN, sep = "")
  mmlastfm <<- fromJSON(paste("",uri,sep = "")) 
  mmlastfmdf <- as.data.frame(mmlastfm$response$data)
  xtslastfm <<- xts(mmlastfm$response$data,(ISOdate(1970,1,1)+mmlastfm$response$start_time)+((as.numeric(rownames(mmlastfmdf))-1)*mmlastfm$response$period))

  uri2 <<- paste("http://apib2.semetric.com/artist/",ID,"?token=",TOKEN, sep = "")
  mmid <<- fromJSON(paste("",uri2,sep = "")) 
  chartSeries(xtslastfm, name=mmid$response$name, theme="white")
  }

