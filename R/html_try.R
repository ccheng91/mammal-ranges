library(rvest)
library(xml2)
library(selectr)
library(stringr)
library(jsonlite)


url_name <- 'https://scholar.google.ca/scholar?hl=en&as_sdt=0%2C5&q=Panthera+tigris&btnG='
wp <- xml2::read_html(url_name)
# Extract raw data
titles <- rvest::html_text(rvest::html_nodes(wp, '#gs_ab_md .gs_ab_mdw'))

kkk <- rvest::html_text(rvest::html_nodes(wp, 'gs_ab_mdw'))

wp %>%
html_node('#gs_ab_md') %>%
  html_text() 



library(RCurl)

library(XML)


GetGoogleResults <- function(keyword, service, key) {       
  library(RCurl)
  library(rjson)
  base_url <- "http://ajax.googleapis.com/ajax/services/search/"
  keyword <- gsub(" ", "+", keyword)
  query <- paste(base_url, service, "?v=1.0&q=", keyword, sep="")
  if(!is.null(key))
    query <- paste(query, "&key=", key, sep="")
  
  query <- paste(query, "&start=", 0, sep="")
  results <- fromJSON(getURL(query))
  return(results)
}

google <- GetGoogleResults("Tragulus javanicus", "web", key="AIzaSyBwmIU7LC4COWFD4B6zssFhfQOzkQcp_Pg")


GetGoogleResults


getURL(query)

query <- paste(query, "&key=", key, sep="")

query <- paste(query, "&start=", 0, sep="")
key="AIzaSyBwmIU7LC4COWFD4B6zssFhfQOzkQcp_Pg"

keyword <- "Tragulus javanicus"
service <- "web"

base_url <- "http://ajax.googleapis.com/ajax/services/search/"
keyword <- gsub(" ", "+", keyword)
query <- paste(base_url, service, "?v=1.0&q=", keyword, sep="")
if(!is.null(key))
  query <- paste(query, "&key=", key, sep="")

query <- paste(query, "&start=", 0, sep="")


res <- gtrendsR::gtrends(c("Tragulus javanicus"))


plot(res)








require(RCurl)

# ;pad the xml package
require(XML)


google.counts<-function(s){
  # take the variable "s" and paste it into a google search url
  search.url<-paste("http://www.google.com/search?q=",gsub(" ","+",s),sep="")
  # grab the html contents of the search results page
  search.html<-getURL(search.url)
  # format the html contents
  parse.search<-htmlTreeParse(search.html,useInternalNodes = TRUE)
  # find a div with the id "resultStats"
  search.nodes<-getNodeSet(parse.search,"//div[@id='resultStats']")
  # Take the entire tag, remove tags themselves (xmlValue), seperate every string by the spaces (strsplit), and take the second string (strsplit()[[1]][2]). 
  search.value<-strsplit(xmlValue(search.nodes[[1]])," ",fixed=TRUE)[[1]][2]
  # display, as numeric, the number of search results
  return(as.numeric(gsub(",","",search.value,fixed=TRUE)))
}


hits <- google.counts("tiger")


google.counts<-function(s){
  search.url<-paste("http://www.google.com/search?q=",gsub(" ","+",s),sep="")
  search.html<-getURL(search.url)
  parse.search<-htmlTreeParse(search.html,useInternalNodes = TRUE)
  search.nodes<-getNodeSet(parse.search,"//div[@id='resultStats']")
  search.value<-strsplit(xmlValue(search.nodes[[1]])," ",fixed=TRUE)[[1]][2]
  return(as.numeric(gsub(",","",search.value,fixed=TRUE)))
}







GoogleHits <- function(input)
{
  require(XML)
  require(RCurl)
  url <- paste("https://www.google.com/search?q=\"",
               input, "\"", sep = "")
  
  CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
  script <- getURL(url, followlocation = TRUE, cainfo = CAINFO)
  doc <- htmlParse(script)
  res <- xpathSApply(doc, '//*/div[@id="resultStats"]', xmlValue)
  cat(paste("\nYour Search URL:\n", url, "\n", sep = ""))
  cat("\nNo. of Hits:\n")
  return(as.integer(gsub("[^0-9]", "", res)))
}

hit <- GoogleHits("R%Statistical%Software")

