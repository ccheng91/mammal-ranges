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
