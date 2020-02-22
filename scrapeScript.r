library(rvest)
library(tidyverse)
library(httr)

# get user agent for accessing the site
ua <- 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:72.0) Gecko/20100101 Firefox/72.0'

# specify top level url to scrape
url <- html_session('https://www.psychologytoday.com/us/therapists/profile-listings/montana')
url_session <- html_session('https://www.psychologytoday.com/us/therapists/profile-listings/montana', user_agent(ua))

# build function to find HTML attribute for pagination
get_pages <- function(html){
pages_data <- html %>%
#indicates HTML class
html_nodes('.btn-default') %>%
#extracts text as a list
html_text()
}

#read in url
page <- read_html(url)

#generate list of URL endings
url_endings <- get_pages(page)

#add each of these URL endings to the end of the main URL
list_of_pages <- str_c(url, '/', url_endings)
#change list_of_pages to only pages that are actually available (A-Z)
pages <- list_of_pages[11:36]

#number of pages to access
number_of_pages <- 26

# get names of each therapist
name <- function(html) {
  html %>%
  # The relevant tag
  html_nodes('.listing-title > h2') %>%
  #convert to text
  html_text()
}

# get title of each therapist
title <- function(html) {
  html %>%
  html_nodes('.bestField') %>%
  #convert to text
  html_text()
}

# get each therapist's locality
locality <- function(html) {
  html %>%
  #the relevant tag
  html_nodes('.textNoLink:nth-child(1) span:nth-child(1)') %>%
  html_text()
}

# get each therapist's latitude
latitude <- function(html) {
  html %>%
  html_nodes('button') %>%
  html_attr('data-map-lat') %>%
  na.omit() %>%
  unique() %>%
  as.numeric()
}

# get each therapist's longitude
longitude <- function(html) {
  html %>%
  html_nodes('button') %>%
  html_attr('data-map-lon') %>%
  #omit any null/NA values
  na.omit() %>%
  #remove duplicates
  unique() %>%
  #convert to numeric values
  as.numeric()
}

specialty <- function(html) {
  html %>%
  html_nodes('.top-border li') %>%
  html_text
}

# put all functions into a tibble
get_data_table <- function(html){
  # Extract the Basic information from the HTML
      names <- name(html)
      titles <- title(html)
      localities <- locality(html)

      #combine together
      combined_data <- tibble(name = names,
                              title = titles,
                              locality = localities)
}

get_data_from_url <- function(url) {
   html <- read_html(url)
   get_data_table(html)
}

scraper <- function(url) {
  pages %>%
        # Apply to all URLs
        map(get_data_from_url) %>%
        # Combine the tibbles into one tibble
        bind_rows() %>%
        # Write a tab-separated file
        write_tsv(str_c(therapists,'.tsv'))
    }


### access as HTML session ###
a <- html_session('https://www.psychologytoday.com/us/therapists/profile-listings/montana/a', user_agent(ua))
single_page <- read_html('https://www.psychologytoday.com/us/therapists/wellspring-counseling-family-services-helena-mt/477231')
single_page1 <- read_html('https://www.psychologytoday.com/us/therapists/susan-k-allen-missoula-mt/239461')
