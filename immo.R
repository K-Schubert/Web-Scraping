library(robotstxt)
library(rvest)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)

rm(list=ls())

## IMMOSTREET.CH

url_base <- c("https://www.immostreet.ch/fr/louer/appartement/a-geneve/?pageNum=%d")
paths_allowed(url_base)

map_df(1:16, function(i){
    
    cat('.')
    page <- read_html(sprintf(url_base, i))
    n_data <- length(html_text(html_nodes(page, ".amount")))
    data.frame(Price = html_text(html_nodes(page, ".amount")),
               Text = html_text(html_nodes(page, ".title"))[2:(n_data+1)],
               Location = html_text(html_nodes(page, ".location")),
               Link = str_extract(url_base, '[www].*[ch]') %>%
                 paste0(html_nodes(page, ".results-item a") %>%
                          extract2(5) %>%
                          html_attr('href')),
               stringsAsFactors = F)
  
}) -> flats_street

flats_street %>% head

dim(flats_street)

flats_street %>%
  select(Price) %>%
  lapply(., function(x) gsub("[P].*", '0', x)) %>%
  lapply(. , function(x) gsub("'", '', x)) %>%
  str_extract_all('\\d+') %>%
  unlist %>%
  as.numeric -> flats_street$Price

flats_street %>% 
  filter(Price > 0, Price < 1200) %>%
  arrange(Price)



###############################################
# IMMOSCOUT24.CH

url_base <- c("https://www.homegate.ch/louer/appartement/lieu-geneve/liste-annonces?tab=list&o=sortToplisting-desc&ep=%d")
paths_allowed(url_base)

map_df(1:5, function(i){
  
  cat('.')
  page <- read_html(sprintf(url_base, i))
  n_data <- length(html_text(html_nodes(page, ".amount")))
  data.frame(Price = page %>%
               html_nodes(".item-content-label") %>%
               html_text() %>%
               str_extract_all("\\d+[']*\\d*") %>%
               gsub("'", '', .) %>%
               as.numeric,
             Pieces = page %>%
               html_nodes(".box-row-item--attributes") %>%
               html_text() %>%
               str_extract_all("Pièces\n\\d[.]*\\d*") %>%
               str_extract_all('\\d[.]*\\d*') %>%
               as.numeric,
             Adress = page %>%
               html_nodes(".value span") %>%
               html_text(), 
             Link = page %>%
               html_nodes(".box-row-wrapper a") %>%
               html_attr('href') %>% 
               .[seq(2, length(.), 2)] %>%
               paste0('www.homegate.ch', .),
             stringsAsFactors = F)
  
}) -> flats_homegate

dim(flats_homegate)

flats_homegate %>% 
  filter(Price > 0, Price < 1500) %>%
  arrange(Price)

flats_homegate %>%
  filter(Pieces > 2, Price < 1200) %>%
  arrange(Price)

