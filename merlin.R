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

####################################################################################################
# FACTORY RIM BRAKE WHEELS

url_base <- c("https://www.merlincycles.com/")
class <- c("factory-road-wheels-75320/")
url <- paste0(url_base, class)
paths_allowed(url)

page <- read_html(url)

# Get product IDs
html_nodes(page, ".product") %>% 
  html_attr('data-id') -> ID 

# Get individual product path
html_nodes(page, ".product-title") %>%
  html_text %>%
  tolower %>%
  gsub(' ', '-', .) %>%
  gsub('---', '-', .) %>%
  gsub("'", '', .) -> product_path

# Create individual product link
paste0(url_base, product_path) %>%
  paste(ID, sep='-') %>%
  paste0('.html') -> product_links

n_products <- length(ID)

map_df(125:n_products, function(i){
  
  cat('.')
  
  tryCatch({ 
  
  # Get options of each specific product
  page <- read_html(product_links[i])
  
  html_nodes(page, ".product-title") %>%
    html_text -> product
  
  if (length(html_nodes(page, ".table")) == 0){
    next
  } else {
    html_nodes(page, ".table") %>% 
      html_table(fill=T) %>%
      extract2(1) -> opts
  }
  
  html_nodes(page, ".productOption") %>%
    html_attr('data-baseprice') -> price
  
  html_nodes(page, ".productOption") %>%
    html_attr('data-baserrp')  -> price_base
  
  opts %>% 
    select(-ncol(opts)) %>%
    mutate(Price = as.numeric(price), Base_Price = as.numeric(price_base), 
           Discount = (Base_Price-Price)/Base_Price) -> opts
  
  data.frame(Product = product, 
             Colour = opts$Colour,
             Price = opts$Price,
             Base_Price = opts$Base_Price,
             Discount = opts$Discount,
             Availability = opts$Availability,
             stringsAsFactors = F)
  
  print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}) -> merlin_rim_wheels


merlin_rim_wheels %>% head


map_df(1:1, function(i){
  
  cat('.')
  page <- read_html(url)
  data.frame(Brand = html_nodes(page, ".product") %>%
               html_attr('data-brand'),
             Product = html_text(html_nodes(page, ".product-title")),
             Price = html_nodes(page, ".product") %>%
               html_attr('data-baseprice') %>%
               gsub('\\|\\d.*', '', .) %>%
               as.numeric,
             Base_price = html_nodes(page, ".product") %>% 
               html_attr('data-baserrp') %>%
               as.numeric,
             Sale = html_nodes(page, ".product") %>% 
               html_attr('data-promotion') %>%
               { ifelse(is.na(.), "NO", "YES") },
             Options = html_nodes(page, ".product") %>%
               html_attr('data-baseprice') %>%
               str_count('\\|') %>%
               + 1,
             Stock = html_nodes(page, ".product") %>%
               html_attr('data-stock'),
             Colour =  html_nodes(page, ".product") %>%
               html_attr('data-colour'),
             Quantity = html_nodes(page, ".product") %>%
               html_attr('data-single_or_pair'),
             Tyre = html_nodes(page, ".product") %>%
               html_attr('data-tyre_type'), 
             Material = html_nodes(page, ".product") %>%
               html_attr('data-material'),
             Link = paste0('https://www.merlincycles.com/factory-road-wheels-',
                           html_nodes(page, ".product") %>% 
                             html_attr('data-id')),
             stringsAsFactors = F)
  
}) -> merlin_rim_wheels

merlin_rim_wheels %>% head

merlin_rim_wheels %>%
  select(Base_price, Price) %>%
  mutate(Savings=Base_price-Price)/Base_price -> merlin

merlin_rim_wheels %>%
  filter(Brand=='Fulcrum')

####################################################################################################
# FACTORY DISC BRAKE WHEELS

url_base <- c("https://www.merlincycles.com/factory-road-disc-wheels-92604/")
paths_allowed(url_base)

map_df(1:1, function(i){
  
  cat('.')
  page <- read_html(url_base)
  data.frame(Brand = html_nodes(page, ".product") %>%
               html_attr('data-brand'),
             Product = html_text(html_nodes(page, ".product-title")),
             Price = html_nodes(page, ".product") %>%
               html_attr('data-baseprice') %>%
               gsub('\\|\\d.*', '', .) %>%
               as.numeric,
             Base_price = html_nodes(page, ".product") %>% 
               html_attr('data-baserrp') %>%
               as.numeric,
             Sale = html_nodes(page, ".product") %>% 
               html_attr('data-promotion') %>%
               { ifelse(is.na(.), "NO", "YES") },
             Options = html_nodes(page, ".product") %>%
               html_attr('data-baseprice') %>%
               str_count('\\|') %>%
               + 1,
             Stock = html_nodes(page, ".product") %>%
               html_attr('data-stock'),
             Rotor_mount =  html_nodes(page, ".product") %>%
               html_attr('data-rotor_mount'),
             Axle = html_nodes(page, ".product") %>%
               html_attr('data-axle'),
             Link = paste0('https://www.merlincycles.com/factory-road-wheels-',
                           html_nodes(page, ".product") %>% 
                             html_attr('data-id')),
             stringsAsFactors = F)
  
}) -> merlin_disc_wheels


merlin_disc_wheels %>% head

################################################################################
# scrap product id from main page => iterate through all prods
# get in stock options and price and colour
# get comments and ratings
  
url_base <- c("https://www.merlincycles.com/")
class <- c("short-sleeved-cycling-jerseys-75341/")
url <- paste0(url_base, class)
paths_allowed(url)

page <- read_html(url)


# Get product IDs
html_nodes(page, ".product") %>% 
  html_attr('data-id') -> ID 

# Get individual product path
html_nodes(page, ".product-title") %>%
  html_text %>%
  tolower %>%
  gsub(' ', '-', .) %>%
  gsub('---', '-', .) %>%
  gsub("'", '', .) -> product_path

# Create individual product link
paste0(url_base, product_path) %>%
  paste(ID, sep='-') %>%
  paste0('.html') -> product_links

n_products <- length(ID)
  
map_df(1:(n_products-50), function(i){
  
  cat('.')
  
  # Get options of each specific product
  page <- read_html(product_links[i])
  
  html_nodes(page, ".product-title") %>%
    html_text -> product
  
  html_nodes(page, ".table") %>%
    html_table(fill=T) %>%
    extract2(1) -> options
  
  html_nodes(page, ".productOption") %>%
    html_attr('data-baseprice') -> price
  
  html_nodes(page, ".productOption") %>%
    html_attr('data-baserrp')  -> price_base
  
  options %>% 
    select(-'') %>%
    mutate(Price = as.numeric(price), Base_Price = as.numeric(price_base), 
           Discount = (Base_Price-Price)/Base_Price) -> options
      
    data.frame(Product = product, 
               Colour = options$Colour,
               Size = options$Size,
               Price = options$Price,
               Base_Price = options$Base_Price,
               Discount = options$Discount,
               Availability = options$Availability,
               stringsAsFactors = F)
  
}) -> merlin_jerseys
  

merlin_jerseys


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
