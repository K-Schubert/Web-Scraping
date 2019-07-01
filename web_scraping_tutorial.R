install.packages("robotstxt")
install.packages("rvest")
install.packages("selectr")
install.packages("xml2")
install.packages("dplyr")
install.packages("stringr")
install.packages("forcats")
install.packages("magrittr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("tibble")
install.packages("purrr")
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

############
## AMAZON ##
############

# robotstxt access authorisation
paths_allowed(
  paths = c("https://www.amazon.in/mobile-phones/b?ie=UTF8&node=1389401031&ref_=nav_shopall_sbc_mobcomp_all_mobiles")
)

# xml2
top_phones <- read_html("https://www.amazon.in/mobile-phones/b?ie=UTF8&node=1389401031&ref_=nav_shopall_sbc_mobcomp_all_mobiles")
top_phones

# brand name
top_phones %>% 
  html_nodes(".crwTitle a") %>% #specify class: '.' before class name => specify id: '#' before id name
  html_text() %>% # extracts text between opening/closing tags
  str_split('\\(') %>%
  map_chr(1) %>%
  str_trim() -> mobile_name

mobile_name

# color
top_phones %>% 
  html_nodes(".crwTitle a") %>% #specify class: '.' before class name => specify id: '#' before id name
  html_text() %>% # extracts text between opening/closing tags
  str_split('\\(') %>%
  map_chr(2) %>%
  str_split(",") %>%
  map_chr(1) -> mobile_color

mobile_name

# rating




############
### IMDB ###
############

paths_allowed(
  paths = c("https://www.imdb.com/search/title?groups=top_250&sort=user_rating")
)

imdb <- read_html("https://www.imdb.com/search/title?groups=top_250&sort=user_rating")
imdb

imdb %>%
  html_nodes(".lister-item-content h3 a") %>%
  html_text() -> movie_title

movie_title

imdb %>%
  html_nodes(".lister-item-content h3 .lister-item-year") %>%
  html_text() %>%
  str_sub(start = 2, end = 5) %>%
  as.Date(format = "%Y") %>%
  year() -> movie_year

movie_year

imdb %>%
  html_nodes(".lister-item-content p .certificate") %>%
  html_text() -> movie_certificate

movie_certificate

imdb %>%
  html_nodes(".lister-item-content p .runtime") %>%
  html_text() %>%
  str_split(" ") %>%
  map_chr(1) %>%
  as.numeric() -> movie_runtime

movie_runtime

imdb %>%
  html_nodes(".lister-item-content p .genre") %>%
  html_text() %>%
  str_trim() -> movie_genre

movie_genre

imdb %>%
  html_nodes(".ratings-bar .ratings-imdb-rating") %>%
  html_attr("data-value") %>% 
  as.numeric() -> movie_rating

movie_rating

imdb %>%
  html_nodes(xpath = '//meta[@itemprop="ratingCount"]') %>% 
  html_attr('content') %>% 
  as.numeric() -> movie_votes

movie_votes

imdb %>%
  html_nodes(xpath = '//span[@name="nv"]') %>%
  html_text() %>%
  str_extract(pattern = "^\\$.*") %>%
  na.omit() %>%
  as.character() %>%
  append(values = NA, after = 30) %>%
  append(values = NA, after = 46) %>%
  str_sub(start = 2, end = nchar(.) - 1) %>%
  as.numeric() -> movie_revenue

movie_revenue

top_50 <- tibble(title = movie_title, release = movie_year, 
                 `runtime (mins)` = movie_runtime, genre = movie_genre, rating = movie_rating, 
                 votes = movie_votes, `revenue ($ millions)` = movie_revenue)

top_50


############
### RBI ####
############

paths_allowed(
  paths = c("https://en.wikipedia.org/wiki/List_of_Governors_of_Reserve_Bank_of_India")
)

rbi_guv <- read_html("https://en.wikipedia.org/wiki/List_of_Governors_of_Reserve_Bank_of_India")
rbi_guv

rbi_guv %>%
  html_nodes("table") %>%
  html_table() %>%
  extract2(2) -> profile

profile

profile %>%
  separate(`Term in office`, into = c("term", "days")) %>%
  select(Officeholder, term) %>%
  arrange(desc(as.numeric(term)))

profile %>%
  count(Background) 

profile %>%
  pull(Background) %>%
  fct_collapse(
    Bureaucrats = c("IAS officer", "ICS officer",
                    "Indian Administrative Service (IAS) officer",
                    "Indian Audit and Accounts Service officer",
                    "Indian Civil Service (ICS) officer"),
    `No Info` = c(""),
    `RBI Officer` = c("Career Reserve Bank of India officer")
  ) %>%
  fct_count() %>%
  rename(background = f, count = n) -> backgrounds

backgrounds

backgrounds %>%
  ggplot() +
  geom_col(aes(background, count), fill = "blue") +
  xlab("Background") + ylab("Count") +
  ggtitle("Background of RBI Governors")

############
### TdF ####
############

paths_allowed(
  paths = c("https://en.wikipedia.org/wiki/List_of_Tour_de_France_general_classification_winners")
)

tdf <- read_html("https://en.wikipedia.org/wiki/List_of_Tour_de_France_general_classification_winners")
tdf

tdf %>%
  html_nodes("table") %>%
  html_table(fill=T) %>%
  extract2(4) -> winners

winners

winners %>%
  separate(`Cyclist`, into = c("Cyclist", "Nationality"), sep=-6) %>%
  select(Cyclist, Nationality, Total, Years) %>%
  arrange(desc(as.numeric(Total))) %>%
  slice(1:(n()-1)) -> winners

#winners$Nationality %>% str_match("\\((.*)\\)") %>% as.list()

winners %>%
  count(Total) 

# Bar chart
winners %>%
  ggplot() +
  geom_bar(stat='count', aes(x=Total, fill=Nationality)) +
  scale_fill_brewer(palette = "Spectral") +
  ggtitle("Tdf Wins") +
  theme(plot.title = element_text(hjust = 0.5))

# Diverging bars
winners %>% 
  mutate(type=ifelse(Total < mean(Total), "below", "above")) %>%
  mutate(rel_avg=round(Total-mean(Total), 2)) %>%
  arrange(rel_avg) %>%
  ggplot(aes(x=factor(Cyclist, levels=Cyclist), y=rel_avg)) + 
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(name="Wins", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Number of wins compared to average wins", 
       title= "Diverging Bars") + 
  xlab('Cyclist') +
  coord_flip()

# Lollipop chart
winners %>% 
  mutate(type=ifelse(Total < mean(Total), "below", "above")) %>%
  mutate(rel_avg=round(Total-mean(Total), 2)) %>%
  arrange(rel_avg) %>%
  ggplot(aes(x=factor(Cyclist, levels=Cyclist), y=rel_avg, label=rel_avg)) + 
  geom_point(stat='identity', size=6, color='steelblue')  +
  geom_segment(aes(y = 0, 
                   x = factor(Cyclist, levels=Cyclist), 
                   yend = rel_avg, 
                   xend = factor(Cyclist, levels=Cyclist)), 
               color = "steelblue") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Number of wins compared to average wins") + 
  ylim(-1.5, 2.5) +
  xlab('Cyclist') + 
  coord_flip()

# Rankings
winners %>% 
  arrange(Total) %>%
  ggplot(aes(x=factor(Cyclist, levels=Cyclist), y=Total, fill=Cyclist)) + 
  geom_bar(stat='identity') + 
  #scale_fill_brewer(palette = "Spectral") + 
  theme(legend.position = "none") +
  xlab('Cyclist') + 
  ylab('Total Wins') + 
  coord_flip()
