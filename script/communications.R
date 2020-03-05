library(tidyverse)
library(rvest)
library(glue)

wdr <- getwd()

start.page <- "http://juris.ohchr.org/search/results/2?typeOfDecisionFilter=0&countryFilter=0&treatyFilter=0"

pages <- seq(1, 260,1)

df.links <- glue("http://juris.ohchr.org/search/results/{pages}?typeOfDecisionFilter=0&countryFilter=0&treatyFilter=0") %>% 
  enframe(name=NULL) %>% 
  mutate(value=as.character(value))



pb <- progress_estimated(nrow(df.links))

  
fn_scrap <- function(x) {
  
#  pb$tick()$print()
  
  x %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(.,fill=T) %>% 
    as.data.frame()

    }

df.all <- df.links$value[1:10] %>% 
  set_names() %>% 
  map_dfr(., possibly(fn_scrap, otherwise=NULL))

#write_csv2(df.all, paste0(wdr, "/data/communications.csv"))

coms <- readr::read_csv2(paste0(wdr, "/data/communications.csv")) %>% 
  janitor::clean_names() %>% 
  rename(date_of_adoption="date_of_adoption_of_the_views")

coms <- coms %>% 
  mutate(date_of_adoption=as.Date(date_of_adoption, format="%d %b %Y")) %>% 
  mutate(year_of_adoption=lubridate::year(date_of_adoption)) %>% 
  mutate(year_of_submission=str_extract(communications, "/[0-9]{4}")) %>% 
  mutate(year_of_submission=as.numeric(parse_number(year_of_submission)))

CAT<- coms %>% 
  filter(treaties=="CAT") %>% 
  mutate(articles.list=str_split(articles, "CAT")) %>% 
  mutate(articles.list=map(articles.list, str_replace, "-","")) %>% 
  mutate(articles_df=map(articles.list, as_tibble) %>% map(., ~filter(.x, nchar(value)>0))) #filters tibble
  
CAT.all<- CAT %>% 
  unnest(articles_df)
  
CAT.all %>% 
  filter(str_detect(value, "^3")) %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(year_of_adoption), fill=type_of_decisions),
           stat="count", position=position_stack())




coms %>% 
  filter(is.na(year_of_adoption)) #date is missing when inadmissible?


coms %>% 
  ggplot() +
  geom_bar(aes(x=treaties, fill=type_of_decisions),
           stat="count")

coms %>% 
  ggplot()+
  geom_bar(aes(x=as.factor(year_of_submission),
               fill=type_of_decisions),
           stat="count")+
  facet_wrap(vars(treaties), scales = "free")


coms %>% 
  ggplot()+
  geom_bar(aes(x=year_of_adoption),
           stat="count")+
  facet_wrap(vars(treaties))
