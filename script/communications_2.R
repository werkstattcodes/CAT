library(tidyverse)
library(rvest)
library(glue)
library(xml2)

test_page  <- "http://juris.ohchr.org/search/results/2?typeOfDecisionFilter=0&countryFilter=0&treatyFilter=0"

pages <- seq(1, 52, 1)

df_links <- glue("http://juris.ohchr.org/search/results/{pages}?typeOfDecisionFilter=0&countryFilter=0&treatyFilter=0") %>% 
  enframe(value="page", name=NULL) %>% 
  mutate(page=as.character(page))


pb <- dplyr::progress_estimated(length(MP_links))


fn_scrap <- function(x) {
  
  #pb$tick()$print()
  
  x  %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(.,trim=T, fill=T) %>% 
    map_dfr(., as_tibble)
}

df.links$page[[1]] %>% 
  read_html()

main_link %>% 
  read_html() %>% 
  html_nodes("table")

get_test <- httr::GET(test_page)


# Get communication details -----------------------------------------------

test_communication <- "https://juris.ohchr.org/Search/Details/2625"

d <- test_communication %>% 
  read_html() #%>% 
  html_node("dd") %>% 
  html_table(., trim=T)
