library(tidyverse)
library(here)
library(rvest)
library(glue)

id <- seq(0, 3500, 1)
df_links <- glue("https://juris.ohchr.org/Search/Details/{id}") %>% 
  enframe(name=NULL) %>% 
  mutate(value=as.character(value))


pb <- progress_estimated(nrow(df_links))

fn_scrap <- function(page_link) {
  
  pb$tick()$print()
  
  df_headings <- page_link %>% 
    read_html() %>% 
    html_nodes("body > div > div > div.col-sm-10 > section > section > dl > dt") %>% 
    html_text(., trim=T) %>% 
    enframe(name=NULL, value="heading")
  #df_headings
  
  df_content <- page_link %>% 
    read_html() %>% 
    html_nodes("body > div > div > div.col-sm-10 > section > section > dl > dd") %>% 
    html_text(., trim=T) %>% 
    enframe(name=NULL, value="content")
  #df_content
  
  bind_cols(headings=df_headings, values=df_content) %>% 
    mutate(id=page_link)
  
  #print(page_link)
  
}

df_results_3000 <- df_links$value %>% 
  map_dfr(., possibly(fn_scrap, otherwise=NULL))

readr::write_csv2(df_results_3000, here("data", "df_results_3000.csv"))
df_results_3000 <- readr::read_csv2(here("data", "df_results_3000.csv"))

df_wide <- df_results_3000 %>% 
  pivot_wider(id_cols = id,
              names_from = heading,
              values_from = content) %>% 
  filter(!Committee=="")


n_distinct(df_wide$`Communication number(s)`) #2576

#test_country_twice <- fn_scrap("https://juris.ohchr.org/Search/Details/50")
