library(tidyverse)
library(unjuris)

df_juris <- unjuris::juris_search(year_start="1900", 
                                  year_end="2020")
#1726

df_juirs2 <- unjuris::juris_search(decision_type="all")


readr::write_csv2(df_juris, here("data", "df_juris.csv"))

names(df_juris)


