library(tidyverse)
library(unjuris)

df_juris <- unjuris::juris_search(year_start="2019", 
                                  year_end="2020")
df_juris
names(df_juris)

juris_search(communication_number = "396/2009,")
