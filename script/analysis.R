
# packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(tidytext)
library(hrbrthemes)
library(ggforce)
library(extrafont)
library(ggtext)
library(paletteer)
library(ggiraph)
library(lubridate)
#extrafont::font_import()


# set theme ---------------------------------------------------------------

#bgr_color <- "#ecf0f7"
bgr_color <- "#ffffff"

hrbrthemes::update_geom_font_defaults(family="Roboto Condensed",
                                      size=4)

theme_rs <- function(){
  hrbrthemes::theme_ipsum_rc() %+replace%
    theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(linetype="dotted"),
        panel.grid.minor.x = element_line(linetype="dotted"),
        legend.position = "top",
        legend.justification = "left",
        legend.title = element_blank(),
        strip.text = element_text(face="bold", 
                                  hjust=0),
        plot.margin=margin(l=0, t=0.5, b=0.5, unit="cm"),
        plot.background=element_rect(fill=bgr_color, color="transparent"),
        plot.title = element_text(size = 14, 
                                  hjust=0,
                                  face="bold", 
                                  margin=margin(b=0.1, unit="cm")),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 12, 
                                     hjust=0,
                                     margin=margin(b=0.5, unit="cm"),
                                     color = "grey30"),
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "grey30", 
                                        margin=margin(t=0.5, unit="cm"),
                                        hjust = c(0, 1)))
}




my_caption <- c("Data: juris.ohchr.org/", "Analysis: Roland Schmidt | @zoowalk | <span style='color:black'>**werk.statt.codes**</span>")



# get data ----------------------------------------------------------------

df_results_3000 <- readr::read_csv2(here::here("data", "df_results_3000.csv")) %>% 
  pivot_wider(id_col=id,
              names_from=heading,
              values_from = content) %>% 
  filter(!Committee=="") %>% 
  clean_names() %>% 
  rename(communication_no=communication_number_s,
         date_submission=submission_date,
         date_views=date_of_adoption_of_the_views,
         date_admissibility=date_of_admissibility) %>% 
  mutate_at(vars(contains("date")), lubridate::dmy) %>% 
  mutate(communication_no=str_remove_all(communication_no, ","))




# > checks ----------------------------------------------------------------

n_distinct(df_results_3000$communication_no)
n_distinct(df_results_3000$id)
nrow(df_results_3000)

table(df_results_3000$committee, useNA = c("always"))

#number of entries on website
#Total: 

#CAT: 504
#ccpr: 1915
#ced: 1
#cedaw: 95
#cerd: 62
#cecsr: 40
#crc: 
#crpd: 34


n_distinct(df_results_3000$id)
#2651

df_juris <- readr::read_csv2(here("data", "df_juris.csv")) %>% 
  rename(communication_no=communications,
       #  date_submission=submission_date,
       committee=treaties,
       #date_admissibility=date_of_admissibility,
       date_views=date_of_adoption_of_the_views)
  

# check diffrences between data -------------------------------------------

n_distinct(df_results_3000$communication_no) #2576
n_distinct(df_juris$communication_no) #1581

df_missing <- anti_join(df_results_3000,
                        df_juris,
                        by=c("communication_no"))



# # communications received per committee ---------------------------------

df_results_3000 %>% 
  group_by(committee) %>% 
  summarise(n_obs=n()) %>% 
  ungroup() %>% 
  mutate(n_rel=n_obs/sum(n_obs)*100) %>% 
  ggplot()+
  labs(title="Total number of communications received per treaty",
       caption=my_caption)+
  geom_bar(aes(y=reorder(committee, n_obs),
               x=n_obs,
               fill=committee),
           stat="identity")+
  geom_text(aes(y=reorder(committee, n_obs),
                x=n_obs,
                label=paste0(n_obs,
                            " (", round(n_rel, 1), "%)")),
            nudge_x = 110)+
  scale_x_continuous(expand=expansion(mult = c(0, 0.1)))+
  scale_fill_viridis_d(option="inferno")+
  theme_rs()+
  theme(panel.grid.major.y = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_text(face="bold"))


# communication received by year ------------------------------------------

plot_coms_per_year <- df_results_3000 %>% 
  group_by(year=lubridate::year(date_submission), 
           committee) %>% 
  summarise(n_submissions_year=n()) %>% 
  group_by(committee) %>% 
  mutate(total_committee=sum(n_submissions_year)) %>% 
  mutate(facet_label=paste(committee, "(total: ", total_committee,")")) %>% 
  ggplot()+
  labs(title="Number of communciations submitted per year per treaty",
      caption=my_caption)+
  geom_bar_interactive(aes(x=year,
               y=n_submissions_year,
               fill=committee,
               tooltip=paste0("year: ", year,
                             "\n",
                             "# coms: ", n_submissions_year)),
           stat="identity")+
  facet_wrap(vars(facet_label),
            # labeller = labeller(~paste(., "x")),
             scales="free_y")+
  scale_fill_viridis_d(option = "inferno")+
  theme_rs()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot_coms_per_year <- girafe(ggobj=plot_coms_per_year,
                        options=list(opts_tooltip(css = "background-color:#323E4F; 
                                 color: white;
                                 font-family:Roboto Condensed;",
                                                  delay_mouseout = 5000)),
                        pointsize=6,
                        width_svg=10, 
                        height_svg=7)
plot_coms_per_year


# communications concluded per year ---------------------------------------


# > final date --------------------------------------------------------------
#error in data
df_results_3000 <- df_results_3000 %>% 
  mutate(date_submission=case_when(communication_no=="156/1983" ~ as.Date("1983/08/08"),
                                   TRUE ~ as.Date(date_submission))) %>% 
  mutate(date_admissibility=case_when(communication_no=="025/2002" ~ as.Date("2003/03/19"),
                                      TRUE ~ as.Date(date_admissibility)))

df_results_3000 <- df_results_3000 %>% 
  mutate(date_final=coalesce(date_views, date_admissibility)) %>% 
  #filter(!is.na(date_admissibility) & !is.na(date_views)) %>% 
  mutate(duration_days=date_final-date_submission) %>% 
  mutate(duration_year=lubridate::as.duration(duration_days)/dyears(1))



# > ins and outs ----------------------------------------------------------

plot_pending_communications <- df_results_3000 %>% 
  ungroup() %>% 
  select(committee, communication_no, date_submission, date_final) %>% 
  mutate_at(vars(contains("date")), .funs=list(year=lubridate::year))

plot_pending_communications_submitted <- plot_pending_communications %>% 
  select(committee, communication_no, date_submission_year) %>% 
  group_by(committee, date_submission_year) %>% 
  summarise(n_obs=n()) %>% 
  mutate(status="submitted") %>% 
  rename(year=date_submission_year)

plot_pending_communications_finalized <- plot_pending_communications %>% 
  select(committee, communication_no, date_final_year) %>% 
  group_by(committee, date_final_year) %>% 
  summarise(n_obs=n()) %>% 
  mutate(status="concluded") %>% 
  rename(year=date_final_year)

plot_ins_out <- bind_rows(plot_pending_communications_submitted,
          plot_pending_communications_finalized) %>% 
  ggplot()+
  labs(title="Number of submitted and concluded communications per year and treaty",
       subtitle="Note different scales on y axis.",
       caption=my_caption)+
  geom_bar_interactive(aes(x=year,
               y=n_obs,
               tooltip=paste("year: ", year, "\n",
                             "status: ", status, "\n",
                             "# coms: ", n_obs),
               fill=status),
           stat="identity",
           position=position_dodge(preserve="single"))+
  facet_wrap(vars(committee),
             scales="free_y")+
  scale_fill_manual(values=c("submitted"="firebrick", "concluded"="navy"))+
  theme_rs()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


plot_ins_out <- girafe(ggobj=plot_ins_out,
                             options=list(opts_tooltip(css = "background-color:#323E4F; 
                                 color: white;
                                 font-family:Roboto Condensed;",
                                                       delay_mouseout = 5000)),
                             pointsize=6,
                             width_svg=10, 
                             height_svg=7)
plot_ins_out


# pending cases -----------------------------------------------------------

libray(padr)

df_pending <- df_results_3000 %>% 
  select(committee, communication_no, date_submission, date_final) %>% 
  mutate_at(vars(contains("date")), .funs=list(year=lubridate::year)) %>% 
  select(committee, communication_no, matches("^date.*year$")) %>% 
  pivot_longer(cols=contains("date"), names_to = "year_name",
               values_to="year") %>% 
  mutate(year=lubridate::ymd(year, truncated=2L)) %>% 
  ungroup() %>% 
  padr::pad(interval="year", group=c("communication_no","committee")) %>% 
  group_by(communication_no) %>% 
  arrange(year, .by_group=T) %>% 
  tidyr::fill(committee, .direction=c("down"))

df_pending %>% 
  group_by(committee, year) %>% 
  summarise(n_obs=n()) %>% 
  group_by(committee) %>% 
  mutate(max_n_obs=max(n_obs,na.rm=T),
         max_year=year[max_n_obs==n_obs]) %>% 
  ggplot()+
  labs(title="Number of pending communicaitons per year and treaty",
       caption=my_caption)+
  # geom_line(aes(x=year,
  #               y=n_obs))+
  geom_area(aes(x=year,
                y=n_obs,
                fill=committee))+
  geom_text(data=. %>% filter(year==max_year) %>% 
              slice(1), aes(x=max_year,
                y=n_obs,
                label=n_obs),
            nudge_y=30)+
  facet_wrap(vars(committee))+
  theme_rs()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(margin=margin(b=0.5, unit="cm")))


u <- x %>% 
  filter(str_detect(communication_no, "444/2010"))
  






# duration of communication ----------------------------------------------------------------

library(gghalves)

df_results_3000 %>% 
  group_by(committee) %>% 
  mutate(median_duration=median(duration_year, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot()+
  labs(title="Duration of communications per treaty",
       subtitle=str_wrap("Time from submission to final conclusion in years (either inadmissiblity decision or
       adoption of views). Median indicated.", 130),
       y="time from submission to conclusion in years",
       caption=my_caption)+
  gghalves::geom_half_boxplot(aes(y=duration_year,
                                  x=reorder(committee, median_duration),
                                  fill=committee),
                              outlier.shape = NA,
                              side="l")+
  gghalves::geom_half_point(aes(y=duration_year,
                                x=reorder(committee, median_duration),
                                color=committee),
                            side="r",
                            range_scale = 0.7)+
  geom_text(aes(y=median_duration,
                x=reorder(committee, median_duration),
                label=round(median_duration, 2),
                color=committee),
            #nudge_y=3,
            nudge_x=-0.5,
            check_overlap = T)+
  scale_y_continuous(breaks=c(seq(0, 5, 1), 10, 15))+
  coord_flip()+
  theme_rs()+
  theme(panel.grid.major.y = element_blank(),
        legend.position = "none",
        axis.text.y=element_text(face="bold"),
        axis.title.y=element_blank())


x5 <- df_results_3000 %>% 
  filter(duration_year>10)


# > check -----------------------------------------------------------------
#why are CRPD missing
df_results_3000 %>% 
  filter(str_detect(committee, "CRPD"))



# # country overall -------------------------------------------------------


# > split countries -------------------------------------------------------


#problem: more than one country can be addresee of complain; DenmarkIran
#how to split

#detect coms with multiple addresses
#df_multiple_addresses <- 
  
df_results_countries <- df_results_3000 %>% 
 # select(contains("countries")) %>% 
  # mutate(country_split=str_split(countries, 
  #                               "(?<=[A-Z][a-z]{0,20}+[a-z:space:]{0,20}+)(?=[A-Z])")) %>% 
  mutate(country_split=str_split(countries, 
                                 "(?<=[a-z)])(?=[A-Z])")) %>% 
  unnest(country_split)

df_results_long$country_split  

df_results_countries <- df_results_countries %>% 
  mutate(country_split=case_when(str_detect(country_split, "United Kingdom") ~ "UK",
                             TRUE ~ as.character(country_split)))

df_results_countries %>%
  mutate(country_l=fct_lump_n(country_split, n=15)) %>% 
  group_by(country_l) %>% 
  summarise(n_obs=n()) %>% 
  ggplot()+
  geom_bar(aes(y=reorder(country_l, n_obs),
               x=n_obs),
           stat="identity")




# # country & by committee -------------------------------------------------------

  my_breaks1 <- function(x) {
    new_x=max(x)
    old_max <- new_x/1.5/1.3
    old_max
    my_pretty=labeling::extended(0, old_max, m=5)
    my_pretty=signif(my_pretty, digits=-2)
    my_pretty=head(unique(my_pretty), -1)
    my_pretty=c(my_pretty, old_max)
    my_pretty
    
  }  

#split needed to lump within group  
library(tidytext)
df_results_countries %>% 
  group_split(committee) %>% 
  map_dfr(., ~mutate(., country_lump=fct_lump_n(country_split, n=10))) %>% 
  group_by(committee, country_lump) %>% 
  summarise(n_obs=n()) %>% 
  group_by(committee) %>% 
  mutate(max_n_obs=max(n_obs, na.rm=T)) %>% 
  mutate(rel_n_obs=n_obs/sum(n_obs)*100) %>% 
  ggplot()+
  geom_bar(aes(y=tidytext::reorder_within(country_lump, by = n_obs, within=committee),
               x=n_obs,
               fill=committee),
           stat="identity")+
  geom_text(aes(y=tidytext::reorder_within(country_lump, by = n_obs, within=committee),
                x=max_n_obs*1.50,
                label=paste0(n_obs,
                            " (", 
                            round(rel_n_obs, 2), 
                            "%)")),
            hjust=1,
            check_overlap = T)+
  tidytext::scale_y_reordered()+
  scale_x_continuous(expand = expansion(mult=c(0, 0.3)),
                    minor_breaks=NULL,
                    breaks=my_breaks1)+
  scale_fill_viridis_d()+
  facet_wrap(vars(committee),
             scales="free")+
  theme_rs()+
  theme(panel.grid.major.y = element_blank(),
        legend.position="none")


# Articles ----------------------------------------------------------------

# > split articles --------------------------------------------------------

df_articles_availability <- df_results_3000 %>% 
  select(id, articles, type_of_decision) %>% 
  mutate(article_yes_no=case_when(nchar(articles)>0 ~ "yes",
                                  TRUE ~ "no"))

library(gtsummary)
table(df_articles_availability$article_yes_no, useNA=c("always"))
#275 


# df_results_3000_article_long <- df_results_3000 %>% 
#   mutate(articles_divided=str_replace_all(articles, "(?<=.)CAT", ", CAT") %>%
#            str_replace_all(., "(?<=.)CESCR", ", CESCR") %>%
#            str_replace_all(., "(?<=.)CCPR", ", CCPR") %>%
#            str_replace_all(., "(?<=.)CEDAW", ", CEDAW") %>%
#            str_replace_all(., "(?<=.)CERD", ", CERD") %>%
#            str_replace_all(., "(?<=.)CRPD", ", CPRD") %>%
#            str_replace_all(., "(?<=.)CED", ", CED")) %>%
#   mutate(articles_divided_split=str_split(articles_divided, ", ")) %>%
#   unnest(articles_divided_split) %>%
#   mutate(articles_clean=str_remove_all(articles_divided_split, "^[A-Z]*-(?=[0-9])")) %>%
#   mutate(article_main=str_extract(articles_clean, "^[0-9]{1,2}"))

u <- df_results_3000 %>% 
  mutate(articles_split=stringr::str_split(articles, "(?<=[0-9,a-z])(?=[A-Z])")) %>% 
  select(contains("articles")) %>% 
  mutate(articles_num=map_dbl(articles_split, length)) %>% 
  unnest(articles_split) %>% 
  mutate(articles_split=str_remove_all(articles_split, "^[A-Z]*-(?=[0-9])"))
  
  
  
  
df_results_3000_long %>% 
  distinct(id, committee, article_main, type_of_decision) %>% 
  group_by(committee, article_main, type_of_decision) %>% 
  summarise(n_obs=n()) %>% 
  ggplot()+
  geom_bar(aes(y=tidytext::reorder_within(article_main, by=n_obs,
                                          within=committee),
                 x=n_obs,
               fill=type_of_decision),
           stat="identity")+
  tidytext::scale_y_reordered()+
  facet_wrap(vars(committee),
#                     space="free",
             scales="free")+
  theme_ipsum_rc()


# combination of articles -------------------------------------------------

#ggupset w subgroup totals





# admissible --------------------------------------------------------------

df_results_3000 %>% 
  group_by(committee, type_of_decision) %>% 
  summarise(n_obs=n()) %>% 
  ggplot()+
  geom_bar(aes(y=committee,
               x=n_obs,
               fill=type_of_decision),
           stat="identity",
           position="fill")

df_results_3000 %>% 
  filter()


