library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
library(googlesheets4)
library(fuzzyjoin)
library(crayon)
`%notin%`=Negate(`%in%`)
# setwd("C:/R/git/dreamleague")
a=Sys.time()
source("C:/R/git/dreamleague/R/dl-preprocessing.R")
gs4_auth(
  email = T
)
dl_d=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague24-25.xlsx", na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  dplyr::select(2:8)
managers_d=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague24-25.xlsx", na=c("SOLD"), sheet = "Table")%>% dplyr::select(3:4) %>% na.omit() %>% 
  rename(manager=1,
         team=2) %>% 
  filter(team!="TEAM")

out_d=dl_process(dl_d, managers_d, "Didsbury")

# sheet_write(out_d$scores, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="scores" )
# sheet_write(out_d$weekly, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="weekly" )
# sheet_write(out_d$daily, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="daily" )


dl_o=readxl::read_excel("C:/R/git/dreamleague/data/DL24-25.xlsx", na=c(""), sheet = "Stats", skip=0, col_names = F) %>%
  dplyr::select(1:7)
managers_o=readxl::read_excel("C:/R/git/dreamleague/data/DL24-25.xlsx", na=c("SOLD"), sheet = "Table")%>% dplyr::select(3:4) %>% na.omit() %>% 
  rename(manager=1,
         team=2) %>% 
  filter(team!="TEAM")

dl_o=dl_o %>% mutate(`...7`=case_when(`...2`=="JAMES TRAFFORD"~"45529",
                                      #`...2`=="FREDDIE LADAPO"~"45548",
                                      #`...2`=="HARRY SMITH"~NA_character_,
                                      `...2`=="NONI MADUEKE"~"45530",
                                      T~`...7`))



out_o=dl_process(dl_o, managers_o,"Original")
# out_o$weekly$SBgoals=0

# sheet_write(out_o$scores, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="scores_original" )
#sheet_write(out_o$weekly, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="weekly_original" )
# sheet_write(out_o$daily, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="daily_original" )

d=data.frame("update_time"=Sys.time())
sheet_write(d, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="update")

dl_d=out_d$scores
dl_o=out_o$scores
weekly_d=out_d$weekly
weekly_o=out_o$weekly
daily_o=out_o$daily
daily_d=out_d$daily
time=d
# weekly_d, weekly_o,

dl=rbind.data.frame(out_d$scores %>% mutate(league="didsbury"),
                    out_o$scores %>% mutate(league="original"))
managers=rbind.data.frame(managers_d %>% mutate(league="didsbury"),
                          managers_o %>% mutate(league="original"))
daily=rbind.data.frame(out_d$daily %>% mutate(league="didsbury"),
                       out_o$daily %>% mutate(league="original"))
# sheet_write(dl, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="scores" )
# sheet_write(daily, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="daily" )
  
save(dl,  daily, time, file="dreamleague/data.RDa")
# save(managers_o,managers_d, file="dreamleague/managers.RDa")

renv::activate()
rsconnect::deployApp("dreamleague")
b=Sys.time()

difftime(b,a, units = "mins")
