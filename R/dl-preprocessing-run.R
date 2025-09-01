library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
library(googlesheets4)
library(fuzzyjoin)
library(crayon)
`%notin%`=Negate(`%in%`)
# setwd("C:/R/git/dreamleague")
# renv::activate(project = "C:/R/git/dreamleague")
a=Sys.time()
source("C:/R/git/dreamleague/R/dl-preprocessing.R")
# gs4_auth(
#   email = T
# )

file_d="C:/R/git/dreamleague/data/DreamLeague25-26.xlsx"
dl_d=readxl::read_excel(file_d, na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  dplyr::select(2:8)
managers_d=readxl::read_excel(file_d, na=c("SOLD"), sheet = "Stats")%>% dplyr::select(11:12) %>% na.omit() %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

mod_d=file.info(file_d)$mtime

out_d=dl_process(dl_d, managers_d, "Didsbury")
# 
# sheet_write(out_d$scores, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="scores" )
# sheet_write(out_d$weekly, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="weekly" )
# sheet_write(out_d$daily, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="daily" )

file_o="C:/R/git/dreamleague/data/DL25-26.xlsx"
dl_o=readxl::read_excel(file_o, na=c(""), sheet = "Stats", skip=0, col_names = F) %>%
  dplyr::select(1:7)
managers_o=readxl::read_excel(file_o, na=c("SOLD"), sheet = "Table", skip = 4)%>% dplyr::select(c(2,4))  %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

mod_o=file.info(file_o)$mtime

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

# sheet_write(d, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="update")

dl_d=out_d$scores
dl_o=out_o$scores
weekly_d=out_d$weekly
weekly_o=out_o$weekly
daily_o=out_o$daily
daily_d=out_d$daily
time=list("update_time"=Sys.time(), "mod_d"=mod_d,"mod_o"=mod_o)
# weekly_d, weekly_o,
cupties=read.csv("data/cupties.csv") %>% 
  mutate(date=as.Date(date, format = "%d/%m/%Y"))

dl=rbind.data.frame(out_d$scores %>% mutate(league="didsbury"),
                    out_o$scores %>% mutate(league="original"))
managers=rbind.data.frame(managers_d %>% mutate(league="didsbury"),
                          managers_o %>% mutate(league="original"))
daily=rbind.data.frame(out_d$daily %>% mutate(league="didsbury"),
                       out_o$daily %>% mutate(league="original"))
# sheet_write(dl, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="scores" )
# sheet_write(daily, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="daily" )

save(dl,  daily, time, cupties, file="C:/R/git/dreamleague/dreamleague/data.RDa")

googledrive::drive_auth(
  email = TRUE,
  path = NULL,
  subject = NULL,
  scopes = "drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

googledrive::drive_update(media="C:/R/git/dreamleague/dreamleague/data.RDa",
                          file=googledrive::as_id("108pNlDYjniFZiPU3PG82bIdChZmZGqUh"),)
b=Sys.time()

difftime(b,a, units = "mins")
