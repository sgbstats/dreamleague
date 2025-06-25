library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
library(googlesheets4)
library(fuzzyjoin)
library(crayon)
`%notin%`=Negate(`%in%`)
dl_2425=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague24-25.xlsx", na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  dplyr::select(2:8)
managers_2425=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague24-25.xlsx", na=c("SOLD"), sheet = "Table")%>% dplyr::select(3:4) %>% na.omit() %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

dl_2324=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague23-24.xlsx", na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  dplyr::select(2:8)
managers_2324=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague23-24.xlsx", na=c("SOLD"), sheet = "Table")%>% dplyr::select(3:4) %>% na.omit() %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

dl_2223=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague22-23.xlsx", na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  dplyr::select(2:8)
managers_2223=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague22-23.xlsx", na=c("SOLD"), sheet = "Table")%>% dplyr::select(3:4) %>% na.omit() %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

dl_2122=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague21-22.xlsx", na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  dplyr::select(2:8)
managers_2122=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague21-22.xlsx", na=c("SOLD"), sheet = "Table")%>% dplyr::select(3:4) %>% na.omit() %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

dl_2021=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague20-21.xlsx", na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  dplyr::select(2:8)
managers_2021=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague20-21.xlsx", na=c("SOLD"), sheet = "Table")%>% dplyr::select(3:4) %>% na.omit() %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

dl_1920mini=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague19-20 Mini League.xls", na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  dplyr::select(2:8)
managers_1920mini=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague19-20 Mini League.xls", na=c("SOLD"), sheet = "Table")%>% dplyr::select(3:4) %>% na.omit() %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

dl_1920=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague19-20.xls", na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  dplyr::select(2:8)
managers_1920=readxl::read_excel("C:/R/git/dreamleague/data/DreamLeague19-20.xls", na=c("SOLD"), sheet = "Table")%>% dplyr::select(3:4) %>% na.omit() %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

managers=rbind.data.frame(managers_2122,managers_2223, managers_2324,managers_2425, managers_2021, managers_1920mini, managers_1920)


teams=rbind.data.frame(dl_2122 %>% mutate(year="21-22"),
                    dl_2223 %>% mutate(year="22-23"),
                    dl_2324 %>% mutate(year="23-24"),
                    dl_2425 %>% mutate(year="24-25"),
                    dl_2021 %>% mutate(year="20-21"),
                    dl_1920mini %>% mutate(year="19-20mini"),
                    dl_1920 %>% mutate(year="19-20"))%>% 
  rename(position=1,
         player=2,
         club=3,
         cost=4,
         goals=5,
         sold=7,
         bought=6) %>% 
  mutate(team=NA_character_)

teams2=teams
for(i in 1:nrow(teams)){
  if(teams$player[i] %in% managers$team){
    team=teams$player[i]
  }
  if(teams$position[i] %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")){
    teams2$team[i]=team
  }
}

teams2=teams2 %>% mutate(cost=as.numeric(cost),
                         goals=case_when(goals=="TRANSFER"~0,
                                         position=="GOALKEEPER"~-as.numeric(goals),
                                         T~as.numeric(goals))) %>% 
  filter(position %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"))

teams2 %>% summarise(totalgoals=sum(goals)) %>% 
  arrange(-totalgoals)



managers2=managers %>% distinct()

teams2 %>% merge(managers2, by="team")%>% filter(goals>0) %>% 
  summarise(totalgoals=sum(goals), .by="year") %>% 
  arrange(-totalgoals)


teams2 %>% merge(managers2, by="team")%>% 
  summarise(totalgoals=sum(goals), .by=c( "year")) %>% 
  arrange(-totalgoals) %>% 
  mutate(rank=row_number(),.by="year") %>% 
  mutate(behind=totalgoals-max(totalgoals), .by="year") %>% 
  filter(rank!=1) %>% 
  arrange(-behind)

teams2 %>% filter(year=="24-25", !is.na(cost), goals==0) %>% 
  arrange(-cost)
  