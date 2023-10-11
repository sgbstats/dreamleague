library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
library(googlesheets4)
library(fuzzyjoin)
`%notin%`=Negate(`%in%`)
# setwd("C:/R/git/dreamleague")

dl=readxl::read_excel("data/DreamLeague23-24.xlsx", na=c("SOLD"), sheet = "Stats")
gs4_auth(
  email = T
)
# dl=googlesheets4::read_sheet(ss="https://docs.google.com/spreadsheets/d/1hn2sqtUYCrMUCa9qW54MDxfZxpttG_KBuLHLpBzq_0M/edit?usp=sharing",
#                              na=c("SOLD","", NULL),
#                              col_names = F) %>% 
#   mutate(dummy=NA_character_) %>% 
#   relocate(dummy, .before = `...1`)

teams=dl %>% dplyr::select(2:8) %>% 
  rename(position=1,
         player=2,
         club=3,
         cost=4,
         goals=5,
         sold=7,
         bought=6) %>% 
  mutate(team=NA_character_)

managers=dl %>% dplyr::select(11:12) %>% na.omit() %>% 
  rename(manager=1,
         team=2) %>% 
  filter(team!="TEAM")

teams2=teams
for(i in 1:nrow(teams))
{
  if(teams$player[i] %in% managers$team)
  {
    team=teams$player[i]
  }
  if(teams$position[i] %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"))
  {
    teams2$team[i]=team
  }
}

# sb_id=read.csv("data/sb_id.csv") 
load("C:/R/git/dreamleague/data/ids.RDa")



teams3=teams2 %>% filter(position %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")) %>% 
  mutate(goals=if_else(position=="GOALKEEPER", -abs(as.numeric(goals)), as.numeric(goals)),
         bought=format(as.Date(bought), "%d-%b"),
         sold=format(as.Date(sold), "%d-%b")) 

# merge(team_id %>% mutate(team=str_to_upper(team)), by.x = "club", by.y="team", all.x = T)


outfield0=teams3 %>% filter(position %in% c( "DEFENDER", "MIDFIELDER", "FORWARD")) %>% 
  fuzzyjoin::stringdist_join(player_id %>% select(player, player_id,team) %>% 
                               filter(player_id %notin% c(70735, 182525)),
                             by="player", mode="left", method="jw", distance_col="dist") %>% 
  group_by(player.x) %>%
  slice_min(order_by=dist, n=1) %>% 
  ungroup() %>% 
  rename(team=team.x) %>%
  select(-team.y) %>% 
  mutate(bought2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", bought)~ paste(bought,"-2023", sep=""),
                                   grepl("Jan|Feb|Mar|Apr|May|Jun", bought)~paste(bought,"-2024", sep=""),
                                   T~"01-Jul-2023"), "%d-%b-%Y"),
         sold2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", sold)~ paste(sold,"-2023", sep=""),
                                 grepl("Jan|Feb|Mar|Apr|May|Jun", sold)~paste(sold,"-2024", sep=""),
                                 T~"30-Jun-2024"), "%d-%b-%Y"))

mismatch=outfield0 %>% filter(dist!=0|is.na(dist)) %>% 
  select(team, player.x, club, player.y, dist, player_id)

duplicates=outfield0 %>% count(player.y) %>% filter(n>1)

outfield=outfield0 %>% 
  group_by(player_id, team) %>% 
  slice_min(team, n=1, with_ties = F) %>%
  ungroup() %>% 
  mutate(SBgoals=0,
         SBapp=0) %>% 
  select(-player.y, -dist) %>% 
  rename("player"="player.x")

weekly=tribble(~"player_id", ~"Date", ~"Goals", ~"App", ~"team")

for(i in 1:nrow(outfield))
  # for(i in 1:10)
{
  skip_to_next <- FALSE
  # if(is.na(outfield$id[i])){next}
  
  url=paste("https://www.soccerbase.com/players/player.sd?player_id=",outfield$player_id[i],"&season_id=156",sep="")
  link=RCurl::getURL(url)  
  
  print(outfield$player[i])
  tryCatch({
    tables=readHTMLTable(link)
    
    if(outfield$player_id[i]==75804)
    {
      tables$tpg$V7[5]="1"
    }
    
    if(outfield$player_id[i]==52657)
    {
      tables$tpg$V7=""
    }
    appgoals=(tables$tpg) %>% filter(V1 %in% c("Premier League", 
                                               "EFL Cup", "English League Cup",
                                               "Europa League", 
                                               "Community Shield",
                                               "Champions League",
                                               "FA Cup", "English FA Cup",
                                               "Europa Conference League",
                                               "Championship", "Championship Play-Off",
                                               "League One", "League One Play-Off",
                                               "League Two", "League Two Play-Off" )) %>% 
      mutate(Date=as.Date(substr(V2,4,13), "%d%b %Y")) %>% 
      filter(Date>outfield$bought2[i],
             Date<outfield$sold2[i]) %>% 
      mutate(Goals=as.numeric(V7),
             App=1,
             Goals=if_else(is.na(Goals), 0, Goals),
             player_id=outfield$player_id[i],
             team=outfield$team[i]) 
    
    
    appgoals2=appgoals%>% 
      summarise(App=sum(App, na.rm=T),
                Goals=sum(Goals, na.rm=T))
    
    
    outfield$SBgoals[i]=appgoals2[1,2]
    
    outfield$SBapp[i]=appgoals2[1,1]
    
    weekly=rbind(weekly, appgoals %>% select(player_id, Date, Goals, App, team))
    
    print(outfield$SBgoals[i])
  },
  error = function(e) { skip_to_next <<- TRUE
  warning("Error")
  } 
  
  )
  
  if(skip_to_next) { next } 
  
}
# 
# test=outfield %>% filter(goals!=SBgoals, !is.na(id))
# load("Data/team_id.RDa")

gk=teams3 %>% filter(position %notin% c( "DEFENDER", "MIDFIELDER", "FORWARD")) %>% 
  merge(team_id %>% select(team, team_id) %>% 
          group_by(team) %>% 
          slice_min(team_id, with_ties = F) %>%
          ungroup()  %>% 
          mutate(team=str_to_upper(team)), by.x="club", by.y="team", all.x=T) %>% 
  mutate(bought2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", bought)~ paste(bought,"-2023", sep=""),
                                   grepl("Jan|Feb|Mar|Apr|May|Jun", bought)~paste(bought,"-2024", sep=""),
                                   T~"01-Jul-2023"), "%d-%b-%Y"),
         sold2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", sold)~ paste(sold,"-2023", sep=""),
                                 grepl("Jan|Feb|Mar|Apr|May|Jun", sold)~paste(sold,"-2024", sep=""),
                                 T~"30-Jun-2024"), "%d-%b-%Y")) %>% 
  mutate(SBgoals=0, SBapp=0) %>% 
  ungroup()

weekly_gk=tribble(~"team_id", ~"Date", ~"Goals", ~"App")

scraplinks2 <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  # url_ <- webpage %>%
  #   rvest::html_nodes("tbody") %>%
  #   rvest::html_attr("match")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("tbody") %>%
    rvest::html_text()
  
  
  return(tibble(link = link_))
}


lg="Premier League|EFL Cup|Europa League|Community Shield|Champions League|European Super Cup|FA Cup|English FA Cup|Europa Conference League|Championship|Championship Play-Off|League One|League One Play-Off|League Two|League Two Play-Off|English League Cup"


for(i in 1:nrow(gk))
  # for(i in 1:10)
{
  skip_to_next <- FALSE
  # if(is.na(outfield$id[i])){next}
  
  url=paste("https://www.soccerbase.com/teams/team.sd?team_id=",gk$team_id[i],"&teamTabs=results",sep="")
  link=RCurl::getURL(url)  
  
  print(gk$club[i])
  tryCatch({
    sl=scraplinks2(url)
    
    x=sl%>% group_by(row_number()) %>% 
      mutate(comp=str_extract(link, lg),
             date=as.Date(str_extract(link, "\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])"),"%Y-%m-%d"),
             score=str_extract(link, "\\d{1}[[:space:]]-[[:space:]]\\d{1}"),
             App=1) %>% 
      # group_by(row_number()) %>% 
      mutate(teampos=unlist(gregexpr("[a-z][[:space:]]1[[:space:]]\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])", link))[1],
             opppos=unlist(gregexpr("[a-z][[:space:]]2[[:space:]]\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])", link))[1]) %>% 
      ungroup() %>% 
      separate(score, into = c("H", "A"), sep="-") %>% 
      mutate(concede=-as.numeric(str_trim(if_else(teampos>opppos, H, A))))%>%
      filter(comp %in% c("Premier League", 
                         "EFL Cup", "English League Cup",
                         "Europa League", 
                         "Community Shield",
                         "Champions League", "European Super Cup",
                         "FA Cup", "English FA Cup",
                         "Europa Conference League",
                         "Championship", "Championship Play-Off",
                         "League One", "League One Play-Off",
                         "League Two", "League Two Play-Off" )) %>% 
      filter(date>gk$bought2[i],
             date<gk$sold2[i],
             !is.na(H)
      )
    
    
    x2=x%>%
      summarise(Goals=sum(concede, na.rm=T),
                App=sum(App, na.rm=T))
    gk$SBgoals[i]= x2[1,1]
    gk$SBapp[i]= x2[1,2]
    
    weekly_gk=rbind(weekly_gk, x %>% rename(Goals=concede,
                                            Date=date) %>%
                      select(Date, Goals, App) %>% 
                      mutate(team_id=gk$team_id[i]))
    
    
  },
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  
}

team_score=rbind(outfield%>% ungroup() %>% dplyr::select(-player_id) , gk %>% dplyr::select(-team_id) %>% 
                   mutate(SBgoals=as.numeric(SBgoals),
                          SBapp=as.numeric(SBapp))) %>% 
  ungroup() %>% 
  mutate(position=factor(position, levels=c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"), ordered = T)) %>% 
  mutate(cost2=as.numeric(cost)) %>% 
  arrange(team, position, -cost2, bought2) %>% 
  select(-cost2)

# gs4_deauth()


sheet_write(team_score, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="scores")

weekly2=weekly %>% merge(outfield %>% select(-SBgoals, -SBapp), by=c("player_id", "team"))
weekly_gk2=weekly_gk %>% merge(gk %>% select(-SBgoals, -SBapp), by="team_id")

seq.Date(as.Date("2023-07-26"), by=7, length.out = 52)

team_score_weekly=rbind.data.frame(weekly2 %>% select(-player_id), weekly_gk2 %>% select(-team_id)) %>% 
  ungroup() %>% 
  mutate(position=factor(position, levels=c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"), ordered = T)) %>% 
  mutate(cost2=as.numeric(cost)) %>% 
  mutate(week=floor_date(Date,"weeks",week_start = 1)) %>% 
  group_by(position, player, club, cost,cost2, bought, sold, bought2,sold2, team, week) %>% 
  summarise(SBgoals=sum(Goals, na.rm=T),
            App=sum(App, na.rm=T)) %>% 
  arrange(team, position, -cost2, bought2, week) %>% 
  ungroup()  


sheet_write(team_score_weekly, ss="https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0", sheet="weekly")
