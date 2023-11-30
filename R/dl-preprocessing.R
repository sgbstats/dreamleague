library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
`%notin%`=Negate(`%in%`)

dl=readxl::read_excel("data/DreamLeague22-23.xlsx", na=c("SOLD"))

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
load("data/ids.RDa")

teams3=teams2 %>% filter(position %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")) %>% 
  mutate(goals=if_else(position=="GOALKEEPER", -abs(as.numeric(goals)), as.numeric(goals)),
         bought=format(as.Date(bought), "%d-%b"),
         sold=format(as.Date(sold), "%d-%b")) 

# merge(team_id %>% mutate(team=str_to_upper(team)), by.x = "club", by.y="team", all.x = T)


outfield=teams3 %>% filter(position %in% c( "DEFENDER", "MIDFIELDER", "FORWARD")) %>% 
  fuzzyjoin::stringdist_join(player_id %>% select(player, player_id), by="player", mode="left", method="jw", max_dist=0.15, distance_col="dist") %>% 
  group_by(player.x) %>%
  slice_min(order_by=dist, n=1) %>% 
  mutate(bought2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", bought)~ paste(bought,"-2022", sep=""),
                                   grepl("Jan|Feb|Mar|Apr|May|Jun", bought)~paste(bought,"-2023", sep=""),
                                   T~"01-Jul-2022"), "%d-%b-%Y"),
         sold2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", sold)~ paste(sold,"-2022", sep=""),
                                 grepl("Jan|Feb|Mar|Apr|May|Jun", sold)~paste(sold,"-2023", sep=""),
                                 T~"30-Jun-2023"), "%d-%b-%Y")) %>% 
  mutate(SBgoals=0)

for(i in 1:nrow(outfield))
  # for(i in 1:10)
{
  skip_to_next <- FALSE
  # if(is.na(outfield$id[i])){next}
  
  url=paste("https://www.soccerbase.com/players/player.sd?player_id=",outfield$player_id[i],"&season_id=155",sep="")
  link=RCurl::getURL(url)  
  
  print(outfield$player[i])
  tryCatch({
    tables=readHTMLTable(link)
    outfield$SBgoals[i]=((tables$tpg) %>% filter(V1 %in% c("Premier League", 
                                                           "EFL Cup",
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
                           mutate(Goals=as.numeric(V7)) %>% 
                           summarise(Goals=sum(Goals, na.rm=T)))[1,1]
    
  },
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  
}

test=outfield %>% filter(goals!=SBgoals, !is.na(id))
load("data/team_id.RDa")

gk=teams3 %>% filter(position %notin% c( "DEFENDER", "MIDFIELDER", "FORWARD")) %>% 
  merge(team_id %>% select(team, id) %>% 
          group_by(team) %>% 
          slice_min(id, with_ties = F) %>%
          ungroup()  %>% 
          mutate(team=str_to_upper(team)), by.x="club", by.y="team", all.x=T) %>% 
  mutate(bought2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", bought)~ paste(bought,"-2022", sep=""),
                                   grepl("Jan|Feb|Mar|Apr|May|Jun", bought)~paste(bought,"-2023", sep=""),
                                   T~"01-Jul-2022"), "%d-%b-%Y"),
         sold2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", sold)~ paste(sold,"-2022", sep=""),
                                 grepl("Jan|Feb|Mar|Apr|May|Jun", sold)~paste(sold,"-2023", sep=""),
                                 T~"30-Jun-2023"), "%d-%b-%Y")) %>% 
  mutate(SBgoals=0) %>% 
  ungroup()

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

# x=scraplinks2(url)
lg="Premier League|EFL Cup|Europa League|Community Shield|Champions League|FA Cup|English FA Cup|Europa Conference League|Championship|Championship Play-Off|League One|League One Play-Off|League Two|League Two Play-Off"
# x2=x  %>% mutate(comp=str_extract(link, lg),
#                  date=str_extract(link, "\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])"),
#                  score=str_extract(link, "\\d{1}[[:space:]]-[[:space:]]\\d{1}")) %>% 
#   group_by(row_number()) %>% 
#   mutate(teampos=unlist(gregexpr("[a-z][[:space:]]1[[:space:]]\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])", link))[1],
#          opppos=unlist(gregexpr("[a-z][[:space:]]2[[:space:]]\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])", link))[1]) %>% 
#   ungroup() %>% 
#   separate(score, into = c("H", "A"), sep="-") %>% 
#   mutate(concede=-as.numeric(str_trim(if_else(teampos>opppos, H, A))))


for(i in 1:nrow(gk))
  # for(i in 1:10)
{
  skip_to_next <- FALSE
  # if(is.na(outfield$id[i])){next}
  
  url=paste("https://www.soccerbase.com/teams/team.sd?team_id=",gk$id[i],"&teamTabs=results&season_id=155",sep="")
  link=RCurl::getURL(url)  
  
  print(gk$club[i])
  tryCatch({
    sl=scraplinks2(url)
    gk$SBgoals[i]= (sl%>% group_by(row_number()) %>% 
                      mutate(comp=str_extract(link, lg),
                                date=as.Date(str_extract(link, "\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])"),"%Y-%m-%d"),
                                score=str_extract(link, "\\d{1}[[:space:]]-[[:space:]]\\d{1}")) %>% 
      # group_by(row_number()) %>% 
      mutate(teampos=unlist(gregexpr("[a-z][[:space:]]1[[:space:]]\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])", link))[1],
             opppos=unlist(gregexpr("[a-z][[:space:]]2[[:space:]]\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])", link))[1]) %>% 
      ungroup() %>% 
      separate(score, into = c("H", "A"), sep="-") %>% 
      mutate(concede=-as.numeric(str_trim(if_else(teampos>opppos, H, A))))%>%
      filter(comp %in% c("Premier League", 
                         "EFL Cup",
                         "Europa League", 
                         "Community Shield",
                         "Champions League",
                         "FA Cup", "English FA Cup",
                         "Europa Conference League",
                         "Championship", "Championship Play-Off",
                         "League One", "League One Play-Off",
                         "League Two", "League Two Play-Off" )) %>% 
      filter(date>gk$bought2[i],
             date<gk$sold2[i]
             ) %>%
      summarise(Goals=sum(concede, na.rm=T)))[1,1]
  
  },
  error = function(e) { skip_to_next <<- TRUE})

if(skip_to_next) { next } 

}



league=managers %>% merge(teams2 %>% group_by(team) %>% 
                            summarise(total=sum(goals)), by="team") %>% 
  merge(teams2 %>% filter(goals>0) %>% group_by(team) %>% 
          summarise(gf=sum(goals)), by="team") %>% 
  merge(teams2 %>% filter(goals<0) %>% group_by(team) %>% 
          summarise(ga=-sum(goals)), by="team") %>% 
  arrange(-total,-gf)

save(teams2, league, file = "dreamleague22-23/Data.RDa")
save(teams2, league, file = "data/Data.RDa")
