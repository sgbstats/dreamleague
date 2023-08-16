library(XML)
library(tidyverse)
library(RCurl)
library(readxl)
library(openxlsx)
library(beepr)
library(chron)
library(tibble)
library(chron)
library(googlesheets4)
library(rvest)
`%notin%`=Negate(`%in%`)
# url="https://www.soccerbase.com/teams/team.sd?team_id=1718"
# url="https://www.soccerbase.com/teams/team.sd?team_id=2898"
# link=getURL(url)  
# tables=readHTMLTable(link)
# 
# writeLines(link, "C:\\Users\\mbbx4sb5\\OneDrive - MANCHESTER UNIVERSITY NHS FOUNDATION TRUST\\Personal\\test.txt")
# 
# tables[2]$`NULL`[4,1]
tictoc::tic()
team=character(2898)
id=1:2898
for(i in 1:2898)
{
  skip_to_next <- FALSE
  url=paste("https://www.soccerbase.com/teams/team.sd?team_id=",i,sep="")
  link=getURL(url)  
  
  print(i)
  tryCatch({
    tables=readHTMLTable(link)
    team[i]=tables[2]$`NULL`[4,1]
  },
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  
}
tictoc::toc()
team_id=cbind.data.frame(team, id) %>% filter(!is.na(team), team!="Year Formed", team!="", id %notin% c(1821, 2123))
alias=readxl::read_excel("data/DL2324.xlsx", sheet = "Alias")
team_id=team_id %>% filter(id %notin% c(2123, 1821, 1895)) %>% 
  rbind.data.frame(alias) 
save(team_id, file="data\\team_id.RDa")



# load("data/team_id.RDa")
gk=readxl::read_excel("data/DL2324.xlsx", sheet = "Goalkeepers")


teams_id_eng=gk %>% merge(team_id, by.x="Team", by.y="team", all.x=T)

# i=142
# url=paste("https://www.soccerbase.com/teams/team.sd?team_id=",i,sep="")
# link=getURL(url)  
# tables=readHTMLTable(link)
# write_lines(link, file="data/junk/arsenal.txt")


scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}

t=0
out=tribble(~link, ~id,~Team,~team_id)
for(i in teams_id$id)
{
  skip_to_next <- FALSE
  
  print(i)
  tryCatch({
    url=paste("https://www.soccerbase.com/teams/team.sd?team_id=",i,sep="")
    players=scraplinks(url = url) %>% 
      filter(grepl("player_id", url), !is.na(link)) %>% 
      mutate(id=as.numeric(substr(url, 30, nchar(url)))) %>% 
      select(-url)
    
  },
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  out=out %>% rbind.data.frame(players %>% mutate(team_id=i, Team=teams$Team[i]))

  
}

out=out %>% mutate(link=str_to_upper(link))

write.csv(out, "data/sb_id.csv", row.names = F)
# write.csv(teams_id_eng, "data/sb_teams_id.csv", row.names = F)














fw=readxl::read_excel("data/DL2324.xlsx", sheet = "Forwards") %>% 
  select(1:4)

fw2= fw %>% merge(team_id %>% rename("team_id"="id"), by.x="Club", by.y="team", all.x=T) %>% 
  mutate(Player=stringi::stri_trans_general(str = Player, id = "Latin-ASCII")) %>% 
  merge(out, by.x=c("Player"), by.y = c("link"), all.x=T) %>% 
  # filter(team_id.x==team_id.y) %>% 
  arrange(-Total)

fw3=fw2 %>%ungroup() %>%  filter(team_id.x==team_id.y)


md=readxl::read_excel("data/DL2324.xlsx", sheet = "Midfielders") %>% 
  select(1:4)

md2= md %>% merge(team_id %>% rename("team_id"="id"), by.x="Club", by.y="team", all.x=T) %>% 
  mutate(Player=stringi::stri_trans_general(str = Player, id = "Latin-ASCII")) %>% 
  merge(out, by.x=c("Player"), by.y = c("link"), all.x=T) %>% 
  # filter(team_id.x==team_id.y) %>% 
  arrange(-Total)
