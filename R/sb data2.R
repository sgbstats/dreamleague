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

team_id=scraplinks("https://www.soccerbase.com/teams/home.sd") %>% 
  filter(grepl("comp_id=[1-4]$", url), grepl("team_id", url)) %>% 
  mutate(team_id=as.numeric(stringi::stri_extract_first_regex(url, "[0-9]+"))) %>% 
  select(-url)%>% 
  rename("team"="link") %>% 
  mutate(team=case_when(team=="AFC W'bledon"~"Wimbledon",
                        team=="Man City"~"Manchester City",
                        team=="Man Utd"~"Manchester United",
                        team=="Bristol C"~"Bristol City",
                        team=="Bristol R"~"Bristol Rovers",
                        team=="MK Dons"~"Milton Keynes Dons",
                        team=="West Brom"~"West Bromwich Albion",
                        team=="Sheff Utd"~"Sheffield United",
                        team=="Sheff Wed"~"Sheffield Wednesday",
                        team=="Cambridge U"~"Cambridge",
                        team=="Nottm Forest"~"Nottingham Forest",
                        team=="Notts Co"~"Notts County",
                        team=="Newport Co"~"Newport County",
                        team=="Norwich"~"Norwich City",
                        
                   T~team))
 
t=0
player_id0=tribble(~player, ~player_id,~team,~team_id)
for(i in 1:nrow(team_id))
{
  skip_to_next <- FALSE
  
  cat(paste0(team_id$team[i], "\n"))
  tryCatch({
    url=paste("https://www.soccerbase.com/teams/team.sd?team_id=",team_id$team_id[i],sep="")
    players0=scraplinks(url = url) %>% 
      mutate(n=row_number()) 
    
    #find where the junk starts
    rn=(players0 %>% filter(grepl( "tourn_id", url, ignore.case = T)) %>% slice_min(n))$n
    players=players0%>% 
      filter(n<rn,grepl("player_id", url), !is.na(link)) %>% 
      mutate(player_id=as.numeric(stringi::stri_extract_first_regex(url, "[0-9]+"))) %>% 
      select(-url) %>% 
      rename("player"="link") 
      
    
  },
  error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  player_id0=player_id0 %>% rbind.data.frame(players %>% mutate( team=team_id$team[i],team_id=team_id$team_id[i]))
  
  
}

player_id=player_id0 %>% mutate(player=case_when(player=="Ali Ibrahim Ali Al Hamadi"~"Ali Al Hamadi",
                                                player=="Dan Agyei"~"Daniel Agyei",
                                                player=="Joshua Sargent"~"Josh Sargent",
                                                player=="Mo Eisa"~"Mohamed Eisa",
                                                player=="Iyenoma Destiny Udogie"~"Destiny Udogie",
                                                T~player))%>% 
  mutate(player=str_to_upper(player)) %>% 
  mutate(team=str_to_upper(team)) %>% 
  # rbind(tribble(~"player", ~"n", ~"player_id", ~"team", ~"team_id",
  #               "HARRY KANE", NA_integer_, 52657, "BAYERN MUNICH", 469,
  #               "CHUBA AKPOM", NA_integer_, 68532, "AJAX", 80,
  #               "NATHAN TELLA", NA_integer_, 107792, "BAYER LEVERKUSEN", 468,
  #               "OSCAR ESTUPINAN", NA_integer_, 104942, "METZ", 1772)) %>%
  group_by(player_id) %>% 
  slice_min(team, with_ties = F) %>% 
  filter(player_id!=107014) %>% 
  rbind.data.frame(tribble(~"player", ~"n", ~"player_id", ~"team", ~"team_id",
                           "TOMMY CONWAY", NA_integer_, 134873, "BRISTOL CITY", 376,
                           "GABRIEL SARA", NA_integer_, 146267, "GALATASTARY", 1125,
                           "VICTOR OSHIMEN", NA_integer_, 100403, "NAPOLI", 1801))

team_id=team_id %>% mutate(team=str_to_upper(team))



mostrecent=max(list.files(path = "data/legacy/", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))

load(paste("data/legacy/", mostrecent, sep=""))

player_id=player_id %>% rbind.data.frame(legacy) %>% group_by(player_id) %>% slice_min(team, with_ties = F)%>% 
  filter(player_id!=107014)

legacy=player_id
save(legacy, file = paste("data/legacy/ids", Sys.Date(),".RDa",sep=""))
save(team_id, player_id, file = "data/ids.RDa")
