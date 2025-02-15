library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
library(googlesheets4)
library(fuzzyjoin)
`%notin%`=Negate(`%in%`)
# setwd("C:/R/git/dreamleague")
tictoc::tic()

load("data/ids.RDa")

player_id2=player_id %>% 
  mutate(position="",
         SBgoals=0,
         SBapp=0)
weeklyreport=tribble(~"player_id", ~"Date", ~"Goals", ~"App", ~"team")
comps=c("English premier", "English Premier",
        "EFL Cup", "English League Cup",
        "Europa League", 
        "Community Shield",
        "Champions League",
        "FA Cup", "English FA Cup",
        "Europa Conference League",
        "Football League Championship", "Football League Championship Play-Off",
        "Football League One", "Football League One Play-Off",
        "Football League Two", "Football League Two Play-Off" )
for(i in 1:nrow(player_id2))
  # for(i in 1:100)
{
  skip_to_next <- FALSE
  # if(is.na(player_id2$id[i])){next}
  
  url=paste("https://www.soccerbase.com/players/player.sd?player_id=",player_id2$player_id[i],"&season_id=157",sep="")
  link=RCurl::getURL(url)  
  cat(paste(i, player_id$player[i], "\n"))
  tryCatch({
    tables=readHTMLTable(link)
    player_id2$position[i]=stringr::word(tables[[1]],1)
    # if(player_id2$player_id[i]==75804)
    # {
    #   tables$tpg$V7[5]="1"
    # }
    # 
    # if(player_id2$player_id[i]==52657)
    # {
    #   tables$tpg$V7=""
    # }
    appgoals=(tables$tpg) %>% filter(V1 %in% comps) %>% 
      mutate(Date=as.Date(substr(V2,4,13), "%d%b %Y")) %>% 
      mutate(Goals=as.numeric(V7),
             App=1,
             Goals=if_else(is.na(Goals), 0, Goals),
             player_id=player_id2$player_id[i],
             team=player_id2$team[i]) 
    
    
    appgoals2=appgoals%>% 
      summarise(App=sum(App, na.rm=T),
                Goals=sum(Goals, na.rm=T))
    
    
    player_id2$SBgoals[i]=appgoals2[1,2]
    
    player_id2$SBapp[i]=appgoals2[1,1]
    
    weeklyreport=rbind(weeklyreport, appgoals %>% select(player_id, Date, Goals, App, team))
    
    cat(paste(player_id2$SBgoals[i], "\n"))
  },
  error = function(e) { skip_to_next <<- TRUE
  warning("Error")
  } 
  
  )
  
  if(skip_to_next) { next } 
  
  }


dl=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0",
                             sheet = "scores",
                             na=c("SOLD",""),
                             col_names = T) %>% 
  filter(!is.na(player), is.na(sold))

weeklyleaguedata=weeklyreport %>% merge(player_id %>% select(player, player_id), by="player_id") %>% 
  select(player, player_id, team, Date, Goals, App) %>% 
  mutate(taken=player %in% dl$player)

player_id_scout=player_id2%>% 
  mutate(taken=player %in% dl$player)

tictoc::toc()

save(player_id_scout,  file = "data/scouting/scoutingdata.RDa")
