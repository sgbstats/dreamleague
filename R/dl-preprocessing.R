library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
library(googlesheets4)
library(fuzzyjoin)
library(crayon)
`%notin%`=Negate(`%in%`)
library(rvest)
# setwd("C:/R/git/dreamleague")


dl_process=function(dl, managers, league)
{
  tictoc::tic()
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
  
  if(league=="Didsbury")
  {
    teams=dl  %>% 
      rename(position=1,
             player=2,
             club=3,
             cost=4,
             goals=5,
             sold=7,
             bought=6) %>% 
      mutate(team=NA_character_)
  }else if(league=="Original")
  {
    teams=dl  %>% 
      rename(position=1,
             player=2,
             club=3,
             cost=4,
             goals=5,
             sold=6,
             bought=7) %>% 
      mutate(team=NA_character_)
  }
  
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
  
  teams3a=teams2 %>% filter(position %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")) %>% 
    mutate(goals=if_else(position=="GOALKEEPER", -abs(as.numeric(goals)), as.numeric(goals)),
           club=if_else(club=="OXFORD UTD", "OXFORD", club),
           player=case_when(player=="DANIEL ORSI"~ "DANILO ORSI-DADAMO", 
                            player=="BEN BRERETON DIAZ"~"BEN BRERETON",
                            player=="HEUNG MIN SON"~"SON HEUNG-MIN",
                            player=="HEUNG-MIN SON"~"SON HEUNG-MIN",
                            player=="DAN JAMES"~"DANIEL JAMES",
                            player=="HWANG HEE CHAN"~"HEE-CHAN HWANG HEE-CHAN",
                            player=="DAN AGYEI"~"DANIEL AGYEI",
                            T~player)) 
  
  if(league=="Didsbury")
  {
    teams3=teams3a %>% mutate(bought=format(as.Date(bought), "%d-%b"),
                              sold=format(as.Date(sold), "%d-%b"))
  }else if(league=="Original")
  {
    for(i in 1:(nrow(teams3a)-1))
    {
      teams3a$sold[i]=teams3a$bought[i+1]
    }
    teams3=teams3a %>% 
      mutate(bought=format(openxlsx::convertToDate(bought)-1, "%d-%b"),
             sold=format(openxlsx::convertToDate(sold)-1, "%d-%b"))
  }
  
  
  # merge(team_id %>% mutate(team=str_to_upper(team)), by.x = "club", by.y="team", all.x = T)
  player_id2=player_id %>% select(player, player_id,team) %>% 
    filter(player_id %notin% c(70735, 182525))
  
  outfield0=teams3 %>% filter(position %in% c( "DEFENDER", "MIDFIELDER", "FORWARD")) %>%
    mutate(player=case_when(player=="STRAND LARSEN"~"JORGEN STRAND LARSEN",
                            T~player)) %>% 
    fuzzyjoin::stringdist_join(player_id2,
                               by="player", mode="left", method="jw", distance_col="dist") %>% 
    group_by(player.x) %>%
    slice_min(order_by=dist, n=1) %>% 
    ungroup() %>% 
    rename(team=team.x) %>%
    select(-team.y) %>% 
    mutate(bought2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", bought)~ paste(bought,"-2024", sep=""),
                                     grepl("Jan|Feb|Mar|Apr|May|Jun", bought)~paste(bought,"-2025", sep=""),
                                     T~"01-Jul-2024"), "%d-%b-%Y"),
           sold2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", sold)~ paste(sold,"-2024", sep=""),
                                   grepl("Jan|Feb|Mar|Apr|May|Jun", sold)~paste(sold,"-2025", sep=""),
                                   T~"30-Jun-2025"), "%d-%b-%Y"))
  
  
  
  mismatch=outfield0 %>% filter(dist!=0|is.na(dist)) %>% 
    select(team, player.x, club, player.y, dist, player_id)
  
  duplicates=outfield0 %>% count(player.y) %>% filter(n>1)
  
  outfield=outfield0 %>% 
    group_by(player_id, team, bought2) %>% 
    slice_min(team, n=1, with_ties = F) %>%
    ungroup() %>% 
    mutate(SBgoals=0,
           SBapp=0) %>% 
    select(-player.y, -dist) %>% 
    rename("player"="player.x") %>% 
    arrange(player) %>% 
    drop_na(player)
  
  weekly=tribble(~"player_id", ~"Date", ~"Goals", ~"App", ~"team")
  
  for(i in 1:nrow(outfield))
    # for(i in 1:10)
  {
    skip_to_next <- FALSE
    # if(is.na(outfield$id[i])){next}
    
    url=paste("https://www.soccerbase.com/players/player.sd?player_id=",outfield$player_id[i],"&season_id=157",sep="")
    link=RCurl::getURL(url)  
    
    cat(paste0(outfield$player[i],"\n"))
    tryCatch({
      tables=readHTMLTable(link)
      appgoals=(tables$tpg) %>% filter(V1 %in% comps) %>% 
        mutate(Date=as.Date(substr(V2,4,13), "%d%b %Y")) %>% 
        filter(Date>outfield$bought2[i],
               Date<=outfield$sold2[i]) %>% 
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
      
      cat(blue(paste0(outfield$SBgoals[i], "\n")))
    },
    error = function(e) 
    { 
      skip_to_next <<- TRUE
      cat(red("Error\n"))
    }
    
    )
    
    if(skip_to_next) { next } 
    
  }
  # 
  test=outfield %>% filter(goals!=SBgoals, !is.na(id)) %>% 
    select(position, player, goals, SBgoals, player_id) %>% 
    rename("BCgoals"="goals")
  # load("Data/team_id.RDa")
  
  gk=teams3 %>% filter(position %notin% c( "DEFENDER", "MIDFIELDER", "FORWARD")) %>% 
    merge(team_id %>% select(team, team_id) %>% 
            rbind.data.frame(data.frame(team=c("WEST BROM"), team_id=c(2744))) %>% 
            mutate(team=if_else(team=="WEST BROMWICH ALBION", "WEST BROMWICH", team)) %>% 
            group_by(team) %>% 
            slice_min(team_id, with_ties = F) %>%
            ungroup()  %>% 
            mutate(team=str_to_upper(team)), by.x="club", by.y="team", all.x=T) %>% 
    mutate(SBgoals=0, SBapp=0) %>% 
    ungroup() %>% 
    drop_na(club) %>% 
      mutate(bought2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", bought)~ paste(bought,"-2024", sep=""),
                                       grepl("Jan|Feb|Mar|Apr|May|Jun", bought)~paste(bought,"-2025", sep=""),
                                       T~"01-Jul-2024"), "%d-%b-%Y"),
             sold2=as.Date(case_when(grepl("Jul|Aug|Sep|Oct|Nov|Dec", sold)~ paste(sold,"-2024", sep=""),
                                     grepl("Jan|Feb|Mar|Apr|May|Jun", sold)~paste(sold,"-2025", sep=""),
                                     T~"30-Jun-2025"), "%d-%b-%Y"))
  
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
  
  for(i in 1:nrow(gk))
    # for(i in 1:10)
  {
    skip_to_next <- FALSE
    # if(is.na(outfield$id[i])){next}
    
    url=paste("https://www.soccerbase.com/teams/team.sd?team_id=",gk$team_id[i],"&teamTabs=results",sep="")
    link=RCurl::getURL(url)  
    
    cat(paste0(gk$club[i], "\n"))
    tryCatch({
      
      
      sl=scraplinks2(url)
      lg=paste(comps, collapse = "|")
      x=sl%>% group_by(row_number()) %>% 
        mutate(comp=str_extract(link, lg),
               date=as.Date(str_extract(link, "\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])"),"%Y-%m-%d"),
               score=str_extract(link, "\\d{1}[[:space:]]-[[:space:]]\\d{1}"),
               status=str_extract(link, "(W|L|D|D\\*)[[:space:]]"),
               App=1) %>% 
        # mutate(link=gsub("Bristol C", "Bristolc ", link),
        #        link=gsub("Bristol R", "Bristolr ", link),
        #        link=gsub("Cambridge U", "Cambridgeu", link),
        #        link=gsub("Paris St-G.", "Psg ", link),
        #        link=gsub("QPR", "Qpr ", link),
        #        link=gsub("Sheff Wed", "Sheffw ", link)) %>%
        # mutate(teampos=unlist(gregexpr("[a-z][[:space:]]1[[:space:]]\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])", link))[1],
        #        opppos=unlist(gregexpr("[a-z][[:space:]]2[[:space:]]\\d{4}-([0]\\d|1[0-2])-([0-2]\\d|3[01])", link))[1]) %>% 
        # ungroup() %>% 
        mutate(score=gsub(" ", "", score)) %>% 
        separate(score, into = c("H", "A"), sep="-") %>% 
        ungroup() %>% 
        mutate(H=as.numeric(str_trim(H)),
               A=as.numeric(str_trim(A)),
               rn=row_number()) %>% 
        mutate(concede=case_when(status=="W "~ -min(H,A),
                                 status=="D "~ -H,
                                 status=="D* "~ -H,
                                 status=="L "~ -max(H,A)),.by="rn")%>%
        filter(comp %in% comps ) %>% 
        filter(date>gk$bought2[i],
               date<=gk$sold2[i],
               !is.na(H)
        )
      
      
      # if(gk$club[i]=="NEWCASTLE")
      # {
      #   x$concede[10]=-1
      # }  
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
    error = function(e) 
    { 
      skip_to_next <<- TRUE
      cat(red("Error\n"))
    }
    )
    
    if(skip_to_next) { next } 
    
  }
  gk$SBgoals=as.numeric(gk$SBgoals)
  
  testgk=gk %>% filter(goals!=SBgoals, !is.na(id)) %>% 
    select(position, club, goals, SBgoals, team_id) %>% 
    rename("BCgoals"="goals")
  
  team_score=rbind(outfield%>% ungroup() %>% dplyr::select(-player_id) , gk %>% dplyr::select(-team_id) %>% 
                     mutate(SBgoals=as.numeric(SBgoals),
                            SBapp=as.numeric(SBapp))) %>% 
    ungroup() %>% 
    mutate(position=factor(position, levels=c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"), ordered = T)) %>% 
    mutate(cost2=as.numeric(cost)) %>% 
    arrange(team, position, -cost2, bought2) %>% 
    select(-cost2)
  
  # gs4_deauth()
  
  
  weekly2=weekly %>% merge(outfield %>% select(-SBgoals, -SBapp), by=c("player_id", "team"))
  weekly_gk2=weekly_gk %>% merge(gk %>% select(-SBgoals, -SBapp), by="team_id")
  
  #seq.Date(as.Date("2023-07-26"), by=7, length.out = 52)
  
  team_score_weekly=rbind.data.frame(weekly2 %>% select(-player_id), weekly_gk2 %>% select(-team_id)) %>% 
    ungroup() %>% 
    mutate(position=factor(position, levels=c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"), ordered = T)) %>% 
    mutate(cost2=as.numeric(cost)) %>% 
    mutate(week=lubridate::floor_date(Date,"weeks",week_start = 1)) %>% 
    group_by(position, player, club, cost,cost2, bought, sold, bought2,sold2, team, week) %>% 
    summarise(SBgoals=sum(Goals, na.rm=T),
              App=sum(App, na.rm=T)) %>% 
    arrange(team, position, -cost2, bought2, week) %>% 
    ungroup()  
  
  
  tictoc::toc()
  return(list("scores"=team_score,
              "weekly"=team_score_weekly,
              "mismatch"=mismatch,
              "goals_for_mistatch"=test,
              "goals_ag_mistatch"=testgk))
}