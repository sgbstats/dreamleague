library(shiny)
library(tidyverse)
library(shinyWidgets)
library(lubridate)
library(readxl)
library(ggplot2)
library(htmltools)
library(flextable)
library(htmlwidgets)
library(shinydashboard)
library(dplyr)
# library(googlesheets4)
library(DT)
library(shinyjs)
# library(golem)
# 
# 
# `%notin%`=Negate(`%in%`)
# options(gargle_oauth_cache = ".secrets",
#         gargle_oauth_email = TRUE)
# googlesheets4::gs4_auth(
#   cache = ".secrets",
#   email = "sebastiangbate@gmail.com",
#   scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
# )
# # preprocessing
# dl=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0",
#                                sheet = "scores",
#                                na=c("SOLD",""),
#                                col_names = T)
# 
# daily=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0",
#                                    sheet = "daily",
#                                    na=c("SOLD",""),
#                                    col_names = T) %>%
#   mutate(position=factor(position, c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")),
#          # week=as.Date(week)
#          )
# 
# # dl_o=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0",
# #                                sheet = "scores_original",
# #                                na=c("SOLD",""),
# #                                col_names = T)
# #
# # daily_o=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0",
# #                                    sheet = "daily_original",
# #                                    na=c("SOLD",""),
# #                                    col_names = T) %>%
# #   mutate(position=factor(position, c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")),
# #         # week=as.Date(week)
# #         )
# time=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0",
#                                sheet = "update",
#                                na=c("SOLD",""),
#                                col_names = T)

load("data.RDa")

weeks=seq.Date(as.Date("2024-08-05"), by=7, length.out = 52)
weeks2=weeks[weeks<=Sys.Date()]
weekschar=format(weeks2, format="%d-%b")
# weekmatch=cbind.data.frame(weekschar, weeks2)
names(weeks2)=weekschar

load("managers.RDa")

# dl=dl_o
# managers=managers_o
# weekly=weekly_o
# dl=rbind.data.frame(dl_d %>% mutate(league="didsbury"),
#                     dl_o %>% mutate(league="original"))
managers=rbind.data.frame(managers_d %>% mutate(league="didsbury"),
                          managers_o %>% mutate(league="original"))
# daily=rbind.data.frame(daily_d %>% mutate(league="didsbury"),
#                         daily_o %>% mutate(league="original"))
weekly=daily %>% 
  mutate(week=lubridate::floor_date(Date,"weeks",week_start = 1)) %>% 
  group_by(position, player, club, cost,cost2, bought, sold, bought2,sold2, team, week) %>% 
  summarise(SBgoals=sum(SBgoals, na.rm=T),
            App=sum(App, na.rm=T)) %>% 
  arrange(team, position, -cost2, bought2, week) %>% 
  ungroup()  



league=managers %>% merge(dl %>% group_by(team) %>% 
                            summarise(total=sum(SBgoals)), by="team") %>% 
  merge(dl%>% filter(position != "GOALKEEPER") %>% group_by(team) %>% 
          summarise(gf=sum(SBgoals)), by="team") %>% 
  merge(dl %>% filter(position == "GOALKEEPER") %>% group_by(team) %>% 
          summarise(ga=-sum(SBgoals)), by="team") %>% 
  arrange(-total,-gf) %>% 
  mutate(rank=row_number(), .by="league")

teamslist=(managers %>% arrange(team))$team
names(teamslist)=paste((league %>% arrange(team))$team, " (", (league %>% arrange(team))$manager, ")", sep="")

ui <- dashboardPage(
  skin="red",
  # md = TRUE,
  dashboardHeader(title = "DreamLeague"),
  dashboardSidebar(
    sidebarMenu(
      #menuItem("Home",tabName="home",startExpanded = T, icon=icon("home"),
      menuItem("League", tabName = "league", icon = icon("table")),
      menuItem("Teams", tabName = "teams", icon = icon("shirt")),
      menuItem("cup", tabName = "cup", icon = icon("trophy")),
      menuItem("Players taken", tabName = "players", icon = icon("user-xmark")),
      menuItem("Weekly scores", tabName = "weekly", icon = icon("calendar"),
               menuSubItem("Weekly Goals", tabName = "weekly2", icon = icon("futbol")),
               menuSubItem("Weekly League", tabName = "league_weekly", icon = icon("table"))),
      menuItem("History", tabName = "history", icon=icon("clock-rotate-left"),
               menuSubItem("Team History", tabName = "team_history", icon = icon("futbol")),
               menuSubItem("League History", tabName = "league_history", icon = icon("table"))
               #menuSubItem("Horserace", tabName = "horserace", icon=icon("horse"))
      ),
      menuItem("Diagnostics", tabName = "diagnostics", icon=icon("stethoscope"))
      #menuItem("Report an issue", tabName = "issues", icon = icon("circle-exclamation"))
    )
  ),
  
  dashboardBody(
    #tags$head(tags$link(rel = "shortcut icon", href = "img/favicon.ico")),
    tabItems(
      tabItem(tabName = "league", fluid=T,
              sidebarLayout(
                sidebarPanel(
                  h3("Last Updated"),
                  textOutput("update_time"),
                  radioButtons("league", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury")
                ),
                
                mainPanel(
                  uiOutput("table",inline = TRUE, style = "margin:0px; padding:0px")
                )
              )
      ),
      tabItem(tabName = "teams", fluid=T,
              sidebarLayout(
                sidebarPanel(
                  radioButtons("league_teams", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury"),
                  pickerInput("team", "Team", choices = teamslist, selected = NULL),
                  checkboxInput("current","Current team only", value=T),
                  imageOutput("img", inline = T),
                  htmlOutput("teamtext")
                  
                ),
                mainPanel(
                  dataTableOutput("team")
                )
              )
              
      ),
      tabItem(tabName = "players", fluid=T,
              sidebarLayout(
                sidebarPanel(
                  radioButtons("league_players", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury")
                ),
                
                mainPanel(
                  
                  dataTableOutput("playerstaken")
                )
              )
      ),
      tabItem(tabName = "cup", fluid=T,
              sidebarPanel(
                radioButtons("league_cup", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury"),
                #pickerInput("team_cup", "Team", choices = teamslist, selected = NULL),
                dateInput("cup_start", "Cup round start date:", value = "2025-02-14")
              ),
              mainPanel(
                uiOutput("cup",inline = TRUE, style = "margin:0px; padding:0px")
              )),
      
      tabItem(tabName = "league_weekly", fluid=T,
              sidebarPanel(
                radioButtons("league_league_weekly", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury"),
                pickerInput("week_league", "Game week", choices = weeks2, selected = weeks2[length(weekschar)-1])),
              mainPanel(
                uiOutput("table_weekly",inline = TRUE, style = "margin:0px; padding:0px")
              )
              
      ),
      tabItem(tabName = "weekly2", fluid=T,
              sidebarLayout(
                sidebarPanel(
                  radioButtons("league_weekly2", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury"),
                  pickerInput("team2", "Team", choices = teamslist, selected = NULL),
                  pickerInput("week_player", "Game week", choices = weeks2, selected = weeks2[length(weekschar)-1])),
                mainPanel(
                  dataTableOutput("team_weekly")
                )
              )
      ) ,
      
      tabItem(tabName = "league_history", fluid=T,
              sidebarPanel(
                radioButtons("league_league_history", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury"),
                pickerInput("week_league2", "Game week", choices = weeks2, selected = weeks2[length(weekschar)-2])),
              mainPanel(
                uiOutput("table_history",inline = TRUE, style = "margin:0px; padding:0px")
              )
              
      ),
      tabItem(tabName = "team_history", fluid=T,
              sidebarLayout(
                sidebarPanel(
                  radioButtons("league_team_history", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury"),
                  pickerInput("team3", "Team", choices = teamslist, selected = NULL),
                  pickerInput("week_player3", "Game week", choices = weeks2, selected = weeks2[length(weekschar)-2])),
                mainPanel(
                  dataTableOutput("team_history_out")
                )
              )
      ) ,
      tabItem(tabName = "horserace", fluid=T,
              sidebarPanel(
                selectInput("team_choose", "Teams", choices = teamslist, selected = NULL, multiple = T),
                pickerInput("hrstart", "Start", choices = weeks2, selected = weeks2[1]),
                pickerInput("hrend", "End", choices = weeks2, selected = weeks2[length(weekschar)-1]),
                checkboxInput("relative","Relative to start date?", value=F),
              ),
              mainPanel(
                plotOutput("horserace")
              )),
      
      tabItem(tabName = "diagnostics", fluid=T,
              sidebarPanel(
                
              ),
              mainPanel(
                dataTableOutput("diagnostics")
              ))
      
    )
  )
)

server <- function(input, output, session) {
  
  league_master=reactiveVal("didsbury")
  
  observeEvent(input$league,
               {
                 #updateRadioButtons(session, "league", selected = input$league)
                 updateRadioButtons(session, "league_teams", selected = input$league)
                 updateRadioButtons(session, "league_players", selected = input$league)
                 updateRadioButtons(session, "league_league_weekly", selected = input$league)
                 updateRadioButtons(session, "league_weekly2", selected = input$league)
                 updateRadioButtons(session, "league_league_history", selected = input$league)
                 updateRadioButtons(session, "league_team_history", selected = input$league)
                 updateRadioButtons(session, "league_cup", selected = input$league)
               })
  observeEvent(input$league_teams,
               {
                 updateRadioButtons(session, "league", selected = input$league_teams)
                 #updateRadioButtons(session, "league_teams", selected = input$league_teams)
                 updateRadioButtons(session, "league_players", selected = input$league_teams)
                 updateRadioButtons(session, "league_league_weekly", selected = input$league_teams)
                 updateRadioButtons(session, "league_weekly2", selected = input$league_teams)
                 updateRadioButtons(session, "league_league_history", selected = input$league_teams)
                 updateRadioButtons(session, "league_team_history", selected = input$league_teams)
                 updateRadioButtons(session, "league_cup", selected = input$league_teams)
                 
                 teamslist=(managers %>% arrange(team) %>% 
                              filter(league==input$league_teams))$team
                 
                 names(teamslist)=paste((managers %>% arrange(team) %>% 
                                           filter(league==input$league_teams))$team, " (", 
                                        (managers %>% arrange(team) %>% 
                                           filter(league==input$league_teams))$manager, ")", sep="")
                 
                 updatePickerInput(session, "team", choices = teamslist)
               })
  observeEvent(input$league_players,
               {
                 #players_taken list
                 updateRadioButtons(session, "league", selected = input$league_players)
                 updateRadioButtons(session, "league_teams", selected = input$league_players)
                 #updateRadioButtons(session, "league_players", selected = input$league_players)
                 updateRadioButtons(session, "league_league_weekly", selected = input$league_players)
                 updateRadioButtons(session, "league_weekly2", selected = input$league_players)
                 updateRadioButtons(session, "league_league_history", selected = input$league_players)
                 updateRadioButtons(session, "league_team_history", selected = input$league_players)
                 updateRadioButtons(session, "league_cup", selected = input$league_players)
               })
  observeEvent(input$league_league_weekly,
               {
                 #weekly league
                 updateRadioButtons(session, "league", selected = input$league_league_weekly)
                 updateRadioButtons(session, "league_teams", selected = input$league_league_weekly)
                 updateRadioButtons(session, "league_players", selected = input$league_league_weekly)
                 #updateRadioButtons(session, "league_league_weekly", selected = input$league_league_weekly)
                 updateRadioButtons(session, "league_weekly2", selected = input$league_league_weekly)
                 updateRadioButtons(session, "league_league_history", selected = input$league_league_weekly)
                 updateRadioButtons(session, "league_team_history", selected = input$league_league_weekly)
                 updateRadioButtons(session, "league_cup", selected = input$league_league_weekly)
               })
  observeEvent(input$league_weekly2,
               {
                 updateRadioButtons(session, "league", selected = input$league_weekly2)
                 updateRadioButtons(session, "league_teams", selected = input$league_weekly2)
                 updateRadioButtons(session, "league_players", selected = input$league_weekly2)
                 updateRadioButtons(session, "league_league_weekly", selected = input$league_weekly2)
                 #updateRadioButtons(session, "league_weekly2", selected = input$league_weekly2)
                 updateRadioButtons(session, "league_league_history", selected = input$league_weekly2)
                 updateRadioButtons(session, "league_team_history", selected = input$league_weekly2)
                 updateRadioButtons(session, "league_cup", selected = input$league_weekly2)
                 
                 teamslist=(managers %>% arrange(team) %>% 
                              filter(league==input$league_weekly2))$team
                 
                 names(teamslist)=paste((managers %>% arrange(team) %>% 
                                           filter(league==input$league_weekly2))$team, " (", 
                                        (managers %>% arrange(team) %>% 
                                           filter(league==input$league_weekly2))$manager, ")", sep="")
                 
                 updatePickerInput(session, "team2", choices = teamslist)
               })
  observeEvent(input$league_league_history,
               {
                 updateRadioButtons(session, "league", selected = input$league_league_history)
                 updateRadioButtons(session, "league_teams", selected = input$league_league_history)
                 updateRadioButtons(session, "league_players", selected = input$league_league_history)
                 updateRadioButtons(session, "league_league_weekly", selected = input$league_league_history)
                 updateRadioButtons(session, "league_weekly2", selected = input$league_league_history)
                 #updateRadioButtons(session, "league_league_history", selected = input$league_league_history)
                 updateRadioButtons(session, "league_team_history", selected = input$league_league_history)
                 updateRadioButtons(session, "league_cup", selected = input$league_league_history)
               })
  observeEvent(input$league_team_history,
               {
                 updateRadioButtons(session, "league", selected = input$league_team_history)
                 updateRadioButtons(session, "league_teams", selected = input$league_team_history)
                 updateRadioButtons(session, "league_players", selected = input$league_team_history)
                 updateRadioButtons(session, "league_league_weekly", selected = input$league_team_history)
                 updateRadioButtons(session, "league_weekly2", selected = input$league_team_history)
                 updateRadioButtons(session, "league_league_history", selected = input$league_team_history)
                 #updateRadioButtons(session, "league_team_history", selected = input$league_team_history)
                 updateRadioButtons(session, "league_cup", selected = input$league_team_history)
                 
                 teamslist=(managers %>% arrange(team) %>% 
                              filter(league==input$league_team_history))$team
                 
                 names(teamslist)=paste((managers %>% arrange(team) %>% 
                                           filter(league==input$league_team_history))$team, " (", 
                                        (managers %>% arrange(team) %>% 
                                           filter(league==input$league_team_history))$manager, ")", sep="")
                 
                 updatePickerInput(session, "team3", choices = teamslist)
               })
  observeEvent(input$league_cup,
               {
                 updateRadioButtons(session, "league", selected = input$league_cup)
                 updateRadioButtons(session, "league_teams", selected = input$league_cup)
                 updateRadioButtons(session, "league_players", selected = input$league_cup)
                 updateRadioButtons(session, "league_league_weekly", selected = input$league_cup)
                 updateRadioButtons(session, "league_weekly2", selected = input$league_cup)
                 updateRadioButtons(session, "league_league_history", selected = input$league_cup)
                 updateRadioButtons(session, "league_team_history", selected = input$league_cup)
                 # updateRadioButtons(session, "league_cup", selected = input$league_team_history)
                 
                 teamslist=(managers %>% arrange(team) %>% 
                              filter(league==input$league_cup))$team
                 
                 names(teamslist)=paste((managers %>% arrange(team) %>% 
                                           filter(league==input$league_cup))$team, " (", 
                                        (managers %>% arrange(team) %>% 
                                           filter(league==input$league_cup))$manager, ")", sep="")
                 
                 updatePickerInput(session, "team_cup", choices = teamslist)
               })
  
  output$table=renderUI({
    league%>% 
      filter(league==input$league) %>%
      select(-league, -rank) %>% 
      flextable() %>% 
      set_header_labels(team="Team",
                        manager="Manager",
                        total="Total",
                        gf="For",
                        ga="Against") %>% 
      bg(i=1:2, bg=c("#FFD700", "#C0C0C0")) %>% 
      autofit() %>%
      font(fontname = "Arial", part="all") %>% 
      htmltools_value()
  })
  
  output$table_weekly=renderUI({
    
    league2=managers %>% 
      merge(weekly %>%mutate(week2=as.Date(week, format="%d-%b")) %>% 
              filter(week ==input$week_league) %>%
              group_by(team) %>% 
              summarise(total=sum(SBgoals)), by="team", all = T) %>% 
      merge(weekly %>%mutate(week2=as.Date(week, format="%d-%b")) %>% 
              filter(week ==input$week_league)%>% 
              filter(position != "GOALKEEPER") %>% group_by(team) %>% 
              summarise(gf=sum(SBgoals)), by="team", all=T) %>% 
      merge(weekly %>%mutate(week2=as.Date(week, format="%d-%b")) %>% 
              filter(week ==input$week_league) %>% 
              filter(position == "GOALKEEPER") %>% group_by(team) %>% 
              summarise(ga=-sum(SBgoals)), by="team", all=T) %>%
      replace(is.na(.), 0) %>% 
      filter(league==input$league_league_weekly) %>%
      select(-league) %>% 
      arrange(-total,-gf)
    
    league2%>% flextable() %>% 
      set_header_labels(team="Team",
                        manager="Manager",
                        total="Total",
                        gf="For",
                        ga="Against") %>% 
      # bg(i=1:2, bg=c("#FFD700", "#C0C0C0")) %>% 
      autofit() %>%
      font(fontname = "Arial", part="all") %>% 
      htmltools_value()
  })
  
  output$table_history=renderUI({
    
    league3=managers %>% 
      merge(weekly %>%mutate(week2=as.Date(week, format="%d-%b")) %>% 
              filter(week <=input$week_league2) %>%
              group_by(team) %>% 
              summarise(total=sum(SBgoals)), by="team", all = T) %>% 
      merge(weekly %>%mutate(week2=as.Date(week, format="%d-%b")) %>% 
              filter(week <=input$week_league2) %>% 
              filter(position != "GOALKEEPER") %>% group_by(team) %>% 
              summarise(gf=sum(SBgoals)), by="team", all=T) %>% 
      merge(weekly %>%mutate(week2=as.Date(week, format="%d-%b")) %>% 
              filter(week <=input$week_league2)  %>% 
              filter(position == "GOALKEEPER") %>% group_by(team) %>% 
              summarise(ga=-sum(SBgoals)), by="team", all=T) %>%
      replace(is.na(.), 0) %>% 
      filter(league==input$league_league_history) %>%
      select(-league) %>% 
      arrange(-total,-gf)
    
    league3%>% flextable() %>% 
      set_header_labels(team="Team",
                        manager="Manager",
                        total="Total",
                        gf="For",
                        ga="Against") %>% 
      # bg(i=1:2, bg=c("#FFD700", "#C0C0C0")) %>% 
      autofit() %>%
      font(fontname = "Arial", part="all") %>% 
      htmltools_value()
  })
  
  output$team=DT::renderDT({
    
    if(input$current)
    {
      teams3= dl%>% filter(team==input$team)  %>% filter(is.na(sold)) %>% select(-sold, -bought2, -sold2, -SBapp, -league)
    }else
    {
      teams3= dl%>% filter(team==input$team)  %>% select( -bought2, -sold2, -SBapp, -league)
    }
    
    teams3 %>% select(-team) %>% 
      select(-goals) %>% 
      rename("Goals"="SBgoals")%>% 
      rename_with(str_to_title) %>% 
      relocate(Goals, .after=Club)
    
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '100px', targets = c(1,2)),
                      list(width = '50px', targets = c(0,3)),
                      list(width = '30px', targets = c(4,5))),
    scrollX=T,
    pageLength=15)
  )
  
  output$team_weekly=DT::renderDT({
    
    
    weekly%>% filter(team==input$team2) %>% 
      mutate(week2=format(week, format="%d-%b")) %>%
      filter(is.na(sold))%>% 
      filter(week ==input$week_player) %>%
      filter(SBgoals!=0) %>%
      select(-sold, -bought2, -sold2, -App, -week, -week2)%>%
      select(-team, -cost2) %>%
      # select(-goals) %>%
      rename("Goals"="SBgoals")%>%
      rename_with(str_to_title) %>%
      relocate(Goals, .after=Club)
    # 
    
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '100px', targets = c(1,2)),
                      list(width = '50px', targets = c(0,3)),
                      list(width = '30px', targets = c(4,5))),
    scrollX=T,
    pageLength=15)
  )
  
  
  output$team_history_out=DT::renderDT({
    
    
    teams5= weekly%>% filter(team==input$team3)  %>%
      mutate(week2=as.Date(week, format="%d-%b")) %>%
      filter(bought2<=input$week_player3) %>%
      filter(week <=input$week_player3) %>%
      # filter(SBgoals!=0) %>%
      group_by(position, player,club,cost,bought,sold, cost2) %>%
      summarise(SBgoals=sum(SBgoals, na.rm = T)) %>%
      #select(-sold, -bought2, -sold2, -App, -week, -week2) %>%
      ungroup() %>%
      arrange(position, -cost2) %>%
      select(-cost2) %>%
      rename("Goals"="SBgoals")%>%
      rename_with(str_to_title) %>%
      relocate(Goals, .after=Club)
    
    teams5 %>% data.table::as.data.table()
    
  }#,
  # options = list(
  #   autoWidth = TRUE,
  #   columnDefs = list(list(width = '100px', targets = c(1,2)),
  #                     list(width = '50px', targets = c(0,3)),
  #                     list(width = '30px', targets = c(4,5))),
  #   scrollX=T)
  )
  
  output$teamtext=renderUI({
    text1=paste("<b>League position:",league$rank[which(league$team==input$team)] , "</b>")
    text2=paste("<b>Score:", league$total[which(league$team==input$team)], "</b>")
    text3=paste("<font color=\"#4DAF4A\">For:", league$gf[which(league$team==input$team)], "</font>")
    text4=paste("<font color=\"#E41A1C\">Against:", league$ga[which(league$team==input$team)], "</font>")
    outfield=paste("Outfield transfers remaining:", 8-dl %>% filter(team==input$team, position!= "GOALKEEPER", cost=="TRANSFER") %>% nrow())
    goalie=paste("Goalkeeper transfers remaining:", 2-dl %>% filter(team==input$team, position== "GOALKEEPER", cost=="TRANSFER") %>% nrow())
    HTML(paste(text1, text2, text3, text4, outfield,goalie,sep="<br/>"))
  })
  
  output$img=renderImage({
    outfile=paste("img/", str_to_upper(str_replace_all(input$team, "[^[:alnum:]]", "")), ".png", sep="")
    
    list(src = outfile,
         contentType = 'image/png',
         width = 100,
         height = 100)
    
  }, deleteFile = F)
  
  output$playerstaken=DT::renderDT({
    dl %>% filter(is.na(sold),
                  league==input$league) %>% 
      dplyr::select(team, player,club, position) %>% 
      rename_with(str_to_title)
  },
  options = list(
    autoWidth = TRUE,
    # columnDefs = list(list(width = '100px', targets = c(1,2)),
    #                   list(width = '50px', targets = c(0,3)),
    #                   list(width = '30px', targets = c(4,5))),
    scrollX=T,
    pageLength=15))
  
  output$diagnostics=DT::renderDT({
    dl %>% filter(is.na(sold)) %>% 
      dplyr::select(team, player,club, position) %>% 
      mutate(position=factor(position, c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"), ordered = T)) %>% 
      count(team, position) %>% 
      pivot_wider(names_from = "position", values_from = "n") %>% 
      filter(GOALKEEPER!=1|DEFENDER!=2|MIDFIELDER!=3|FORWARD!=5)
  })
  
  output$horserace=renderPlot({
    if(input$relative)
    {
      weekly0=weekly %>% 
        filter(week>=input$hrstart)
    }else
    {
      weekly0=weekly
    }
    
    weekly0 %>% group_by(team, week) %>% 
      filter(team %in% input$team_choose) %>% 
      summarise(score=sum(SBgoals, na.rm=T)) %>% 
      ungroup() %>% 
      arrange(team, week) %>% 
      group_by(team) %>% 
      mutate(score=cumsum(score)) %>% 
      filter(week>=input$hrstart,
             week<=input$hrend) %>% 
      ggplot(aes(x=week, y=score, col=team, #xmin=as.POSIXct(input$hrstart, format="%Y-%m-%d"), xmax=as.POSIXct(input$hrend, format="%Y-%m-%d")
      ))+
      geom_line(linewidth=1)+
      labs(x="Date", y=if_else(input$relative, "Relative Score","Total Score"), col="Team")+
      theme_bw()#+
    #scale_x_date(limits = as.Date(c(input$hrstart, input$hrend), format="%d-%b"))
    
  })
  output$update_time=renderText({ 
    
    format(time$update_time, format="%Y-%m-%d %H:%M:%S")
  })
  
  output$cup=renderUI({
    managers %>% merge(daily %>%filter(Date>=input$cup_start,
                                Date<=input$cup_start+lubridate::days(3)) %>% 
                         summarise(total=sum(SBgoals),.by="team"), by="team", all.x = T) %>% 
      merge(daily%>% filter(position != "GOALKEEPER") %>%
              filter(Date>=input$cup_start,
                     Date<=input$cup_start+lubridate::days(3)) %>% 
              summarise(gf=sum(SBgoals),.by="team"), by="team", all.x = T) %>% 
      merge(daily %>% filter(position == "GOALKEEPER") %>%
              filter(Date>=input$cup_start,
                     Date<=input$cup_start+lubridate::days(3)) %>% 
              summarise(ga=-sum(SBgoals),.by="team"), by="team", all.x = T) %>% 
      mutate(ga = replace(ga, is.na(ga), 0),
             gf = replace(gf, is.na(gf), 0)) %>% 
      arrange(-total,-gf) %>% 
      mutate(rank=row_number(), .by="league") %>% 
      filter(league==input$league) %>%
      select(-league, -rank) %>% 
      flextable() %>% 
      set_header_labels(team="Team",
                        manager="Manager",
                        total="Total",
                        gf="For",
                        ga="Against") %>% 
     # bg(i=1:2, bg=c("#FFD700", "#C0C0C0")) %>% 
      autofit() %>%
      font(fontname = "Arial", part="all") %>% 
      htmltools_value()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)