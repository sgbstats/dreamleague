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
library(DT)
library(shinyjs)
library(reactable)

options(gargle_oauth_cache = ".secrets",
        gargle_oauth_email = TRUE)
googledrive::drive_auth(
  cache = ".secrets",
  email = "sebastiangbate@gmail.com"
)
googledrive::drive_download(googledrive::as_id("108pNlDYjniFZiPU3PG82bIdChZmZGqUh"),
                            path="data.RDa",
                            overwrite = T)
load("data.RDa")
weeks=seq.Date(as.Date("2025-07-28"), by=7, length.out = 52)
weeks2=weeks[weeks<=Sys.Date()]
weekschar=format(weeks2, format="%d-%b")
names(weeks2)=weekschar

load("managers.RDa")

managers=rbind.data.frame(managers_d %>% mutate(league="didsbury"),
                          managers_o %>% mutate(league="original"))
league=managers %>% merge(dl %>% group_by(team) %>% 
                            summarise(total=sum(SBgoals)), by="team", all=T) %>% 
  merge(dl%>% filter(position != "GOALKEEPER") %>% group_by(team) %>% 
          summarise(gf=sum(SBgoals)), by="team", all=T) %>% 
  merge(dl %>% filter(position == "GOALKEEPER") %>% group_by(team) %>% 
          summarise(ga=-sum(SBgoals)), by="team", all=T) %>% 
  arrange(-total,-gf) %>% 
  mutate(rank=row_number(), .by="league")

teamslist=(managers %>% arrange(team))$team
names(teamslist)=paste((league %>% arrange(team))$team, " (", (league %>% arrange(team))$manager, ")", sep="")
teamslist_cup=(managers %>% arrange(team))$team

names(teamslist_cup)=paste((managers %>% arrange(team))$team, " (", 
                           (managers %>% arrange(team))$manager, ")", sep="")

rounds=unique(cupties$round)
ui <- dashboardPage(
  skin="red",
  # md = TRUE,
  dashboardHeader(title = "DreamLeague"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("League", tabName = "league", icon = icon("table")),
      menuItem("Teams", tabName = "teams", icon = icon("shirt")),
      menuItem("BFL Cup", tabName = "cup", icon = icon("trophy")),
      menuItem("Players taken", tabName = "players", icon = icon("user-xmark")),
      menuItem("History", tabName = "history", icon=icon("clock-rotate-left")),
      menuItem("Diagnostics", tabName = "diagnostics", icon=icon("stethoscope")),
      menuItem("Report an issue", tabName = "bug", icon=icon("bug"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "league", fluid=T,
              sidebarLayout(
                sidebarPanel(
                  h3("Last Updated"),
                  uiOutput("update_time"),
                  radioButtons("league", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury")
                ),
                mainPanel(
                  tags$div(
                    class = "alert alert-warning",
                    HTML(
                      paste0(
                        "Some goals may be missing due to changes in soccerbase. Please ",
                        "<a href=\"#shiny-tab-bug\" data-toggle=\"tab\">report an issue</a>",
                        " so it can be fixed. ",
                        "<button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>"
                      )
                    )
                  ),
                  uiOutput("table",inline = TRUE, style = "margin:0px; padding:0px")
                )
              )
      ),
      tabItem(
        tabName = "teams",fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            radioButtons(
              "league_teams",
              "League",
              choices = c("Didsbury" = "didsbury", "Original" = "original"),
              selected = "didsbury"
            ),
            pickerInput("team", "Team", choices = teamslist, selected = NULL),
            checkboxInput("current", "Current team only", value = T),
            imageOutput("img", inline = T),
            htmlOutput("teamtext")
          ),
          mainPanel(dataTableOutput("team"))
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
                radioButtons("comp_cup", "Competition ", choices = c("BFL Challenge Cup"="bfl", "Didsbury Cup"="didsbury","Original Cup"="original"), selected = "bfl"),
                pickerInput("round_cup", "Round", choices = rounds, selected="R1 Replay", multiple = F)
              ),
              mainPanel(
                tags$div(
                  class = "alert alert-secondary",
                  HTML(paste0(
                    "Rows expand to show scorers",
                    "<button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>"
                  ))
                ),
                reactableOutput("cup")
              )
      ),
      tabItem(tabName = "history", fluid=T,
              sidebarLayout(
                sidebarPanel(
                  radioButtons("league_team_history", "League", choices = c("Didsbury"="didsbury","Original"="original"), selected = "didsbury"),
                  dateInput("start", "Start date", value = floor_date(Sys.Date(), "week", week_start = 1)- 7),
                  dateInput("end", "End date", value = floor_date(Sys.Date(), "week", week_start = 1) - 1),
                ),
                mainPanel(
                  tags$div(
                    class = "alert alert-secondary",
                    HTML(paste0(
                      "Rows expand to show scorers",
                      "<button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>"
                    ))
                  ),
                  reactableOutput("team_history_out")
                )
              )
      ),
      tabItem(tabName = "diagnostics", fluid=T,
              sidebarPanel(),
              mainPanel(
                dataTableOutput("diagnostics")
              )
      ),
      tabItem(tabName = "bug", fluid=T,
              mainPanel(
                tags$iframe(
                  src ="https://docs.google.com/forms/d/e/1FAIpQLScDhSXL2h8HYjTCuwdYKLTF3En2xPfE9O2BJet6VasuRdn2SQ/viewform?embedded=true",
                  width = "800",
                  height = "500",
                  frameborder = "0",
                  marginheight = "0",
                  marginwidth = "0"
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  league_master=reactiveVal("didsbury")
  
  
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
  
  output$team_history_out=renderReactable({
    
    period=daily  %>%
      filter(Date<=as.Date(input$end), 
             Date>=as.Date(input$start)) 
    
    league2=managers %>% 
      merge(period %>% 
              summarise(total=sum(SBgoals),.by=c("team", "league")),by=c("team", "league"), all = T) %>% 
      merge(period %>% 
              filter(position != "GOALKEEPER") %>% 
              summarise(gf=sum(SBgoals),.by=c("team", "league")), by=c("team", "league"), all=T) %>% 
      merge(period %>% 
              filter(position == "GOALKEEPER") %>% 
              summarise(ga=-sum(SBgoals),.by=c("team", "league")),by=c("team", "league"), all=T) %>%
      replace(is.na(.), 0) %>% 
      filter(league==input$league_team_history) %>%
      select(-league) %>% 
      arrange(-total,-gf)
    
    scorers2=period %>% 
      filter(league==input$league_team_history) %>% 
      filter(SBgoals!=0) %>%
      summarise(SBgoals=sum(SBgoals), .by=c("team", "position", "player", "club")) %>% 
      mutate(name = paste0(ifelse(position == "GOALKEEPER", club, sub(".*\\s", "", player)), if_else(SBgoals==1, "", paste0(" (", SBgoals, ")"))) %>% str_to_title()) %>% 
      summarise(scorers=paste(name, collapse = ", ", sep=""), .by="team") 
    
    
    res2=league2 %>% merge(scorers2, all.x=T)
    reactable(res2[,1:5],
              columns=list(
                team = colDef(width = 150, name = "" ),
                manager = colDef(width = 150, name = ""),
                total = colDef(width = 70, name = "Total"),
                gf = colDef(width = 70, name = "For"),
                ga = colDef(width = 70, name = "Against")
              ),
              details = function(index){
                div(
                  style = "padding: 16px;",
                  strong("Scorers: "),
                  paste0(res2$scorers[index])
                )
              },
              defaultPageSize = 15)
  })
  
  
  output$teamtext=renderUI({
    text1=paste("<b>League position:",league$rank[which(league$team==input$team)] , "</b>")
    text2=paste("<b>Score:", league$total[which(league$team==input$team)], "</b>")
    text3=paste("<font color=\"#4DAF4A\">For:", league$gf[which(league$team==input$team)], "</font>")
    text4=paste("<font color=\"#E41A1C\">Against:", league$ga[which(league$team==input$team)], "</font>")
    outfield=paste("Outfield transfers remaining:", 8-dl %>% filter(team==input$team, position!= "GOALKEEPER", cost=="") %>% nrow())
    goalie=paste("Goalkeeper transfers remaining:", 2-dl %>% filter(team==input$team, position== "GOALKEEPER", cost=="") %>% nrow())
    HTML(paste(text1, text2, text3, text4, outfield,goalie,sep="<br/>"))
  })
  
  output$img=renderImage({
    
    outfile=paste("img/", str_to_upper(str_replace_all(input$team, "[^[:alnum:]]", "")), ".png", sep="")
    hold=magick::image_read(outfile)
    
    list(
      src = outfile,
      contentType = "image/png",
      width = 100,
      height = round(100 * (magick::image_info(hold)$height / magick::image_info(hold)$width))
    )
    
    
  }, deleteFile = F)
  
  output$playerstaken=DT::renderDT({
    dl %>% filter(is.na(sold),
                  league==input$league) %>% 
      dplyr::select(team, player,club, position) %>% 
      rename_with(str_to_title)
  },
  options = list(
    autoWidth = TRUE,
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
  
  output$update_time=renderUI({ 
    HTML(paste0("Last score update: ",
                format(time$update_time, format="%Y-%m-%d %H:%M:%S"),
                "<br>Last file upload<br>Didsbury: ",
                format(time$mod_d, format="%Y-%m-%d %H:%M:%S"),
                "<br>Original: ",
                format(time$mod_o, format="%Y-%m-%d %H:%M:%S")
    ))
  })
  
  
  output$cup <- renderReactable({
    date <- cupties %>%
      filter(comp == input$comp_cup, round == input$round_cup) %>%
      pull(date) %>%
      min(na.rm = TRUE)
    
    weekend=daily %>%
      filter(
        Date >= date,
        Date <= date + lubridate::days(3)
      )
    
    
    scorers=weekend %>% 
      filter(SBgoals!=0) %>%
      mutate(name = paste0(ifelse(position == "GOALKEEPER", club, sub(".*\\s", "", player)), if_else(SBgoals==1, "", paste0(" (", SBgoals, ")"))) %>% str_to_title()) %>% 
      summarise(scorers=paste(name, collapse = ", ", sep=""), .by="team")
    
    
    main <- managers %>%
      merge(weekend %>%
              summarise(total = sum(SBgoals, na.rm = T), .by = "team"),
            by = "team",
            all.x = T) %>%
      merge(weekend %>% filter(position != "GOALKEEPER") %>%
              summarise(gf = sum(SBgoals), .by = "team"),
            by = "team",
            all.x = T) %>%
      merge(weekend %>% filter(position == "GOALKEEPER") %>%
              summarise(ga = -sum(SBgoals), .by = "team"),
            by = "team",
            all.x = T) %>%
      merge(scorers, .by="team") %>% 
      mutate(
        ga = replace(ga, is.na(ga), 0),
        total = replace(total, is.na(total), 0),
        gf = replace(gf, is.na(gf), 0)
      ) %>%
      arrange(-total, -gf) %>%
      mutate(
        team_manager = paste0(team, " (", manager, ")"),
        score = paste0(total, " (", gf, "-", ga, ")")
      ) %>%
      dplyr::select(team, team_manager, total, gf, score, scorers)
    
    res <- cupties %>%
      mutate(rn = row_number()) %>%
      filter(comp == input$comp_cup, round == input$round_cup) %>%
      merge(main, by.x = "team1", by.y = "team") %>%
      merge(main, by.x = "team2", by.y = "team") %>%
      mutate(winner = case_when(
        total.x > total.y ~ 1,
        total.x < total.y ~ 2,
        gf.x > gf.y ~ 1,
        gf.x < gf.y ~ 2
      )) %>%
      arrange(rn) %>%
      dplyr::select(team_manager.x, score.x, score.y, team_manager.y, winner, scorers.x, scorers.y, team1, team2)
    
    reactable(
      res[, 1:4],
      columns = list(
        team_manager.x = colDef(
          name = "",
          width = 150,
          style = function(value, index) {
            if (!is.na(res$winner[index]) && res$winner[index] == 1) {
              list(background = "#FFD700")
            }
          }
        ),
        score.x = colDef(
          name = "",
          width = 70,
          style = function(value, index) {
            if (!is.na(res$winner[index]) && res$winner[index] == 1) {
              list(background = "#FFD700")
            }
          }
        ),
        score.y = colDef(
          name = "",
          width = 70,
          style = function(value, index) {
            if (!is.na(res$winner[index]) && res$winner[index] == 2) {
              list(background = "#FFD700")
            }
          }
        ),
        team_manager.y = colDef(
          name = "",
          width = 150,
          style = function(value, index) {
            if (!is.na(res$winner[index]) && res$winner[index] == 2) {
              list(background = "#FFD700")
            }
          }
        )
      ),
      details = function(index) {
        div(
          style = "padding: 16px;",
          strong("Scorers:"),
          br(),
          paste0(res$team1[index], ": ", res$scorers.x[index]),
          br(),
          paste0(res$team2[index], ": ", res$scorers.y[index])
        )
      }
      
    )
  })
  # maintaining pickers across tabs
  observeEvent(input$league,
               {
                 
                 #updateRadioButtons(session, "league", selected = input$league)
                 updateRadioButtons(session, "league_teams", selected = input$league)
                 updateRadioButtons(session, "league_players", selected = input$leagues)
                 updateRadioButtons(session, "league_team_history", selected = input$league)
               })
  
  observeEvent(input$league_teams,
               {
                 updateRadioButtons(session, "league", selected = input$league_teams)
                 # updateRadioButtons(session, "league_teams", selected = input$league_teams)
                 updateRadioButtons(session, "league_players", selected = input$league_teams)
                 updateRadioButtons(session, "league_team_history", selected = input$league_teams)
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
                 # updateRadioButtons(session, "league_players", selected = input$league_players)
                 updateRadioButtons(session, "league_team_history", selected = input$league_players)
               })
  
  observeEvent(input$league_team_history,
               {
                
                 updateRadioButtons(session, "league", selected = input$league_team_history)
                 updateRadioButtons(session, "league_teams", selected = input$league_team_history)
                 updateRadioButtons(session, "league_players", selected = input$league_team_history)
                 # updateRadioButtons(session, "league_team_history", selected = input$league_team_history)
                 
                 
               })
  
  observeEvent(input$round_cup,
               {
                 updatePickerInput(session, "comp_cup", selected = cupties %>% filter(comp==input$comp_cup) %>% slice_max(date, with_ties = F) %>% pull(round))
                 
                 
                 
               })
}

# Run the application 
shinyApp(ui = ui, server = server)