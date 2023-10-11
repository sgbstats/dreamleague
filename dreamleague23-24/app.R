library(shiny)
library(tidyverse)
library(shinyWidgets)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggsankey)
library(htmltools)
library(flextable)
library(htmlwidgets)
library(shinydashboard)
library(dplyr)
library(googlesheets4)
# library(golem)


`%notin%`=Negate(`%in%`)
options(gargle_oauth_cache = ".secrets",
        gargle_oauth_email = TRUE)
gs4_auth(
  cache = ".secrets",
  email = "sebastiangbate@gmail.com",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
)
# preprocessing
dl=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0",
                             sheet = "scores",
                             na=c("SOLD",""),
                             col_names = T) 

weekly=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0",
                                 sheet = "weekly",
                                 na=c("SOLD",""),
                                 col_names = T) 

weeks=seq.Date(as.Date("2023-07-24"), by=7, length.out = 52)
weeks2=weeks[weeks<=Sys.Date()]
weekschar=format(weeks2, format="%d-%b")
weekmatch=cbind.data.frame(weekschar, weeks2)

managers=tribble(~manager, ~team,
                 "BRYN COOMBE",	"DURHAM DYNAMO",
                 "ROB PICKETT",	"ROBS ROVERS",
                 "JOE HARPER",	"CASEMIGOS",
                 "SEAN HELSBY",	"SHEFSPANYOL",
                 "MIKE ASTLEY","SPORTING HOOF",
                 "MARK WHITTLE",	"RASH IN THE ATTIC",
                 "RICHARD KILPATRICK",	"ERIMUS",
                 "CHRIS DUFFY",	"NUTMEG UTD",
                 "SEB BATE",	"MICHU IN DE GEA BAR",
                 "SAM HARPER",	"ONE AND A HALF A HARPER FC",
                 "TAMIKO JELENJE",	"DREAMVILLE",
                 "CHRIS MCD, JIMBO & ADG",	"TIKI TAKA FC",
                 "JAMIE DUGGAN",	"DUGGAN'S DREAMERS",
                 "CHARLIE REED",	"CHARLIE'S ANGELS"
)


league=managers %>% merge(dl %>% group_by(team) %>% 
                            summarise(total=sum(SBgoals)), by="team") %>% 
  merge(dl%>% filter(position != "GOALKEEPER") %>% group_by(team) %>% 
          summarise(gf=sum(SBgoals)), by="team") %>% 
  merge(dl %>% filter(position == "GOALKEEPER") %>% group_by(team) %>% 
          summarise(ga=-sum(SBgoals)), by="team") %>% 
  arrange(-total,-gf)

teamslist=(league %>% arrange(team))$team
names(teamslist)=paste((league %>% arrange(team))$team, " (", (league %>% arrange(team))$manager, ")", sep="")

ui <- dashboardPage(
  skin="red",
  # md = TRUE,
  dashboardHeader(title = "DreamLeague"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("League", tabName = "league", icon = icon("table")),
      menuItem("Teams", tabName = "teams", icon = icon("shirt")),
      menuItem("Players taken", tabName = "players", icon = icon("user-xmark")),
      menuItem("Weekly scores", tabName = "weekly", icon = icon("calendar"),
               startExpanded = F,
               menuSubItem("Weekly Goals", tabName = "weekly2", icon = icon("futbol")),
               menuSubItem("Weekly League", tabName = "league_weekly", icon = icon("table"))),
      menuItem("Diagnostics", tabName = "diagnostics", icon=icon("stethoscope"))
      #menuItem("Report an issue", tabName = "issues", icon = icon("circle-exclamation"))
    )
  ),
  
  dashboardBody(
    #tags$head(tags$link(rel = "shortcut icon", href = "img/favicon.ico")),
    tabItems(
      tabItem(tabName = "league", fluid=T,
              uiOutput("table",inline = TRUE, style = "margin:0px; padding:0px")
      ),
      tabItem(tabName = "teams", fluid=T,
              sidebarLayout(
                sidebarPanel(
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
              mainPanel(
                dataTableOutput("playerstaken")
              )),
      tabItem(tabName = "weekly", fluid=T,
              mainPanel(
                #dataTableOutput("playerstaken")
              )),
      tabItem(tabName = "league_weekly", fluid=T,
              sidebarPanel(

                pickerInput("week_league", "Game week", choices = weekschar, selected = weekschar[length(weekschar)-1])),
              mainPanel(
                uiOutput("table_weekly",inline = TRUE, style = "margin:0px; padding:0px")
              )
              
      ),
      tabItem(tabName = "weekly2", fluid=T,
              sidebarLayout(
                sidebarPanel(
                  pickerInput("team2", "Team", choices = teamslist, selected = NULL),
                  pickerInput("week_player", "Game week", choices = weekschar, selected = weekschar[length(weekschar)-1])),
                mainPanel(
                  dataTableOutput("team_weekly")
                )
              )
      ) ,
      tabItem(tabName = "diagnostics", fluid=T,
              mainPanel(
                dataTableOutput("diagnostics")
              )),
      tabItem(tabName = "issues", fluid=T,
              mainPanel(
                
              ))
      
    )
  )
)

server <- function(input, output) {
  

  
  output$table=renderUI({
    league%>% flextable() %>% 
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
    
    league2=managers %>% merge(weekly %>%mutate(week2=format(week, format="%d-%b")) %>% 
                                 filter(week2 ==input$week_league) %>%
                                 group_by(team) %>% 
                                 summarise(total=sum(SBgoals)), by="team", all = T) %>% 
      merge(weekly %>%mutate(week2=format(week, format="%d-%b")) %>% 
              filter(week2 ==input$week_league)%>% 
              filter(position != "GOALKEEPER") %>% group_by(team) %>% 
              summarise(gf=sum(SBgoals)), by="team", all=T) %>% 
      merge(weekly %>%mutate(week2=format(week, format="%d-%b")) %>% 
              filter(week2 ==input$week_league) %>% 
              filter(position == "GOALKEEPER") %>% group_by(team) %>% 
              summarise(ga=-sum(SBgoals)), by="team", all=T) %>%
      replace(is.na(.), 0) %>% 
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
  
  output$team=renderDataTable({
    
    if(input$current)
    {
      teams3= dl%>% filter(team==input$team)  %>% filter(is.na(sold)) %>% select(-sold, -bought2, -sold2, -SBapp)
    }else
    {
      teams3= dl%>% filter(team==input$team)  %>% select( -bought2, -sold2, -SBapp)
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
    scrollX=T)
  )
  
  output$team_weekly=renderDataTable({
    
    
    teams4= weekly%>% filter(team==input$team2)  %>% 
      mutate(week2=format(week, format="%d-%b")) %>% 
      filter(is.na(sold)) %>% 
      filter(week2 ==input$week_player) %>%
      filter(SBgoals!=0) %>% 
      select(-sold, -bought2, -sold2, -App, -week, -week2)
    
    
    teams4 %>% select(-team, -cost2) %>%
      # select(-goals) %>%
      rename("Goals"="SBgoals")%>%
      rename_with(str_to_title) %>%
      relocate(Goals, .after=Club)
    

    
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '100px', targets = c(1,2)),
                      list(width = '50px', targets = c(0,3)),
                      list(width = '30px', targets = c(4,5))),
    scrollX=T)
  )
  
  output$teamtext=renderUI({
    text1=paste("<b>League position:", which(league$team==input$team), "</b>")
    text2=paste("<b>Score:", league$total[which(league$team==input$team)], "</b>")
    text3=paste("<font color=\"#4DAF4A\">For:", league$gf[which(league$team==input$team)], "</font>")
    text4=paste("<font color=\"#E41A1C\">Against:", league$ga[which(league$team==input$team)], "</font>")
    outfield=paste("Outfield transfers remaining:", 8-dl %>% filter(team==input$team, position!= "GOALKEEPER", cost=="TRANSFER") %>% nrow())
    goalie=paste("Goalkeeper transfers remaining:", 2-dl %>% filter(team==input$team, position== "GOALKEEPER", cost=="TRANSFER") %>% nrow())
    HTML(paste(text1, text2, text3, text4, outfield,goalie,sep="<br/>"))
  })
  
  output$img=renderImage({
    outfile=paste("img/", input$team, ".png", sep="")
    
    list(src = outfile,
         contentType = 'image/png',
         width = 100,
         height = 100)
    
  }, deleteFile = F)
  
  output$playerstaken=renderDataTable({
    dl %>% filter(is.na(sold)) %>% 
      dplyr::select(team, player,club, position) %>% 
      rename_with(str_to_title)
  })
  
  output$diagnostics=renderDataTable({
    dl %>% filter(is.na(sold)) %>% 
      dplyr::select(team, player,club, position) %>% 
      mutate(position=factor(position, c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"), ordered = T)) %>% 
      count(team, position) %>% 
      pivot_wider(names_from = "position", values_from = "n") %>% 
      filter(GOALKEEPER!=1|DEFENDER!=2|MIDFIELDER!=3|FORWARD!=5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
