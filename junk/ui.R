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

library(googlesheets4)
`%notin%`=Negate(`%in%`)

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
                 "CHRIS MCDERMOTT & ADG",	"TIKI TAKA FC",
                 "JAMIE DUGGAN",	"DUGGAN'S DREAMERS",
                 "CHARLIE REED",	"CHARLIE'S ANGELS"
)

teamslist=(managers %>% arrange(team))$team
names(teamslist)=paste((managers %>% arrange(team))$team, " (", (managers%>% arrange(team))$manager, ")", sep="")

ui <- dashboardPage(
  skin="red",
  # md = TRUE,
  dashboardHeader(title = "Dream League"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("League", tabName = "league", icon = icon("table"))
      #menuItem("Teams", tabName = "teams", icon = icon("shirt")),
      #menuItem("Players taken", tabName = "players", icon = icon("user-xmark"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "league", fluid=T,
              uiOutput("table",inline = TRUE, style = "margin:0px; padding:0px")
      )
      # tabItem(tabName = "teams", fluid=T,
      #         sidebarLayout(
      #           sidebarPanel(
      #             pickerInput("team", "Team", choices = teamslist, selected = NULL),
      #             checkboxInput("current","Current team only", value=T),
      #             imageOutput("img", inline = T),
      #             htmlOutput("teamtext")
      #             
      #           ),
      #           mainPanel(
      #            # dataTableOutput("team")
      #           )
      #         )
      #         
      # ),
      # tabItem(tabName = "players", fluid=T,
      #         mainPanel(
      #           #dataTableOutput("playerstaken")
      #         ))
    )
  )
)