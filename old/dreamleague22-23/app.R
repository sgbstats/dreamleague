library(shiny)
library(tidyverse)
library(shinyWidgets)
library(lubridate)
library(readxl)
library(ggplot2)
# library(ggsankey)
library(htmltools)
library(flextable)
library(htmlwidgets)
library(shinydashboard)

library(googlesheets4)
`%notin%` = Negate(`%in%`)
options(gargle_oauth_cache = ".secrets")
gs4_auth(
  cache = ".secrets",
  email = "sebastiangbate@gmail.com",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
)
# preprocessing
dl = googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Rg7qc_DOyyAn5dIG5KbDVguTPx2h4zDMxB8y5kQfmHQ/edit#gid=0",
  na = c("SOLD", ""),
  col_names = F
)

teams = dl %>%
  dplyr::select(1:7) %>%
  rename(
    position = 1,
    player = 2,
    club = 3,
    cost = 4,
    goals = 5,
    sold = 7,
    bought = 6
  ) %>%
  mutate(team = NA_character_)

managers = dl %>%
  dplyr::select(10:11) %>%
  na.omit() %>%
  rename(manager = 1, team = 2) %>%
  filter(team != "TEAM") %>%
  mutate(manager = as.character(manager), team = as.character(team))

teams2 = teams
for (i in 1:nrow(teams)) {
  if (teams$player[i] %in% managers$team) {
    team = teams$player[i]
  }
  if (
    teams$position[i] %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")
  ) {
    teams2$team[i] = team
  }
}

teams2 = teams2 %>%
  filter(position %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")) %>%
  mutate(
    goals = if_else(
      position == "GOALKEEPER",
      -abs(as.numeric(goals)),
      as.numeric(goals)
    ),
    bought = format(as.Date(bought), "%d-%b"),
    sold = format(as.Date(sold), "%d-%b")
  )

league = managers %>%
  merge(
    teams2 %>% group_by(team) %>% summarise(total = sum(goals)),
    by = "team"
  ) %>%
  merge(
    teams2 %>%
      filter(goals > 0) %>%
      group_by(team) %>%
      summarise(gf = sum(goals)),
    by = "team"
  ) %>%
  merge(
    teams2 %>%
      filter(goals < 0) %>%
      group_by(team) %>%
      summarise(ga = -sum(goals)),
    by = "team"
  ) %>%
  arrange(-total, -gf)


# load("Data.RDa")
teamslist = (league %>% arrange(team))$team
names(teamslist) = paste(
  (league %>% arrange(team))$team,
  " (",
  (league %>% arrange(team))$manager,
  ")",
  sep = ""
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "red",
  # md = TRUE,
  dashboardHeader(title = "Dream League"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("League", tabName = "league", icon = icon("table")),
      menuItem("Teams", tabName = "teams", icon = icon("shirt")),
      menuItem("Players taken", tabName = "players", icon = icon("user-xmark"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "league",
        fluid = T,
        uiOutput("table", inline = TRUE, style = "margin:0px; padding:0px")
      ),
      tabItem(
        tabName = "teams",
        fluid = T,
        sidebarLayout(
          sidebarPanel(
            pickerInput("team", "Team", choices = teamslist, selected = NULL),
            checkboxInput("current", "Current team only", value = T),
            imageOutput("img", inline = T),
            htmlOutput("teamtext")
          ),
          mainPanel(
            dataTableOutput("team")
          )
        )
      ),
      tabItem(
        tabName = "players",
        fluid = T,
        mainPanel(
          dataTableOutput("playerstaken")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$table = renderUI({
    league %>%
      flextable() %>%
      set_header_labels(
        team = "Team",
        manager = "Manager",
        total = "Total",
        gf = "For",
        ga = "Against"
      ) %>%
      bg(i = 1:2, bg = c("#FFD700", "#C0C0C0")) %>%
      autofit() %>%
      font(fontname = "Arial", part = "all") %>%
      htmltools_value()
  })

  output$team = renderDataTable(
    {
      if (input$current) {
        teams3 = teams2 %>%
          filter(team == input$team) %>%
          filter(is.na(sold)) %>%
          select(-sold)
      } else {
        teams3 = teams2 %>% filter(team == input$team)
      }

      teams3 %>% select(-team) %>% rename_with(str_to_title)
    },
    options = list(
      autoWidth = TRUE,
      columnDefs = list(
        list(width = '100px', targets = c(1, 2)),
        list(width = '50px', targets = c(0, 3)),
        list(width = '30px', targets = c(4, 5))
      ),
      scrollX = T
    )
  )

  output$teamtext = renderUI({
    text1 = paste(
      "<b>League position:",
      which(league$team == input$team),
      "</b>"
    )
    text2 = paste(
      "<b>Score:",
      league$total[which(league$team == input$team)],
      "</b>"
    )
    text3 = paste(
      "<font color=\"#4DAF4A\">For:",
      league$gf[which(league$team == input$team)],
      "</font>"
    )
    text4 = paste(
      "<font color=\"#E41A1C\">Against:",
      league$ga[which(league$team == input$team)],
      "</font>"
    )
    outfield = paste(
      "Outfield transfers remaining:",
      8 -
        teams2 %>%
          filter(
            team == input$team,
            position != "GOALKEEPER",
            cost == "TRANSFER"
          ) %>%
          nrow()
    )
    goalie = paste(
      "Goalkeeper transfers remaining:",
      2 -
        teams2 %>%
          filter(
            team == input$team,
            position == "GOALKEEPER",
            cost == "TRANSFER"
          ) %>%
          nrow()
    )
    HTML(paste(text1, text2, text3, text4, outfield, goalie, sep = "<br/>"))
  })

  output$img = renderImage(
    {
      outfile = paste("img/", input$team, ".png", sep = "")

      list(
        src = outfile,
        contentType = 'image/png',
        width = 100,
        height = 100,
        alt = "This is alternate text"
      )
    },
    deleteFile = F
  )

  output$playerstaken = renderDataTable({
    teams2 %>%
      filter(is.na(sold)) %>%
      dplyr::select(team, player, club, position) %>%
      rename_with(str_to_title)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
