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

options(gargle_oauth_cache = ".secrets", gargle_oauth_email = TRUE)
googledrive::drive_auth(
  path = "credentials.json"
)
googledrive::drive_download(
  googledrive::as_id("108pNlDYjniFZiPU3PG82bIdChZmZGqUh"),
  path = "data.RDa",
  overwrite = T
)
load("data.RDa")
weeks = seq.Date(as.Date("2025-07-28"), by = 7, length.out = 52)
weeks2 = weeks[weeks <= Sys.Date()]
weekschar = format(weeks2, format = "%d-%b")
names(weeks2) = weekschar

load("managers.RDa")

managers = rbind.data.frame(
  managers_d |> mutate(league = "didsbury"),
  managers_o |> mutate(league = "original")
)
league = managers |>
  merge(
    dl |> group_by(team) |> summarise(total = sum(SBgoals, na.rm = T)),
    by = "team",
    all = T
  ) |>
  merge(
    dl |>
      filter(position != "GOALKEEPER") |>
      group_by(team) |>
      summarise(gf = sum(SBgoals, na.rm = T)),
    by = "team",
    all = T
  ) |>
  merge(
    dl |>
      filter(position == "GOALKEEPER") |>
      group_by(team) |>
      summarise(ga = -sum(SBgoals, na.rm = T)),
    by = "team",
    all = T
  ) |>
  arrange(-total, -gf) |>
  mutate(rank = row_number(), .by = "league")

teamslist = (managers |> arrange(team))$team
names(teamslist) = paste(
  (league |> arrange(team))$team,
  " (",
  (league |> arrange(team))$manager,
  ")",
  sep = ""
)
teamslist_cup = (managers |> arrange(team))$team

names(teamslist_cup) = paste(
  (managers |> arrange(team))$team,
  " (",
  (managers |> arrange(team))$manager,
  ")",
  sep = ""
)

rounds = unique(cupties$round)
ui <- dashboardPage(
  skin = "red",
  # md = TRUE,
  dashboardHeader(title = "DreamLeague"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("League", tabName = "league", icon = icon("table")),
      menuItem("Teams", tabName = "teams", icon = icon("shirt")),
      menuItem("BFL Cup", tabName = "cup", icon = icon("trophy")),
      menuItem("Players taken", tabName = "players", icon = icon("user-xmark")),
      menuItem(
        "History",
        tabName = "history",
        icon = icon("clock-rotate-left")
      ),
      menuItem(
        "Diagnostics",
        tabName = "diagnostics",
        icon = icon("stethoscope")
      ),
      menuItem("Report an issue", tabName = "bug", icon = icon("bug"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "league",
        fluid = T,
        sidebarLayout(
          sidebarPanel(
            h3("Last Updated"),
            uiOutput("update_time"),
            radioButtons(
              "league",
              "League",
              choices = c("Didsbury" = "didsbury", "Original" = "original"),
              selected = "didsbury"
            )
          ),
          mainPanel(
            tags$div(
              class = "alert alert-warning",
              style = "margin:0; padding:2px 6px;", # tight alert
              HTML(
                paste0(
                  "Some goals may be missing due to changes in soccerbase. Please ",
                  "<a href=\"#shiny-tab-bug\" data-toggle=\"tab\">report an issue</a>",
                  " so it can be fixed. ",
                  "<button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>"
                )
              )
            ),
            reactableOutput("table")
          )
        )
      ),
      tabItem(
        tabName = "teams",
        fluid = TRUE,
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
            htmlOutput("teamtext"),
            br(),
            actionButton("goto_league", "Return to League")
          ),
          mainPanel(reactableOutput("team_out"))
        )
      ),
      tabItem(
        tabName = "players",
        fluid = T,
        sidebarLayout(
          sidebarPanel(
            radioButtons(
              "league_players",
              "League",
              choices = c("Didsbury" = "didsbury", "Original" = "original"),
              selected = "didsbury"
            )
          ),
          mainPanel(
            reactableOutput("playerstaken")
          )
        )
      ),
      tabItem(
        tabName = "cup",
        fluid = T,
        sidebarPanel(
          radioButtons(
            "comp_cup",
            "Competition ",
            choices = c(
              "BFL Challenge Cup" = "bfl",
              "Didsbury Cup" = "didsbury",
              "Original Cup" = "original"
            ),
            selected = "bfl"
          ),
          pickerInput(
            "round_cup",
            "Round",
            choices = rounds,
            selected = cupties |>
              filter(comp == "bfl") |>
              slice_max(date, with_ties = F) |>
              pull(round),
            multiple = F
          )
        ),
        mainPanel(
          tags$div(
            class = "alert alert-secondary",
            style = "margin:0; padding:2px 6px;", # tight alert
            HTML(paste0(
              "Rows expand to show scorers",
              "<button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>"
            ))
          ),
          div(
            style = "margin:0; padding:0;",
            reactableOutput("cup")
          )
        )
      ),
      tabItem(
        tabName = "history",
        fluid = T,
        sidebarLayout(
          sidebarPanel(
            radioButtons(
              "league_team_history",
              "League",
              choices = c("Didsbury" = "didsbury", "Original" = "original"),
              selected = "didsbury"
            ),
            dateInput("start", "Start date", value = Sys.Date() - 6),
            dateInput("end", "End date", value = Sys.Date()),
          ),
          mainPanel(
            tags$div(
              class = "alert alert-secondary",
              style = "margin:0; padding:2px 6px;", # tight alert
              HTML(paste0(
                "Rows expand to show scorers, data defaults to last 7 days.",
                "<button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>"
              ))
            ),
            div(
              style = "margin:0; padding:0;",
              reactableOutput("team_history_out")
            )
          )
        )
      ),
      tabItem(
        tabName = "diagnostics",
        fluid = T,
        sidebarPanel(),
        mainPanel(
          dataTableOutput("diagnostics")
        )
      ),
      tabItem(
        tabName = "bug",
        fluid = T,
        mainPanel(
          tags$iframe(
            src = "https://docs.google.com/forms/d/e/1FAIpQLScDhSXL2h8HYjTCuwdYKLTF3En2xPfE9O2BJet6VasuRdn2SQ/viewform?embedded=true",
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
  league_master = reactiveVal("didsbury")

  output$table <- renderReactable({
    table_data <- league |>
      filter(league == input$league)

    reactable(
      table_data |> select(-league),
      columns = list(
        rank = colDef(show = FALSE),
        team = colDef(
          name = "Team",
          width = 200,
          cell = function(value) {
            escaped_value <- gsub("'", "\\\\'", value)
            tags$span(
              style = "cursor: pointer; text-decoration: underline; color: #000000;",
              onclick = sprintf(
                "Shiny.setInputValue('goto_team', {team: '%s', nonce: Math.random()})",
                escaped_value
              ),
              value
            )
          }
        ),
        manager = colDef(name = "Manager", width = 200),
        total = colDef(name = "Total", width = 70),
        gf = colDef(name = "For", width = 70),
        ga = colDef(name = "Against", width = 70)
      ),
      defaultPageSize = 15,
      fullWidth = FALSE,
      rowStyle = function(index) {
        if (table_data[index, "rank"] == 1) {
          list(background = "#FFD700")
        } else if (table_data[index, "rank"] == 2) {
          list(background = "#C0C0C0")
        }
      }
    )
  })

  output$team_out <- renderReactable({
    if (input$current) {
      teams3 <- dl |>
        filter(team == input$team) |>
        filter(is.na(sold)) |>
        select(-sold, -bought2, -sold2, -SBapp, -league)
    } else {
      teams3 <- dl |>
        filter(team == input$team) |>
        select(-bought2, -sold2, -SBapp, -league)
    }

    table_data_unformatted <- teams3 |>
      select(-goals)

    table_data <- table_data_unformatted |>
      select(-team) |>
      rename("Goals" = "SBgoals") |>
      rename_with(str_to_title) |>
      relocate(Goals, .after = Club)

    reactable(
      table_data,
      sortable = TRUE,
      searchable = TRUE,
      columns = list(
        Player = colDef(width = 150),
        Club = colDef(width = 150),
        Position = colDef(width = 100),
        Goals = colDef(width = 70),
        Cost = colDef(width = 70),
        Bought = colDef(width = 100)
      ),
      defaultPageSize = 15,
      details = function(index) {
        player_name <- table_data_unformatted$player[index]
        team_name <- table_data_unformatted$team[index]

        if (is.na(player_name)) {
          scoring_history <- daily |>
            filter(team == team_name, SBgoals != 0, position == "GOALKEEPER") |>
            select(date = Date, goals = SBgoals) |>
            arrange(desc(date))
        } else {
          scoring_history <- daily |>
            filter(player == player_name, team == team_name, SBgoals != 0) |>
            select(date = Date, goals = SBgoals) |>
            arrange(desc(date))
        }

        if (nrow(scoring_history) > 0) {
          htmltools::div(
            style = "padding: 1rem",
            reactable(
              scoring_history,
              outlined = TRUE,
              bordered = TRUE,
              striped = TRUE,
              fullWidth = FALSE,
              columns = list(
                date = colDef(name = "Date", width = 100),
                goals = colDef(name = "Goals", width = 70)
              )
            )
          )
        } else {
          htmltools::div(style = "padding: 1rem", "No goals recorded.")
        }
      }
    )
  })

  output$team_history_out = renderReactable({
    period = daily |>
      filter(Date <= as.Date(input$end), Date >= as.Date(input$start))

    league2 = managers |>
      merge(
        period |>
          summarise(total = sum(SBgoals), .by = c("team", "league")),
        by = c("team", "league"),
        all = T
      ) |>
      merge(
        period |>
          filter(position != "GOALKEEPER") |>
          summarise(gf = sum(SBgoals), .by = c("team", "league")),
        by = c("team", "league"),
        all = T
      ) |>
      merge(
        period |>
          filter(position == "GOALKEEPER") |>
          summarise(ga = -sum(SBgoals), .by = c("team", "league")),
        by = c("team", "league"),
        all = T
      ) |>
      replace(is.na(.), 0) |>
      filter(league == input$league_team_history) |>
      select(-league) |>
      arrange(-total, -gf)

    scorers2 = period |>
      filter(league == input$league_team_history) |>
      filter(SBgoals != 0) |>
      summarise(
        SBgoals = sum(SBgoals),
        .by = c("team", "position", "player", "club")
      ) |>
      mutate(
        name = paste0(
          ifelse(position == "GOALKEEPER", club, sub(".*\\s", "", player)),
          if_else(SBgoals == 1, "", paste0(" (", SBgoals, ")"))
        ) |>
          str_to_title()
      ) |>
      summarise(scorers = paste(name, collapse = ", ", sep = ""), .by = "team")

    res2 = league2 |> merge(scorers2, all.x = T)
    reactable(
      res2[, 1:5],
      columns = list(
        team = colDef(width = 150, name = ""),
        manager = colDef(width = 150, name = ""),
        total = colDef(width = 70, name = "Total"),
        gf = colDef(width = 70, name = "For"),
        ga = colDef(width = 70, name = "Against")
      ),
      details = function(index) {
        div(
          style = "padding: 16px;",
          strong("Scorers: "),
          paste0(res2$scorers[index])
        )
      },
      defaultPageSize = 15
    )
  })

  output$teamtext = renderUI({
    text1 = paste(
      "<b>League position:",
      league$rank[which(league$team == input$team)],
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
        dl |>
          filter(team == input$team, position != "GOALKEEPER", cost == "") |>
          nrow()
    )
    goalie = paste(
      "Goalkeeper transfers remaining:",
      2 -
        dl |>
          filter(team == input$team, position == "GOALKEEPER", cost == "") |>
          nrow()
    )
    HTML(paste(text1, text2, text3, text4, outfield, goalie, sep = "<br/>"))
  })

  output$img = renderImage(
    {
      outfile = paste(
        "img/",
        str_to_upper(str_replace_all(input$team, "[^[:alnum:]]", "")),
        ".png",
        sep = ""
      )
      hold = magick::image_read(outfile)

      list(
        src = outfile,
        contentType = "image/png",
        width = 100,
        height = round(
          100 *
            (magick::image_info(hold)$height / magick::image_info(hold)$width)
        )
      )
    },
    deleteFile = F
  )

  output$playerstaken <- renderReactable({
    table_data <- dl |>
      filter(is.na(sold), league == input$league_players) |>
      dplyr::select(team, player, club, position) |>
      rename_with(str_to_title)

    reactable(
      table_data,
      searchable = TRUE,
      columns = list(
        Team = colDef(width = 150),
        Player = colDef(width = 150),
        Club = colDef(width = 150),
        Position = colDef(width = 100)
      ),
      defaultPageSize = 15
    )
  })

  output$diagnostics = DT::renderDT({
    dl |>
      filter(is.na(sold)) |>
      dplyr::select(team, player, club, position) |>
      mutate(
        position = factor(
          position,
          c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"),
          ordered = T
        )
      ) |>
      count(team, position) |>
      pivot_wider(names_from = "position", values_from = "n") |>
      filter(GOALKEEPER != 1 | DEFENDER != 2 | MIDFIELDER != 3 | FORWARD != 5)
  })

  output$update_time = renderUI({
    HTML(paste0(
      "Last score update: ",
      format(time$update_time, format = "%Y-%m-%d %H:%M:%S"),
      "<br>Last file upload<br>Didsbury: ",
      format(time$mod_d, format = "%Y-%m-%d %H:%M:%S"),
      "<br>Original: ",
      format(time$mod_o, format = "%Y-%m-%d %H:%M:%S")
    ))
  })

  output$cup <- renderReactable({
    date <- cupties |>
      filter(comp == input$comp_cup, round == input$round_cup) |>
      pull(date) |>
      min(na.rm = TRUE)

    weekend = daily |>
      filter(
        Date >= date,
        Date <= date + lubridate::days(3)
      )

    scorers = weekend |>
      filter(SBgoals != 0) |>
      mutate(
        name = paste0(
          ifelse(position == "GOALKEEPER", club, sub(".*\\s", "", player)),
          if_else(SBgoals == 1, "", paste0(" (", SBgoals, ")"))
        ) |>
          str_to_title()
      ) |>
      summarise(scorers = paste(name, collapse = ", ", sep = ""), .by = "team")

    main <- managers |>
      merge(
        weekend |>
          summarise(total = sum(SBgoals, na.rm = T), .by = "team"),
        by = "team",
        all.x = T
      ) |>
      merge(
        weekend |>
          filter(position != "GOALKEEPER") |>
          summarise(gf = sum(SBgoals), .by = "team"),
        by = "team",
        all.x = T
      ) |>
      merge(
        weekend |>
          filter(position == "GOALKEEPER") |>
          summarise(ga = -sum(SBgoals), .by = "team"),
        by = "team",
        all.x = T
      ) |>
      merge(scorers, .by = "team", all.x = T) |>
      mutate(
        ga = replace(ga, is.na(ga), 0),
        total = replace(total, is.na(total), 0),
        gf = replace(gf, is.na(gf), 0),
        scorers = replace(scorers, is.na(scorers), "")
      ) |>
      arrange(-total, -gf) |>
      mutate(
        team_manager = paste0(team, " (", manager, ")"),
        score = paste0(total, " (", gf, "-", ga, ")")
      ) |>
      dplyr::select(team, team_manager, total, gf, score, scorers)

    res <- cupties |>
      mutate(rn = row_number()) |>
      filter(comp == input$comp_cup, round == input$round_cup) |>
      merge(main, by.x = "team1", by.y = "team") |>
      merge(main, by.x = "team2", by.y = "team") |>
      mutate(
        winner = case_when(
          total.x > total.y ~ 1,
          total.x < total.y ~ 2,
          gf.x > gf.y ~ 1,
          gf.x < gf.y ~ 2
        )
      ) |>
      arrange(rn) |>
      dplyr::select(
        team_manager.x,
        score.x,
        score.y,
        team_manager.y,
        winner,
        scorers.x,
        scorers.y,
        team1,
        team2
      )

    reactable(
      res[, 1:4],
      columns = list(
        team_manager.x = colDef(
          name = "",
          show = T,
          width = 150,
          style = function(value, index) {
            if (!is.na(res$winner[index]) && res$winner[index] == 1) {
              list(background = "#FFD700")
            }
          }
        ),
        score.x = colDef(
          name = "",
          show = T,
          width = 70,
          style = function(value, index) {
            if (!is.na(res$winner[index]) && res$winner[index] == 1) {
              list(background = "#FFD700")
            }
          }
        ),
        score.y = colDef(
          name = "",
          show = T,
          width = 70,
          style = function(value, index) {
            if (!is.na(res$winner[index]) && res$winner[index] == 2) {
              list(background = "#FFD700")
            }
          }
        ),
        team_manager.y = colDef(
          name = "",
          show = T,
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
      },
      defaultColDef = colDef(header = NULL)
    )
  })
  # maintaining pickers across tabs
  observeEvent(input$league, {
    #updateRadioButtons(session, "league", selected = input$league)
    updateRadioButtons(session, "league_teams", selected = input$league)
    updateRadioButtons(session, "league_players", selected = input$leagues)
    updateRadioButtons(session, "league_team_history", selected = input$league)
  })

  observeEvent(input$league_teams, {
    updateRadioButtons(session, "league", selected = input$league_teams)
    # updateRadioButtons(session, "league_teams", selected = input$league_teams)
    updateRadioButtons(session, "league_players", selected = input$league_teams)
    updateRadioButtons(
      session,
      "league_team_history",
      selected = input$league_teams
    )
    teamslist = (managers |>
      arrange(team) |>
      filter(league == input$league_teams))$team

    names(teamslist) = paste(
      (managers |>
        arrange(team) |>
        filter(league == input$league_teams))$team,
      " (",
      (managers |>
        arrange(team) |>
        filter(league == input$league_teams))$manager,
      ")",
      sep = ""
    )

    updatePickerInput(session, "team", choices = teamslist)
  })

  observeEvent(input$league_players, {
    #players_taken list
    updateRadioButtons(session, "league", selected = input$league_players)
    updateRadioButtons(session, "league_teams", selected = input$league_players)
    # updateRadioButtons(session, "league_players", selected = input$league_players)
    updateRadioButtons(
      session,
      "league_team_history",
      selected = input$league_players
    )
  })

  observeEvent(input$league_team_history, {
    updateRadioButtons(session, "league", selected = input$league_team_history)
    updateRadioButtons(
      session,
      "league_teams",
      selected = input$league_team_history
    )
    updateRadioButtons(
      session,
      "league_players",
      selected = input$league_team_history
    )
    # updateRadioButtons(session, "league_team_history", selected = input$league_team_history)
  })

  observeEvent(input$round_cup, {
    updatePickerInput(
      session,
      "comp_cup",
      selected = cupties |>
        filter(comp == input$comp_cup) |>
        slice_max(date, with_ties = F) |>
        pull(round)
    )
  })

  observeEvent(input$goto_league, {
    updateTabItems(session, "sidebar", "league")
  })

  observeEvent(input$goto_team, {
    req(input$goto_team)
    updateTabItems(session, "sidebar", "teams")
    updatePickerInput(session, "team", selected = input$goto_team$team)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
