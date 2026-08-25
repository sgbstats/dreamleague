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
library(glue)

options(gargle_oauth_cache = ".secrets", gargle_oauth_email = TRUE)

app_dir <- if (file.exists(file.path("dreamleague", "app.R"))) {
  "dreamleague"
} else {
  "."
}
resolve_app_path <- function(...) file.path(app_dir, ...)
app_env <- environment()

load(resolve_app_path("managers.RDa"))
credentials_path <- Sys.getenv(
  "DREAMLEAGUE_GOOGLE_CREDENTIALS",
  resolve_app_path("credentials.json")
)
shared_drive_target <- Sys.getenv(
  "DREAMLEAGUE_SHARED_DRIVE_TARGET",
  ""
)
file_data <- resolve_app_path("data.RDa")

normalize_daily_schema <- function(daily) {
  if ("App" %in% names(daily) && !"SBapp" %in% names(daily)) {
    daily$SBapp <- daily$App
  }
  if ("SBapp" %in% names(daily) && !"App" %in% names(daily)) {
    daily$App <- daily$SBapp
  }
  daily
}

build_runtime_objects <- function(source_mtime = as.POSIXct(NA)) {
  daily <- normalize_daily_schema(get(
    "daily",
    envir = app_env,
    inherits = FALSE
  ))
  assign("daily", daily, envir = app_env)

  managers <- rbind.data.frame(
    managers_d |> mutate(league = "didsbury"),
    managers_o |> mutate(league = "original")
  )
  assign("managers", managers, envir = app_env)

  league <- managers |>
    merge(
      dl |> group_by(team) |> summarise(total = sum(SBgoals, na.rm = TRUE)),
      by = "team",
      all = TRUE
    ) |>
    merge(
      dl |>
        filter(position != "GOALKEEPER") |>
        group_by(team) |>
        summarise(gf = sum(SBgoals, na.rm = TRUE)),
      by = "team",
      all = TRUE
    ) |>
    merge(
      dl |>
        filter(position == "GOALKEEPER") |>
        group_by(team) |>
        summarise(ga = -sum(SBgoals, na.rm = TRUE)),
      by = "team",
      all = TRUE
    ) |>
    arrange(-total, -gf) |>
    mutate(rank = row_number(), .by = "league")

  teamslist <- managers |>
    arrange(team) |>
    pull(team)
  names(teamslist) <- paste(
    (league |> arrange(team) |> pull(team)),
    " (",
    (league |> arrange(team) |> pull(manager)),
    ")",
    sep = ""
  )
  assign("teamslist", teamslist, envir = app_env)

  assign("league", league, envir = app_env)
  assign("rounds", unique(cupties$round), envir = app_env)
  assign(
    "file_updates",
    list(
      teams = source_mtime,
      daily = source_mtime
    ),
    envir = app_env
  )

  invisible(TRUE)
}

load_local_bundle <- function(path = file_data) {
  if (!file.exists(path)) {
    stop(
      "Local fallback data.RDa not found at ",
      normalizePath(path, winslash = "/", mustWork = FALSE),
      call. = FALSE
    )
  }

  load(path, envir = .GlobalEnv)
  build_runtime_objects(file.info(path)$mtime[[1]])
}

try_drive_auth <- function(path = credentials_path) {
  if (!file.exists(path)) {
    message(
      "Drive auth unavailable; credentials file not found. Using bundled data.RDa only."
    )
    return(invisible(NULL))
  }

  tryCatch(
    googledrive::drive_auth(path = path),
    error = function(e) {
      message(
        "Drive auth unavailable; using bundled data.RDa only. ",
        conditionMessage(e)
      )
      invisible(NULL)
    }
  )
}

resolve_shared_drive_path <- function(target = shared_drive_target) {
  if (!nzchar(target)) {
    return(NULL)
  }

  tryCatch(
    if (grepl("^[A-Za-z0-9_-]{20,}$", target)) {
      googledrive::as_id(target)
    } else {
      googledrive::drive_get(target)
    },
    error = function(e) {
      message(
        "Shared Drive target could not be resolved; using bundled data.RDa only. ",
        conditionMessage(e)
      )
      NULL
    }
  )
}

get_remote_listing <- function() {
  tryCatch(
    if (is.null(shared_drive_path)) {
      googledrive::drive_find(pattern = "^data\\.RDa$") |>
        googledrive::drive_reveal("modified_time")
    } else {
      googledrive::drive_ls(shared_drive_path) |>
        dplyr::filter(.data$name == "data.RDa") |>
        googledrive::drive_reveal("modified_time")
    },
    error = function(e) {
      structure(
        list(error = conditionMessage(e)),
        class = "drive_listing_error"
      )
    }
  )
}

pull_remote_bundle <- function(remote_listing, remote_name = "data.RDa") {
  remote <- remote_listing |>
    dplyr::filter(.data$name == remote_name) |>
    dplyr::slice_max(modified_time, n = 1, with_ties = FALSE)

  if (nrow(remote) == 0) {
    return(NULL)
  }

  temp_path <- tempfile(fileext = ".RDa")
  on.exit(unlink(temp_path), add = TRUE)
  googledrive::drive_download(remote, path = temp_path, overwrite = TRUE)

  remote_env <- new.env(parent = emptyenv())
  load(temp_path, envir = remote_env)

  required_objects <- c("dl", "daily", "time", "cupties")
  missing_objects <- required_objects[
    !vapply(
      required_objects,
      exists,
      logical(1),
      envir = remote_env,
      inherits = FALSE
    )
  ]
  if (length(missing_objects) > 0) {
    stop(
      "Remote data.RDa is missing objects: ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }

  assign("dl", get("dl", envir = remote_env, inherits = FALSE), envir = app_env)
  assign(
    "daily",
    get("daily", envir = remote_env, inherits = FALSE),
    envir = app_env
  )
  assign(
    "time",
    get("time", envir = remote_env, inherits = FALSE),
    envir = app_env
  )
  assign(
    "cupties",
    get("cupties", envir = remote_env, inherits = FALSE),
    envir = app_env
  )
  build_runtime_objects(remote$modified_time[[1]])

  list(modified_time = remote$modified_time[[1]])
}

try_drive_auth()
shared_drive_path <- resolve_shared_drive_path()
refresh_drive_cache <- function(force = FALSE) {
  listing <- get_remote_listing()
  if (inherits(listing, "drive_listing_error")) {
    return(invisible(list(
      status = "failed",
      source = if (file.exists(file_data)) {
        "Bundled data.RDa"
      } else {
        "Unavailable"
      },
      error = listing$error
    )))
  }

  pulled_bundle <- tryCatch(
    pull_remote_bundle(listing),
    error = function(e) {
      structure(list(error = conditionMessage(e)), class = "drive_pull_error")
    }
  )

  if (inherits(pulled_bundle, "drive_pull_error") || is.null(pulled_bundle)) {
    error_message <- if (inherits(pulled_bundle, "drive_pull_error")) {
      pulled_bundle$error
    } else {
      "No remote data.RDa found."
    }

    return(invisible(list(
      status = "failed",
      source = if (file.exists(file_data)) {
        "Bundled data.RDa"
      } else {
        "Unavailable"
      },
      error = error_message
    )))
  }

  file_updates$teams <- pulled_bundle$modified_time
  file_updates$daily <- pulled_bundle$modified_time
  assign("file_updates", file_updates, envir = app_env)

  invisible(list(
    status = "updated",
    source = "Google Drive",
    modified_time = pulled_bundle$modified_time
  ))
}

initial_remote_load <- refresh_drive_cache(force = FALSE)
if (identical(initial_remote_load$status, "failed")) {
  if (file.exists(file_data)) {
    load_local_bundle()
  } else {
    stop(
      paste(
        "Unable to load DreamLeague data from Google Drive, and no local fallback data.RDa is available.",
        initial_remote_load$error
      ),
      call. = FALSE
    )
  }
}

weeks <- seq.Date(as.Date("2026-07-27"), by = 7, length.out = 52)
weeks2 <- weeks[weeks <= Sys.Date()]
weekschar <- format(weeks2, format = "%d-%b")
names(weeks2) <- weekschar
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
            checkboxInput("current", "Current team only", value = TRUE),
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
            uiOutput("player_warning"),
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
            selected = "didsbury"
          ),
          pickerInput(
            "round_cup",
            "Round",
            choices = cupties |>
              filter(comp == "didsbury") |>
              arrange(date) |>
              pull(round) |>
              unique(),
            selected = cupties |>
              filter(comp == "didsbury") |>
              slice_max(date, n = 1, with_ties = FALSE) |>
              pull(round),
            multiple = FALSE
          ),
          uiOutput("round_date2")
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
            dateInput("end", "End date", value = Sys.Date())
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
        sidebarLayout(
          sidebarPanel(
            actionButton(
              "force_drive_refresh",
              "Force refresh from Google Drive"
            ),
            br(),
            br(),
            uiOutput("diagnostics_cache_status"),
            uiOutput("diagnostics_cache_warning")
          ),
          mainPanel(
            dataTableOutput("diagnostics")
          )
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
  league_master <- reactiveVal("didsbury")
  refresh_counter <- reactiveVal(0)
  refresh_counter_value <- 0
  cache_status_text <- reactiveVal("Bundled data.RDa")
  cache_warning_text <- reactiveVal(NULL)

  team_choices_for_league <- function(league_name) {
    managers |>
      filter(league == league_name) |>
      arrange(team) |>
      (function(df) {
        setNames(df$team, paste(df$team, " (", df$manager, ")", sep = ""))
      })()
  }

  observeEvent(
    input$league_teams,
    {
      updatePickerInput(
        session,
        "team",
        choices = team_choices_for_league(input$league_teams),
        selected = NULL
      )
    },
    ignoreInit = FALSE
  )

  bump_refresh_counter <- function() {
    refresh_counter_value <<- refresh_counter_value + 1
    refresh_counter(refresh_counter_value)
  }

  update_cache_state <- function(result = NULL) {
    if (!is.null(result) && !is.null(result$source)) {
      cache_status_text(result$source)
    }
    if (!is.null(result) && identical(result$status, "failed")) {
      cache_warning_text(glue::glue(
        "Last remote load failed: {result$error}"
      ))
    } else if (!is.null(result)) {
      cache_warning_text(NULL)
    }
    bump_refresh_counter()
  }

  update_cache_state(initial_remote_load)

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
    refresh_counter()

    req(input$league_teams, input$team)

    team_data <- dl |>
      filter(league == input$league_teams, team == input$team)

    if (isTRUE(input$current)) {
      team_data <- team_data |>
        filter(is.na(sold) | sold == "")
    }

    teams3 <- if (isTRUE(input$current)) {
      team_data |>
        select(-sold, -bought2, -sold2, -SBapp, -league, -is_transfer)
    } else {
      team_data |>
        select(-bought2, -sold2, -SBapp, -league, -is_transfer)
    }

    table_data_unformatted <- teams3 |>
      select(-goals)

    table_data <- table_data_unformatted |>
      select(-url) |>
      rename("Goals" = "SBgoals") |>
      rename_with(str_to_title) |>
      relocate(Goals, .after = Club) |>
      mutate(
        Team = as.character(Team),
        Player = replace_na(as.character(Player), ""),
        Club = replace_na(as.character(Club), "")
      )

    link_style <- "cursor: pointer; text-decoration: underline; color: #808080;"

    team_columns <- list(
      Team = colDef(show = FALSE),
      Player = colDef(
        width = 150,
        cell = function(value, index) {
          if (table_data_unformatted$position[index] == "GOALKEEPER") {
            ""
          } else {
            url <- table_data_unformatted$url[index]
            if (!is.na(url) && nzchar(url)) {
              tags$a(
                href = url,
                target = "_blank",
                rel = "noreferrer",
                style = link_style,
                value
              )
            } else {
              value
            }
          }
        }
      ),
      Club = colDef(
        width = 150,
        cell = function(value, index) {
          if (table_data_unformatted$position[index] == "GOALKEEPER") {
            url <- table_data_unformatted$url[index]
            if (!is.na(url) && nzchar(url)) {
              tags$a(
                href = url,
                target = "_blank",
                rel = "noreferrer",
                style = link_style,
                value
              )
            } else {
              value
            }
          } else {
            value
          }
        }
      ),
      Position = colDef(width = 100),
      Goals = colDef(width = 70),
      Cost = colDef(width = 70)
    )

    if (isTRUE(input$current)) {
      team_columns$Bought <- colDef(show = FALSE)
    } else {
      team_columns$Bought <- colDef(width = 90)
      team_columns$Sold <- colDef(width = 90)
    }

    reactable(
      table_data,
      sortable = TRUE,
      searchable = TRUE,
      columns = team_columns,
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

  output$team_history_out <- renderReactable({
    period <- daily |>
      filter(Date <= as.Date(input$end), Date >= as.Date(input$start))

    league2 <- managers |>
      merge(
        period |>
          summarise(total = sum(SBgoals), .by = c("team", "league")),
        by = c("team", "league"),
        all = TRUE
      ) |>
      merge(
        period |>
          filter(position != "GOALKEEPER") |>
          summarise(gf = sum(SBgoals), .by = c("team", "league")),
        by = c("team", "league"),
        all = TRUE
      ) |>
      merge(
        period |>
          filter(position == "GOALKEEPER") |>
          summarise(ga = -sum(SBgoals), .by = c("team", "league")),
        by = c("team", "league"),
        all = TRUE
      ) |>
      mutate(across(where(is.numeric), ~ tidyr::replace_na(., 0))) |>
      filter(league == input$league_team_history) |>
      select(-league) |>
      arrange(-total, -gf)

    scorers2 <- period |>
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

    res2 <- league2 |> merge(scorers2, all.x = T)
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

  output$teamtext <- renderUI({
    refresh_counter()
    req(input$league_teams, input$team)

    league_row <- league |>
      filter(league == input$league_teams, team == input$team)

    text1 <- paste(
      "<b>League position:",
      league_row$rank,
      "</b>"
    )
    text2 <- paste(
      "<b>Score:",
      league_row$total,
      "</b>"
    )
    text3 <- paste(
      "<font color=\"#4DAF4A\">For:",
      league_row$gf,
      "</font>"
    )
    text4 <- paste(
      "<font color=\"#E41A1C\">Against:",
      league_row$ga,
      "</font>"
    )
    outfield <- paste(
      "Outfield transfers remaining:",
      pmax(
        0,
        8 -
          (dl |>
            filter(
              league == input$league_teams,
              team == input$team,
              position != "GOALKEEPER",
              is_transfer
            ) |>
            nrow())
      )
    )
    goalie <- paste(
      "Goalkeeper transfers remaining:",
      pmax(
        0,
        2 -
          (dl |>
            filter(
              league == input$league_teams,
              team == input$team,
              position == "GOALKEEPER",
              is_transfer
            ) |>
            nrow())
      )
    )
    HTML(paste(text1, text2, text3, text4, outfield, goalie, sep = "<br/>"))
  })

  output$img <- renderImage(
    {
      outfile <- paste(
        "img/",
        str_to_upper(str_replace_all(input$team, "[^[:alnum:]]", "")),
        ".png",
        sep = ""
      )
      hold <- magick::image_read(outfile)

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
    table_data_unformatted <- dl |>
      filter(is.na(sold), league == input$league_players) |>
      dplyr::select(team, player, club, position, any_of("url"))

    table_data <- table_data_unformatted |>
      mutate(
        player = replace_na(as.character(player), ""),
        club = replace_na(as.character(club), "")
      ) |>
      select(team, player, club, position, url) |>
      rename_with(str_to_title)

    reactable(
      table_data,
      searchable = TRUE,
      columns = list(
        Team = colDef(
          width = 150,
          cell = function(value, index) {
            escaped_team <- gsub("'", "\\\\'", value)
            tags$a(
              href = "#",
              style = "cursor: pointer; text-decoration: underline; color: #808080;",
              onclick = sprintf(
                "Shiny.setInputValue('goto_team', {team: '%s', nonce: Math.random()})",
                escaped_team
              ),
              value
            )
          }
        ),
        Player = colDef(width = 150),
        Club = colDef(width = 150),
        Position = colDef(width = 100),
        Url = colDef(show = FALSE)
      ),
      defaultPageSize = 15
    )
  })

  output$diagnostics <- DT::renderDT({
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

  observeEvent(input$force_drive_refresh, {
    result <- refresh_drive_cache(force = TRUE)
    update_cache_state(result)
  })

  output$diagnostics_cache_status <- renderUI({
    refresh_counter()
    tags$div(
      class = "alert alert-info",
      style = "margin:0; padding:8px 12px;",
      HTML(glue::glue(
        "<b>Remote load status</b><br/>Source: {cache_status_text()}"
      ))
    )
  })

  output$diagnostics_cache_warning <- renderUI({
    refresh_counter()
    warning_text <- cache_warning_text()
    if (is.null(warning_text) || !nzchar(warning_text)) {
      return(NULL)
    }

    tags$div(
      class = "alert alert-warning",
      style = "margin:8px 0 0 0; padding:8px 12px;",
      HTML(warning_text)
    )
  })

  output$update_time <- renderUI({
    refresh_counter()
    HTML(paste0(
      "Last score update: ",
      format(time$update_time, format = "%Y-%m-%d %H:%M:%S"),
      "<br>Last file upload<br>Didsbury: ",
      format(time$mod_d, format = "%Y-%m-%d %H:%M:%S"),
      "<br>Original: ",
      format(time$mod_o, format = "%Y-%m-%d %H:%M:%S")
    ))
  })

  output$player_warning <- renderUI({
    refresh_counter()
    req(input$league_players)
    last_mod <- file_updates$daily
    last_mod_text <- if (is.na(last_mod)) {
      "Unavailable"
    } else {
      format(last_mod, "%Y-%m-%d %H:%M:%S")
    }

    tags$div(
      class = "alert alert-warning alert-dismissible",
      role = "alert",
      style = "margin:0; padding:8px 12px;",
      HTML(glue::glue(
        "This table was last updated on {last_mod_text}; transfers since then will not be reflected here."
      )),
      tags$button(
        type = "button",
        class = "close",
        `data-dismiss` = "alert",
        `aria-label` = "Close",
        tags$span(`aria-hidden` = "true", HTML("&times;"))
      )
    )
  })

  output$cup <- renderReactable({
    date <- cupties |>
      filter(comp == input$comp_cup, round == input$round_cup) |>
      pull(date) |>
      min(na.rm = TRUE)

    weekend <- daily |>
      filter(
        Date >= date,
        Date <= date + lubridate::days(3),
        Date >= bought2,
        Date <= sold2
      )

    scorers <- weekend |>
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

  output$round_date2 <- renderUI({
    rd <- cupties |>
      dplyr::filter(round == input$round_cup, comp == input$comp_cup) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(date)

    req(rd)
    if (month(rd) == month(rd + 3)) {
      HTML(paste0(
        "Round date: ",
        format(rd, format = "%d"),
        "-",
        format(rd + 3, format = "%d %b")
      ))
    } else {
      HTML(paste0(
        "Round date: ",
        format(rd, format = "%d %b"),
        "-",
        format(rd + 3, format = "%d %b")
      ))
    }
  })
  # maintaining pickers across tabs
  observeEvent(input$league, {
    #updateRadioButtons(session, "league", selected = input$league)
    updateRadioButtons(session, "league_teams", selected = input$league)
    updateRadioButtons(session, "league_players", selected = input$league)
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
    teamslist <- (managers |>
      arrange(team) |>
      filter(league == input$league_teams))$team

    names(teamslist) <- paste(
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

  observeEvent(input$comp_cup, {
    rounds_for_comp <- cupties |>
      filter(comp == input$comp_cup) |>
      arrange(date) |>
      pull(round) |>
      unique()

    selected_round <- if (length(rounds_for_comp) > 0) {
      tail(rounds_for_comp, 1)
    } else {
      NULL
    }

    updatePickerInput(
      session,
      "round_cup",
      choices = rounds_for_comp,
      selected = selected_round
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
