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
load("managers.RDa")
load("teams.RDa")
credentials_path <- Sys.getenv(
  "DREAMLEAGUE_GOOGLE_CREDENTIALS",
  "credentials.json"
)
shared_drive_target <- Sys.getenv(
  "DREAMLEAGUE_SHARED_DRIVE_TARGET",
  ""
)

try_drive_auth <- function(path = credentials_path) {
  if (!file.exists(path)) {
    message(
      "Drive auth unavailable; credentials file not found. Using bundled cache files only."
    )
    return(invisible(NULL))
  }

  tryCatch(
    googledrive::drive_auth(path = path),
    error = function(e) {
      message(
        "Drive auth unavailable; using bundled cache files only. ",
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
        "Shared Drive target could not be resolved; using bundled cache files only. ",
        conditionMessage(e)
      )
      NULL
    }
  )
}

try_drive_auth()
shared_drive_path <- resolve_shared_drive_path()

cache_dir <- "cache"
file_data <- file.path(cache_dir, "data.RDa")
file_daily <- file.path(cache_dir, "daily.RDa")
file_cupties <- file.path(cache_dir, "cupties.RDa")
cache_meta_file <- file.path(cache_dir, "drive_cache_meta.rds")

cache_meta_default <- list(
  last_check_time = as.POSIXct(NA),
  last_check_success = NA,
  last_check_error = NA_character_,
  last_refresh_time = as.POSIXct(NA),
  last_refresh_forced = FALSE,
  last_source = "Local cache"
)

load_cache_meta <- function(path = cache_meta_file) {
  if (!file.exists(path)) {
    return(cache_meta_default)
  }

  tryCatch(
    modifyList(cache_meta_default, readRDS(path)),
    error = function(e) cache_meta_default
  )
}

save_cache_meta <- function(meta = cache_meta, path = cache_meta_file) {
  saveRDS(meta, path)
  invisible(meta)
}

cache_meta <- load_cache_meta()
cache_pull_source <- cache_meta$last_source
cache_last_updated <- as.POSIXct(NA)

bootstrap_cache <- function() {
  if (
    all(file.exists(c(
      file_data,
      file_daily,
      file_cupties
    )))
  ) {
    return(invisible(TRUE))
  }

  load_first_existing <- function(paths) {
    for (path in paths) {
      if (file.exists(path)) {
        load(path, envir = parent.frame())
        return(invisible(path))
      }
    }
    stop("Unable to locate bundled cache file: ", paste(paths, collapse = ", "))
  }

  load_first_existing(c("data.RDa", file.path("dreamleague", "data.RDa")))
  save(dl, daily, time, cupties, file = file_data)
  save(daily, file = file_daily)
  save(cupties, file = file_cupties)

  invisible(TRUE)
}

bootstrap_cache()

load_bundle_from_cache <- function() {
  load(file_data)
  load(file_daily)
  load(file_cupties)

  assign("dl", dl, envir = .GlobalEnv)
  assign("daily", daily, envir = .GlobalEnv)
  assign("time", time, envir = .GlobalEnv)
  assign("cupties", cupties, envir = .GlobalEnv)

  managers <- rbind.data.frame(
    managers_d |> mutate(league = "didsbury"),
    managers_o |> mutate(league = "original")
  )

  league <- managers |>
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

  teamslist <- (managers |> arrange(team))$team
  names(teamslist) <- paste(
    (league |> arrange(team))$team,
    " (",
    (league |> arrange(team))$manager,
    ")",
    sep = ""
  )
  teamslist_cup <- (managers |> arrange(team))$team
  names(teamslist_cup) <- paste(
    (managers |> arrange(team))$team,
    " (",
    (managers |> arrange(team))$manager,
    ")",
    sep = ""
  )

  file_updates <<- list(
    daily = file.info(file_daily)$mtime,
    cupties = file.info(file_cupties)$mtime
  )

  cache_last_updated <<- max(
    file.info(c(file_data, file_daily, file_cupties))$mtime,
    na.rm = TRUE
  )

  invisible(list(
    managers = managers,
    league = league,
    teamslist = teamslist,
    teamslist_cup = teamslist_cup,
    rounds = unique(cupties$round)
  ))
}

get_remote_listing <- function() {
  tryCatch(
    if (is.null(shared_drive_path)) {
      googledrive::drive_find(pattern = "\\.RDa$") |>
        googledrive::drive_reveal("modified_time")
    } else {
      googledrive::drive_ls(shared_drive_path) |>
        dplyr::filter(grepl("\\.RDa$", .data$name)) |>
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

pull_drive_file <- function(remote_listing, remote_name, local_path) {
  remote <- remote_listing |>
    dplyr::filter(.data$name == remote_name) |>
    dplyr::slice_max(modified_time, n = 1, with_ties = FALSE)

  if (nrow(remote) == 0) {
    return(FALSE)
  }

  local_info <- file.info(local_path)
  remote_time <- remote$modified_time[[1]]
  local_time <- local_info$mtime[[1]]

  if (!is.na(local_time) && remote_time <= local_time) {
    return(FALSE)
  }

  googledrive::drive_download(remote, path = local_path, overwrite = TRUE)
  TRUE
}

refresh_drive_cache <- function(force = FALSE) {
  previous_check_time <- cache_meta$last_check_time
  recent_check <- !force &&
    !is.na(previous_check_time) &&
    difftime(Sys.time(), previous_check_time, units = "hours") < 1

  cache_meta$last_check_time <<- Sys.time()
  cache_meta$last_check_error <<- NA_character_
  cache_meta$last_check_success <<- NA

  if (recent_check) {
    return(invisible(list(status = "skipped", source = cache_meta$last_source)))
  }

  listing <- get_remote_listing()
  if (inherits(listing, "drive_listing_error")) {
    cache_meta$last_check_success <<- FALSE
    cache_meta$last_check_error <<- listing$error
    save_cache_meta(cache_meta)
    return(invisible(list(status = "failed", error = listing$error)))
  }

  if (is.null(listing) || nrow(listing) == 0) {
    cache_meta$last_check_success <<- FALSE
    cache_meta$last_check_error <<- "No remote cache files found."
    save_cache_meta(cache_meta)
    return(invisible(list(
      status = "failed",
      error = cache_meta$last_check_error
    )))
  }

  pulled_any <- FALSE
  pulled_any <- pull_drive_file(listing, "data.RDa", file_data) || pulled_any
  pulled_any <- pull_drive_file(listing, "daily.RDa", file_daily) || pulled_any
  pulled_any <- pull_drive_file(listing, "cupties.RDa", file_cupties) ||
    pulled_any

  cache_meta$last_check_success <<- TRUE
  cache_meta$last_refresh_time <<- if (pulled_any) {
    Sys.time()
  } else {
    cache_meta$last_refresh_time
  }
  cache_meta$last_refresh_forced <<- force
  cache_meta$last_source <<- if (pulled_any) "Google remote" else "Local cache"
  cache_pull_source <<- cache_meta$last_source
  cache_last_updated <<- if (pulled_any) Sys.time() else cache_last_updated
  save_cache_meta(cache_meta)

  if (pulled_any) {
    load_bundle_from_cache()
  }

  invisible(list(
    status = if (pulled_any) "updated" else "current",
    source = cache_meta$last_source
  ))
}

bundle <- load_bundle_from_cache()
managers <- bundle$managers
league <- bundle$league
teamslist <- bundle$teamslist
teamslist_cup <- bundle$teamslist_cup
rounds <- bundle$rounds

refresh_drive_cache(force = FALSE)

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
              slice_max(date, with_ties = FALSE) |>
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
  cache_status_text <- reactiveVal("Local cache")
  cache_warning_text <- reactiveVal(NULL)

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
        "Last remote check failed at {format(cache_meta$last_check_time, '%Y-%m-%d %H:%M:%S')}: {cache_meta$last_check_error}"
      ))
    } else if (!is.null(result)) {
      cache_warning_text(NULL)
    }
    bump_refresh_counter()
  }

  initial_refresh <- refresh_drive_cache(force = FALSE)
  update_cache_state(initial_refresh)

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
    text1 <- paste(
      "<b>League position:",
      league$rank[which(league$team == input$team)],
      "</b>"
    )
    text2 <- paste(
      "<b>Score:",
      league$total[which(league$team == input$team)],
      "</b>"
    )
    text3 <- paste(
      "<font color=\"#4DAF4A\">For:",
      league$gf[which(league$team == input$team)],
      "</font>"
    )
    text4 <- paste(
      "<font color=\"#E41A1C\">Against:",
      league$ga[which(league$team == input$team)],
      "</font>"
    )
    outfield <- paste(
      "Outfield transfers remaining:",
      8 -
        dl |>
          filter(team == input$team, position != "GOALKEEPER", cost == "") |>
          nrow()
    )
    goalie <- paste(
      "Goalkeeper transfers remaining:",
      2 -
        dl |>
          filter(team == input$team, position == "GOALKEEPER", cost == "") |>
          nrow()
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
    source_label <- if (is.na(cache_meta$last_check_time)) {
      "Unavailable"
    } else if (identical(cache_meta$last_source, "Google remote")) {
      "Google remote"
    } else {
      "Local cache"
    }

    cache_time <- if (is.na(cache_meta$last_check_time)) {
      "Unavailable"
    } else {
      format(cache_meta$last_check_time, "%Y-%m-%d %H:%M:%S")
    }

    refresh_time <- if (is.na(cache_meta$last_refresh_time)) {
      "Unavailable"
    } else {
      format(cache_meta$last_refresh_time, "%Y-%m-%d %H:%M:%S")
    }

    tags$div(
      class = "alert alert-info",
      style = "margin:0; padding:8px 12px;",
      HTML(glue::glue(
        "<b>Cache status</b><br/>Last remote check: {cache_time}<br/>Last refresh: {refresh_time}<br/>Source: {source_label}"
      ))
    )
  })

  output$diagnostics_cache_warning <- renderUI({
    refresh_counter()
    if (
      isTRUE(cache_meta$last_check_success) || is.na(cache_meta$last_check_time)
    ) {
      return(NULL)
    }

    age_hours <- as.numeric(difftime(
      Sys.time(),
      cache_meta$last_check_time,
      units = "hours"
    ))
    if (is.na(age_hours) || age_hours > 24) {
      return(NULL)
    }

    tags$div(
      class = "alert alert-warning",
      style = "margin:8px 0 0 0; padding:8px 12px;",
      HTML(glue::glue(
        "Last remote check failed within the past 24 hours.<br/>{cache_meta$last_check_error}"
      ))
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
    last_mod <- file_updates$teams

    tags$div(
      class = "alert alert-warning alert-dismissible",
      role = "alert",
      style = "margin:0; padding:8px 12px;",
      HTML(glue::glue(
        "This table was last updated on {format(last_mod, '%Y-%m-%d %H:%M:%S')}; transfers since then will not be reflected here."
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

    # pick the most recent round as default (if any)
    selected_round <- if (length(rounds_for_comp) > 0) {
      rounds_for_comp[length(rounds_for_comp)]
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
