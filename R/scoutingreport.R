library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
library(googlesheets4)
library(fuzzyjoin)
library(furrr)
library(future)
library(crayon)


# setwd("C:/R/git/dreamleague")

load("data/ids.RDa")

player_id2 = player_id |>
  mutate(position = "", SBgoals = 0, SBapp = 0)
weeklyreport = tribble(
  ~"player_id" , ~"Date" , ~"Goals" , ~"App" , ~"team"
)
comps <<- c(
  "English premier",
  "English Premier",
  "Premier League",
  "EFL Cup",
  "English League Cup",
  "Europa League",
  "Community Shield",
  "Champions League",
  "European Cup",
  "FA Cup",
  "English FA Cup",
  "Europa Conference League",
  "Football League Championship",
  "Football League Championship Play-Off",
  "Championship Play-Off",
  "Football League One",
  "Football League One Play-Off",
  "League One Play-Off",
  "Football League Two",
  "Football League Two Play-Off",
  "League Two Play-Off",
  "Fairs Cup",
  "UEFA Cup",
  "Euro Cup Winners Cup",
  "European Super Cup",
  "Capital One Cup",
  "Carling Cup",
  "Premiership",
  "English Div 1 \\(old\\)",
  "Charity Shield"
)
plan(multisession, workers = availableCores() / 2)

results <- furrr::future_map(1:nrow(player_id2), .f = function(i) {
  url = glue::glue(
    "https://www.soccerbase.com/players/player.sd?player_id={player_id2$player_id[i]}&season_id=158"
  )
  link = RCurl::getURL(url)
  cat(paste(i, player_id$player[i], "\n"))

  position <- NA
  SBgoals <- NA
  SBapp <- NA
  weekly_report_rows <- NULL

  tryCatch(
    {
      tables = readHTMLTable(link)
      position = stringr::word(tables[[1]], 1)

      appgoals = (tables$tpg) |>
        filter(V1 %in% comps) |>
        mutate(Date = as.Date(substr(V2, 4, 13), "%d%b %Y")) |>
        mutate(
          Goals = as.numeric(V7),
          App = 1,
          Goals = if_else(is.na(Goals), 0, Goals),
          player_id = player_id2$player_id[i],
          team = player_id2$team[i]
        )

      appgoals2 = appgoals |>
        summarise(App = sum(App, na.rm = T), Goals = sum(Goals, na.rm = T))

      SBgoals = appgoals2[1, 2]
      SBapp = appgoals2[1, 1]

      weekly_report_rows = appgoals |>
        select(player_id, Date, Goals, App, team)

      cat(paste(SBgoals, "\n"))
    },
    error = function(e) {
      warning("Error")
    }
  )

  list(
    player_update = data.frame(
      i = i,
      position = position,
      SBgoals = SBgoals,
      SBapp = SBapp
    ),
    weekly_report_rows = weekly_report_rows
  )
})

player_updates <- dplyr::bind_rows(lapply(results, `[[`, "player_update"))
weekly_report_rows <- dplyr::bind_rows(lapply(
  results,
  `[[`,
  "weekly_report_rows"
))

if (nrow(player_updates) > 0) {
  player_id2$position[player_updates$i] <- player_updates$position
  player_id2$SBgoals[player_updates$i] <- player_updates$SBgoals
  player_id2$SBapp[player_updates$i] <- player_updates$SBapp
}

weeklyreport <- weekly_report_rows


load("dreamleague/data.RDa")


weeklyleaguedata = weeklyreport |>
  merge(player_id |> select(player, player_id), by = "player_id") |>
  select(player, player_id, team, Date, Goals, App) |>
  mutate(taken = player %in% (dl |> filter(league == "Didsbury"))$player)

player_id_scout = player_id2 |>
  mutate(taken = player %in% dl$player) |>
  filter(!taken) |>
  arrange(-SBgoals)

tictoc::toc()

save(player_id_scout, file = "data/scouting/scoutingdata.RDa")

source("R/dl-preprocessing-fast.R")
team_id$SBgoals = NA_integer_
team_id$SBapp = NA_integer_
team_updates <- furrr::future_map_dfr(1:nrow(team_id), .f = function(i) {
  url = glue::glue(
    "https://www.soccerbase.com/teams/team.sd?team_id={team_id$team_id[i]}&teamTabs=results&season_id=158"
  )

  cat(paste0(team_id$team[i], "\n"))

  SBgoals <- NA
  SBapp <- NA

  tryCatch(
    {
      sl = scraplinks2(url)
      x = sl |>
        mutate(rn = row_number()) |>
        mutate(
          concede = case_when(
            status == "W" ~ -min(H, A),
            status == "D" ~ -H,
            status == "D*" & str_to_upper(Away) == team_id$team[i] ~ -H,
            status == "D*" ~ -A,
            status == "L" ~ -max(H, A)
          ),
          App = 1,
          .by = "rn"
        ) |>
        filter(comp %in% comps)

      x2 = x |>
        summarise(Goals = sum(concede, na.rm = T), App = sum(App, na.rm = T))

      SBgoals = x2[1, 1]
      SBapp = x2[1, 2]

      cat(blue(paste0(x2[1, 1], "\n")))
    },
    error = function(e) {
      cat(red("Error\n"))
    }
  )

  data.frame(i = i, SBgoals = SBgoals, SBapp = SBapp)
})

if (nrow(team_updates) > 0) {
  team_id$SBgoals[team_updates$i] <- team_updates$SBgoals
  team_id$SBapp[team_updates$i] <- team_updates$SBapp
}


team_id |>
  arrange(-SBgoals) |>
  filter(
    !team %in%
      (dl |>
        filter(
          position == "GOALKEEPER",
          is.na(sold),
          league == "didsbury"
        ))$club
  )
