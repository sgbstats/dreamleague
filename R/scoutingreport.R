library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
library(googlesheets4)
library(fuzzyjoin)
`%notin%` = Negate(`%in%`)
# setwd("C:/R/git/dreamleague")
tictoc::tic()

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
for (i in 1:nrow(player_id2)) {
  # for(i in 1:100)
  skip_to_next <- FALSE
  # if(is.na(player_id2$id[i])){next}

  url = paste(
    "https://www.soccerbase.com/players/player.sd?player_id=",
    player_id2$player_id[i],
    "&season_id=158",
    sep = ""
  )
  link = RCurl::getURL(url)
  cat(paste(i, player_id$player[i], "\n"))
  tryCatch(
    {
      tables = readHTMLTable(link)
      player_id2$position[i] = stringr::word(tables[[1]], 1)
      # if(player_id2$player_id[i]==75804)
      # {
      #   tables$tpg$V7[5]="1"
      # }
      #
      # if(player_id2$player_id[i]==52657)
      # {
      #   tables$tpg$V7=""
      # }
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

      player_id2$SBgoals[i] = appgoals2[1, 2]

      player_id2$SBapp[i] = appgoals2[1, 1]

      weeklyreport = rbind(
        weeklyreport,
        appgoals |> select(player_id, Date, Goals, App, team)
      )

      cat(paste(player_id2$SBgoals[i], "\n"))
    },
    error = function(e) {
      skip_to_next <<- TRUE
      warning("Error")
    }
  )

  if (skip_to_next) {
    next
  }
}

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

source("R/dl-preprocessing.R")
team_id$SBgoals = NA_integer_
team_id$SBapp = NA_integer_
for (i in 1:nrow(team_id)) {
  skip_to_next <- FALSE
  # if(is.na(outfield$id[i])){next}

  url = paste(
    "https://www.soccerbase.com/teams/team.sd?team_id=",
    team_id$team_id[i],
    "&teamTabs=results&season_id=158",
    sep = ""
  )

  cat(paste0(team_id$team[i], "\n"))
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
      team_id$SBgoals[i] = x2[1, 1]
      team_id$SBapp[i] = x2[1, 2]

      cat(blue(paste0(x2[1, 1], "\n")))
    },
    error = function(e) {
      skip_to_next <<- TRUE
      cat(red("Error\n"))
    }
  )

  if (skip_to_next) {
    next
  }
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
