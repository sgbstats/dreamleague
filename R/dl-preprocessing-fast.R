library(tidyverse)
library(readxl)
library(RCurl)
library(XML)
library(googlesheets4)
library(fuzzyjoin)
library(crayon)
library(furrr)
library(future)
library(rvest)
library(httr)

scraplinks2 <- function(url) {
  headers = c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9",
    `Connection` = "keep-alive"
  )

  response = GET(url, add_headers(.headers = headers), timeout(15))

  html <- content(response, as = "text", encoding = "UTF-8") |> read_html()
  tables <- html |> html_elements("table")

  x = tables[[2]] |> html_table()

  x <- x[-1:-2, ]
  names(x) <- c(
    "Competition",
    "date",
    "foo",
    "Home",
    "score",
    "Away",
    "W",
    "D",
    "L",
    "foo2"
  )

  lg = paste(comps, collapse = "|")

  x2 = x |>
    as.data.frame() |>
    mutate(rn = row_number()) |>
    mutate(comp = str_extract(Competition, lg), .by = "rn") |>
    dplyr::select(-Competition, -contains("foo")) |>
    mutate(score = gsub(" ", "", score)) |>
    filter(grepl("-", score)) |>
    mutate(
      Home = str_trim(str_extract(Home, "^([^0-9]+)")),
      Away = str_trim(str_extract(Away, "^([^0-9]+)")),
      date = substr(date, 14, 24) |>
        as.Date()
    ) |>
    separate(score, into = c("H", "A"), sep = "-") |>
    drop_na(date) |>
    filter(!grepl("P", H)) |>
    mutate(
      status = paste0(W, D, L),
      H = as.numeric(str_trim(H)),
      A = as.numeric(str_trim(A))
    )

  return(x2)
}

#use cuttime to allow roll back of goals to check for mistaches
dl_process = function(dl, managers, league, cut_time = Sys.Date()) {
  # tictoc::tic()
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

  if (league == "Didsbury") {
    teams = dl |>
      rename(
        position = 1,
        player = 2,
        club = 3,
        cost = 4,
        goals = 5,
        sold = 7,
        bought = 6
      ) |>
      mutate(
        team = NA_character_,
        cost = gsub("TRANSFER|TRASNFER", "", cost, ignore.case = T)
      )

    managers = managers |>
      rbind.data.frame(tribble(
        ~"manager"   , ~"team"   ,
        "WILL SMITH" , "I ROBOT"
      ))
  } else if (league == "Original") {
    teams = dl |>
      rename(
        position = 1,
        player = 2,
        club = 3,
        cost = 4,
        goals = 5,
        sold = 6,
        bought = 7
      ) |>
      mutate(
        team = NA_character_,
        cost = gsub("transfer", "", cost, ignore.case = T)
      )
  }

  teams2 = teams
  for (i in 1:nrow(teams)) {
    if (teams$player[i] %in% managers$team) {
      team = teams$player[i]
    }
    if (
      teams$position[i] %in%
        c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")
    ) {
      teams2$team[i] = team
    }
  }

  teams2 <- teams2 |> filter(team != "I ROBOT")
  managers = managers |> filter(team != "I ROBOT")

  # sb_id=read.csv("data/sb_id.csv")
  load("data/ids.RDa")

  teams3a = teams2 |>
    filter(
      position %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")
    ) |>
    mutate(
      goals = if_else(
        position == "GOALKEEPER",
        -abs(as.numeric(goals)),
        as.numeric(goals)
      ),
      club = case_when(
        club == "OXFORD UTD" ~ "OXFORD",
        club == "MILTON KEYNES" ~ "MILTON KEYNES DONS",
        T ~ club
      ),
      player = case_when(
        player == "DAN JAMES" ~ "DANIEL JAMES",
        player == "MANNY MONTHE" ~ "EMMANUEL MONTHE",
        player == "STRAND LARSEN" ~ "JORGEN STRAND LARSEN",
        player == "ELI KROUPI" ~ "JUNIOR KROUPI",
        T ~ player
      )
    )

  if (league == "Didsbury") {
    teams3 = teams3a |>
      mutate(
        bought = format(as.Date(bought), "%d-%b"),
        sold = format(as.Date(sold), "%d-%b")
      )
  } else if (league == "Original") {
    for (i in 1:(nrow(teams3a) - 1)) {
      teams3a$sold[i] = teams3a$bought[i + 1]
    }
    teams3 = teams3a |>
      mutate(
        bought = format(openxlsx::convertToDate(bought) - 1, "%d-%b"),
        sold = format(openxlsx::convertToDate(sold) - 1, "%d-%b")
      )
  }

  # merge(team_id |> mutate(team=str_to_upper(team)), by.x = "club", by.y="team", all.x = T)
  player_id2 = player_id |>
    select(player, player_id, team) |>
    filter(!player_id %in% c(65492, 199362, 209366, 197134))

  outfield0 = teams3 |>
    filter(position %in% c("DEFENDER", "MIDFIELDER", "FORWARD")) |>
    fuzzyjoin::stringdist_join(
      player_id2,
      by = "player",
      mode = "left",
      method = "jw",
      distance_col = "dist"
    ) |>
    slice_min(order_by = dist, n = 1, by = player.x) |>
    rename(team = team.x) |>
    select(-team.y) |>
    mutate(
      bought2 = as.Date(
        case_when(
          grepl("Jul|Aug|Sep|Oct|Nov|Dec", bought) ~ paste(
            bought,
            "-2025",
            sep = ""
          ),
          grepl("Jan|Feb|Mar|Apr|May|Jun", bought) ~ paste(
            bought,
            "-2026",
            sep = ""
          ),
          T ~ "01-Jul-2025"
        ),
        "%d-%b-%Y"
      ),
      sold2 = as.Date(
        case_when(
          grepl("Jul|Aug|Sep|Oct|Nov|Dec", sold) ~ paste(
            sold,
            "-2025",
            sep = ""
          ),
          grepl("Jan|Feb|Mar|Apr|May|Jun", sold) ~ paste(
            sold,
            "-2026",
            sep = ""
          ),
          T ~ "30-Jun-2026"
        ),
        "%d-%b-%Y"
      )
    )

  mismatch = outfield0 |>
    filter(dist != 0 | is.na(dist)) |>
    select(team, player.x, club, player.y, dist, player_id)

  duplicates = outfield0 |> count(player.y) |> filter(n > 1)

  outfield = outfield0 |>
    group_by(player_id, team, bought2) |>
    slice_min(team, n = 1, with_ties = F) |>
    ungroup() |>
    mutate(SBgoals = 0, SBapp = 0) |>
    select(-player.y, -dist) |>
    rename("player" = "player.x") |>
    arrange(player) |>
    drop_na(player)

  plan(multisession)

  # Process outfield players in parallel
  outfield_results <- future_map(
    1:nrow(outfield),
    function(i) {
      # Short-circuit if player_id is NA
      if (is.na(outfield$player_id[i])) {
        return(NULL)
      }

      url <- glue::glue(
        "https://www.soccerbase.com/players/player.sd?player_id={outfield$player_id[i]}&season_id=158"
      )

      cat(paste0(outfield$player[i], "\n"))

      tryCatch(
        {
          link <- RCurl::getURL(url)
          tables <- readHTMLTable(link)
          appgoals <- tables$tpg |>
            filter(V1 %in% comps) |>
            mutate(Date = as.Date(substr(V2, 4, 13), "%d%b %Y")) |>
            filter(
              Date > outfield$bought2[i],
              Date <= outfield$sold2[i],
              Date <= as.Date(cut_time)
            ) |>
            mutate(
              Goals = as.numeric(V7),
              App = 1,
              Goals = if_else(is.na(Goals), 0, Goals),
              player_id = outfield$player_id[i],
              team = outfield$team[i]
            )

          appgoals2 <- appgoals |>
            summarise(
              App = sum(App, na.rm = TRUE),
              Goals = sum(Goals, na.rm = TRUE)
            )

          sb_goals <- if (outfield$player_id[i] == 134733) {
            0
          } else {
            appgoals2[[1, "Goals"]]
          }
          sb_app <- appgoals2[[1, "App"]]

          cat(blue(paste0(sb_goals, "\n")))

          return(list(
            SBgoals = sb_goals,
            SBapp = sb_app,
            weekly = appgoals |> select(player_id, Date, Goals, App, team)
          ))
        },
        error = function(e) {
          cat(red("Error\n"))
          return(NULL)
        }
      )
    },
    .progress = TRUE
  )

  # Update outfield and weekly with results from parallel processing
  outfield$SBgoals <- map_dbl(outfield_results, ~ .x$SBgoals %||% 0)
  outfield$SBapp <- map_dbl(outfield_results, ~ .x$SBapp %||% 0)
  weekly <- map_df(outfield_results, "weekly")

  outfield = outfield |>
    mutate(SBgoals = if_else(player == "NICOLAS JACKSON", 0, SBgoals))

  test = outfield |>
    filter(goals != SBgoals, !is.na(player_id)) |>
    select(position, player, team, goals, SBgoals, player_id) |>
    rename("BCgoals" = "goals") |>
    mutate(
      url = glue::glue(
        "https://www.soccerbase.com/players/player.sd?player_id={player_id}&season_id=158"
      )
    )
  # load("Data/team_id.RDa")

  gk = teams3 |>
    filter(!position %in% c("DEFENDER", "MIDFIELDER", "FORWARD")) |>
    merge(
      team_id |> #rename(team_id=id) |>
        dplyr::select(team, team_id) |>
        rbind.data.frame(data.frame(
          team = c("WEST BROM"),
          team_id = c(2744)
        )) |>
        mutate(
          team = case_when(
            team == "WEST BROMWICH ALBION" ~ "WEST BROMWICH",
            T ~ team
          )
        ) |>
        group_by(team) |>
        slice_min(team_id, with_ties = F) |>
        ungroup() |>
        mutate(team = str_to_upper(team)),
      by.x = "club",
      by.y = "team",
      all.x = T
    ) |>
    mutate(SBgoals = 0, SBapp = 0) |>
    ungroup() |>
    drop_na(club) |>
    mutate(
      bought2 = as.Date(
        case_when(
          grepl("Jul|Aug|Sep|Oct|Nov|Dec", bought) ~ paste(
            bought,
            "-2025",
            sep = ""
          ),
          grepl("Jan|Feb|Mar|Apr|May|Jun", bought) ~ paste(
            bought,
            "-2026",
            sep = ""
          ),
          T ~ "01-Jul-2025"
        ),
        "%d-%b-%Y"
      ),
      sold2 = as.Date(
        case_when(
          grepl("Jul|Aug|Sep|Oct|Nov|Dec", sold) ~ paste(
            sold,
            "-2025",
            sep = ""
          ),
          grepl("Jan|Feb|Mar|Apr|May|Jun", sold) ~ paste(
            sold,
            "-2026",
            sep = ""
          ),
          T ~ "30-Jun-2026"
        ),
        "%d-%b-%Y"
      )
    )

  # Process goalkeepers in parallel
  gk_results <- future_map(
    1:nrow(gk),
    function(i) {
      if (is.na(gk$team_id[i])) {
        return(NULL)
      }

      url <- glue::glue(
        "https://www.soccerbase.com/teams/team.sd?team_id={gk$team_id[i]}&teamTabs=results&season_id=158"
      )

      cat(paste0(gk$club[i], "\n"))

      tryCatch(
        {
          sl <- scraplinks2(url)
          x <- sl |>
            mutate(rn = row_number()) |>
            mutate(
              concede = case_when(
                status == "W" ~ -min(H, A),
                status == "D" ~ -H,
                status == "D*" & str_to_upper(Away) == gk$club[i] ~ -H,
                status == "D*" ~ -A,
                status == "L" ~ -max(H, A)
              ),
              App = 1,
              .by = "rn"
            ) |>
            filter(comp %in% comps) |>
            filter(
              date > gk$bought2[i],
              date <= gk$sold2[i],
              date <= as.Date(cut_time),
              !is.na(H)
            )

          x2 <- x |>
            summarise(
              Goals = sum(concede, na.rm = TRUE),
              App = sum(App, na.rm = TRUE)
            )

          sb_goals <- x2[[1, "Goals"]]
          sb_app <- x2[[1, "App"]]

          cat(blue(paste0(sb_goals, "\n")))

          return(list(
            SBgoals = sb_goals,
            SBapp = sb_app,
            weekly_gk = x |>
              rename(Goals = concede, Date = date) |>
              select(Date, Goals, App) |>
              mutate(team_id = gk$team_id[i])
          ))
        },
        error = function(e) {
          cat(red("Error fetching data for ", gk$club[i], "\n"))
          return(NULL)
        },
        warning = function(w) {
          cat(red("Warning for ", gk$club[i], "\n"))
          return(NULL)
        }
      )
    },
    .progress = TRUE
  )

  # Update gk and weekly_gk with results
  gk$SBgoals <- map_dbl(gk_results, ~ .x$SBgoals %||% NA_real_)
  gk$SBapp <- map_dbl(gk_results, ~ .x$SBapp %||% NA_real_)
  weekly_gk <- map_df(gk_results, "weekly_gk")
  gk$SBgoals = as.numeric(gk$SBgoals)

  testgk = gk |>
    filter(goals != SBgoals, !is.na(team_id)) |>
    select(position, club, team, goals, SBgoals, team_id) |>
    rename("BCgoals" = "goals") |>
    mutate(
      url = glue::glue(
        "https://www.soccerbase.com/teams/team.sd?team_id={team_id}&teamTabs=results&season_id=158"
      )
    )

  team_score = rbind(
    outfield |> ungroup() |> dplyr::select(-player_id),
    gk |>
      dplyr::select(-team_id) |>
      mutate(SBgoals = as.numeric(SBgoals), SBapp = as.numeric(SBapp))
  ) |>
    ungroup() |>
    mutate(
      position = factor(
        position,
        levels = c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"),
        ordered = T
      )
    ) |>
    mutate(cost2 = as.numeric(cost)) |>
    arrange(team, position, -cost2, bought2) |>
    select(-cost2)

  # gs4_deauth()

  weekly2 = weekly |>
    merge(outfield |> select(-SBgoals, -SBapp), by = c("player_id", "team"))
  weekly_gk2 = weekly_gk |>
    merge(gk |> select(-SBgoals, -SBapp), by = "team_id")

  #seq.Date(as.Date("2023-07-26"), by=7, length.out = 52)

  team_score_daily = rbind.data.frame(
    weekly2 |> select(-player_id),
    weekly_gk2 |> select(-team_id)
  ) |>
    ungroup() |>
    mutate(
      position = factor(
        position,
        levels = c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"),
        ordered = T
      )
    ) |>
    mutate(cost2 = as.numeric(cost)) |>
    rename(SBgoals = Goals) |>
    arrange(team, position, -cost2, bought2) |>
    ungroup()

  # tictoc::toc()
  structure(
    list(
      "scores" = team_score,
      "daily" = team_score_daily,
      "mismatch" = mismatch,
      "goals_for_mismatch" = test,
      "goals_ag_mismatch" = testgk,
      "cut_time" = cut_time
    ),
    class = "DL"
  )
}
