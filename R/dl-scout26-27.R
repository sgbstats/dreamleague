library(XML)
library(tidyverse)
library(RCurl)
library(readxl)
library(openxlsx)
library(beepr)
library(chron)
library(tibble)
library(chron)
library(googlesheets4)
library(rvest)
library(future.apply)


scraplinks <- function(url) {
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage |>
    rvest::html_nodes("a") |>
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}

team_id <- scraplinks("https://www.soccerbase.com/teams/home.sd") |>
  filter(grepl("comp_id=[1-4]$", url), grepl("team_id", url)) |>
  mutate(
    team_id = as.numeric(stringi::stri_extract_first_regex(url, "[0-9]+"))
  ) |>
  select(-url) |>
  rename("team" = "link") |>
  mutate(
    team = case_when(
      team == "AFC W'bledon" ~ "Wimbledon",
      team == "Man City" ~ "Manchester City",
      team == "Man Utd" ~ "Manchester United",
      team == "Bristol C" ~ "Bristol City",
      team == "Bristol R" ~ "Bristol Rovers",
      team == "MK Dons" ~ "Milton Keynes Dons",
      team == "West Brom" ~ "West Bromwich Albion",
      team == "Sheff Utd" ~ "Sheffield United",
      team == "Sheff Wed" ~ "Sheffield Wednesday",
      team == "Cambridge U" ~ "Cambridge",
      team == "Nottm Forest" ~ "Nottingham Forest",
      team == "Notts Co" ~ "Notts County",
      team == "Newport Co" ~ "Newport County",

      T ~ team
    )
  )

load("data/ids.RDa")
t <- 0
team_id <- team_id |>
  filter(team %notin% c("BARROW", "HARROGATE")) |>
  rbind.data.frame(tribble(
    ~"team"    , ~"team_id" ,
    "YORK"     ,       2910 ,
    "ROCHDALE" ,       2175
  )) |>
  distinct()
t <- 0
player_id0 <- tribble(
  ~player , ~player_id , ~team , ~team_id
)
for (i in 1:nrow(team_id)) {
  skip_to_next <- FALSE

  print(team_id$team[i])
  tryCatch(
    {
      url <- paste(
        "https://www.soccerbase.com/teams/team.sd?team_id=",
        team_id$team_id[i],
        sep = ""
      )
      players0 <- scraplinks(url = url) |>
        mutate(n = row_number())

      #find where the junk starts
      rn <- (players0 |>
        filter(grepl("tourn_id", url, ignore.case = T)) |>
        slice_min(n))$n
      players <- players0 |>
        filter(n < rn, grepl("player_id", url), !is.na(link)) |>
        mutate(
          player_id = as.numeric(stringi::stri_extract_first_regex(
            url,
            "[0-9]+"
          ))
        ) |>
        select(-url) |>
        rename("player" = "link")
    },
    error = function(e) {
      skip_to_next <<- TRUE
    }
  )

  if (skip_to_next) {
    next
  }
  player_id0 <- player_id0 |>
    rbind.data.frame(
      players |> mutate(team = team_id$team[i], team_id = team_id$team_id[i])
    )
}

player_id <- player_id0 |>
  mutate(
    player = case_when(
      player == "Ali Ibrahim Ali Al Hamadi" ~ "Ali Al Hamadi",
      player == "Dan Agyei" ~ "Daniel Agyei",
      player == "Joshua Sargent" ~ "Josh Sargent",
      player == "Mo Eisa" ~ "Mohamed Eisa",
      player == "Iyenoma Destiny Udogie" ~ "Destiny Udogie",
      player == "Gabriel" ~ "Gabriel Magalhaes",
      T ~ player
    )
  ) |>
  mutate(player = str_to_upper(player)) |>
  mutate(team = str_to_upper(team)) |>
  # rbind(tribble(~"player", ~"n", ~"player_id", ~"team", ~"team_id",
  #               "HARRY KANE", NA_integer_, 52657, "BAYERN MUNICH", 469,
  #               "CHUBA AKPOM", NA_integer_, 68532, "AJAX", 80,
  #               "NATHAN TELLA", NA_integer_, 107792, "BAYER LEVERKUSEN", 468,
  #               "OSCAR ESTUPINAN", NA_integer_, 104942, "METZ", 1772)) |>
  group_by(player_id) |>
  slice_min(team, with_ties = F) |>
  filter(player_id != 107014)


gk <- read.xlsx("data/DL-DATA2026.xlsx", sheet = "Goalkeepers")
def <- read.xlsx("data/DL-DATA2026.xlsx", sheet = "Defenders")
mid <- read.xlsx("data/DL-DATA2026.xlsx", sheet = "Midfielders")
forwards <- read.xlsx("data/DL-DATA2026.xlsx", sheet = "Forwards")


data1 <- union_all(def, mid) |> union(forwards) |> janitor::clean_names()
data2 <- data1 |>
  mutate(player = str_to_upper(player)) |>
  mutate(player = stringi::stri_trans_general(player, id = "Latin-ASCII")) |>
  fuzzyjoin::stringdist_join(
    player_id,
    by = "player",
    mode = "left",
    method = "jw",
    distance_col = "dist"
  ) |>
  slice_min(order_by = dist, n = 1, by = "player.x") |>
  mutate(num = row_number(), team = str_to_title(team)) |>
  dplyr::select(
    num,
    pos,
    player.x,
    player.y,
    club,
    team,
    player_id,
    dist,
    total
  ) |>
  mutate(
    team = case_when(
      team == "AFC W'bledon" ~ "Wimbledon",
      team == "Man City" ~ "Manchester City",
      team == "Man Utd" ~ "Manchester United",
      team == "Bristol C" ~ "Bristol City",
      team == "Bristol R" ~ "Bristol Rovers",
      team == "MK Dons" ~ "Milton Keynes",
      team == "West Brom" ~ "West Bromwich",
      team == "Sheff Utd" ~ "Sheffield United",
      team == "Sheff Wed" ~ "Sheffield Wednesday",
      team == "Cambridge U" ~ "Cambridge",
      team == "Nottm Forest" ~ "Nottingham Forest",
      team == "Notts Co" ~ "Notts County",
      team == "Newport County" ~ "Newport",
      team == "Qpr" ~ "QPR",
      T ~ team
    )
  )
data3 <- data2 |>
  mutate(check_needed = dist > 0 & team != club) |>
  merge(
    gk |>
      janitor::clean_names() |>
      dplyr::select(-total, -pos) |>
      mutate(club = str_trim(club)) |>
      rename("team" = "club"),
    by = "team"
  ) |>
  dplyr::select(
    num,
    pos,
    player.x,
    player.y,
    club,
    team,
    league,
    total,
    player_id,
    dist,
    check_needed
  ) |>
  arrange(num)

write.csv(data3, "data/interim.csv", row.names = F, na = "")

data1 |> count(player, club, pos) |> filter(n > 1)


player_id2 <- player_id |>
  mutate(position = "", SBgoals = 0, SBapp = 0)

future::plan(future::multisession)

scrape_player <- function(i) {
  skip_to_next <- FALSE

  url <- paste(
    "https://www.soccerbase.com/players/player.sd?player_id=",
    player_id2$player_id[i],
    "&season_id=158",
    sep = ""
  )
  link <- RCurl::getURL(url)

  message(i, " ", player_id$player[i])

  tryCatch(
    {
      tables <- readHTMLTable(link)
      position <- stringr::word(tables[[1]], 1)
      if (player_id2$player_id[i] == 75804) {
        tables$tpg$V7[5] <- "1"
      }

      if (player_id2$player_id[i] == 52657) {
        tables$tpg$V7 <- ""
      }
      appgoals <- (tables$tpg) |>
        filter(
          V1 %in%
            c(
              "English premier",
              "English Premier",
              "Premier League",
              "EFL Cup",
              "English League Cup",
              "Europa League",
              "Community Shield",
              "Champions League",
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
              "League Two Play-Off"
            )
        ) |>
        mutate(Date = as.Date(substr(V2, 4, 13), "%d%b %Y")) |>
        mutate(
          Goals = as.numeric(V7),
          App = 1,
          Goals = if_else(is.na(Goals), 0, Goals),
          player_id = player_id2$player_id[i],
          team = player_id2$team[i]
        )

      appgoals2 <- appgoals |>
        summarise(App = sum(App, na.rm = T), Goals = sum(Goals, na.rm = T))

      list(
        i = i,
        position = position,
        SBgoals = appgoals2[1, 2],
        SBapp = appgoals2[1, 1],
        weeklyreport = appgoals |> select(player_id, Date, Goals, App, team)
      )
    },
    error = function(e) {
      warning("Error")
      list(
        i = i,
        position = NA_character_,
        SBgoals = NA_real_,
        SBapp = NA_real_,
        weeklyreport = tibble()
      )
    }
  )
}

results <- future.apply::future_lapply(seq_len(nrow(player_id2)), scrape_player)

for (res in results) {
  player_id2$position[res$i] <- res$position
  player_id2$SBgoals[res$i] <- res$SBgoals
  player_id2$SBapp[res$i] <- res$SBapp
}

weeklyreport <- bind_rows(lapply(results, `[[`, "weeklyreport"))


save(player_id2, file = "data/sbdata.RDa")
data1 <- union_all(def, mid) |>
  union(forwards) |>
  janitor::clean_names() |>
  mutate(rn = row_number())

data2 <- player_id2 |>
  filter(position != "Goalkeeper") |>
  fuzzyjoin::stringdist_join(
    data1 |> mutate(player = str_to_upper(player)),
    by = "player",
    mode = "left",
    method = "jw",
    distance_col = "dist"
  ) |>
  ungroup() |>
  slice_min(order_by = dist, n = 1, by = "player.y") |>
  arrange(rn)


data3 <- data2 |>
  filter(SBgoals > 0, position != "Goalkeeper") |>
  mutate(
    position = factor(
      substr(position, 1, 1),
      levels = c("D", "M", "F"),
      ordered = T
    ),
    pos = factor(pos, levels = c("D", "M", "F"), ordered = T)
  ) |>
  arrange(pos, rn, -total) |>
  select(-SBgoals) |>
  mutate(team = str_to_title(team), club = str_to_title(club)) |>
  merge(
    gk |>
      janitor::clean_names() |>
      dplyr::select(-total, -pos) |>
      mutate(club = str_trim(club) |> str_to_title()),
    by = "club",
    all.x = T
  ) |>
  arrange(rn) |>
  mutate(
    check_needed = dist > 0 & team != club,
    pos_mismatch = if_else(!check_needed & position != pos, TRUE, NA),
    new_team = if_else(!check_needed & club != team, team, NA_character_)
  ) |>
  select(
    pos,
    player.y,
    club,
    total,
    league,
    notes,
    new_team,
    SBapp,
    player_id,
    check_needed,
    pos_mismatch
  ) |>
  mutate(check_needed = if_else(check_needed, T, NA))

write.csv(data3, "data/sbdata2627.csv", na = "", row.names = FALSE)
