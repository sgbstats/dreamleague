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
`%notin%` = Negate(`%in%`)

scraplinks <- function(url) {
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}

team_id = scraplinks("https://www.soccerbase.com/teams/home.sd") %>%
  filter(grepl("comp_id=[1-4]$", url), grepl("team_id", url)) %>%
  mutate(
    team_id = as.numeric(stringi::stri_extract_first_regex(url, "[0-9]+"))
  ) %>%
  select(-url) %>%
  rename("team" = "link") %>%
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

t = 0
player_id0 = tribble(
  ~player , ~player_id , ~team , ~team_id
)
for (i in 1:nrow(team_id)) {
  skip_to_next <- FALSE

  print(team_id$team[i])
  tryCatch(
    {
      url = paste(
        "https://www.soccerbase.com/teams/team.sd?team_id=",
        team_id$team_id[i],
        sep = ""
      )
      players0 = scraplinks(url = url) %>%
        mutate(n = row_number())

      #find where the junk starts
      rn = (players0 %>%
        filter(grepl("tourn_id", url, ignore.case = T)) %>%
        slice_min(n))$n
      players = players0 %>%
        filter(n < rn, grepl("player_id", url), !is.na(link)) %>%
        mutate(
          player_id = as.numeric(stringi::stri_extract_first_regex(
            url,
            "[0-9]+"
          ))
        ) %>%
        select(-url) %>%
        rename("player" = "link")
    },
    error = function(e) {
      skip_to_next <<- TRUE
    }
  )

  if (skip_to_next) {
    next
  }
  player_id0 = player_id0 %>%
    rbind.data.frame(
      players %>% mutate(team = team_id$team[i], team_id = team_id$team_id[i])
    )
}

player_id = player_id0 %>%
  mutate(
    player = case_when(
      player == "Ali Ibrahim Ali Al Hamadi" ~ "Ali Al Hamadi",
      player == "Dan Agyei" ~ "Daniel Agyei",
      player == "Joshua Sargent" ~ "Josh Sargent",
      player == "Mo Eisa" ~ "Mohamed Eisa",
      player == "Iyenoma Destiny Udogie" ~ "Destiny Udogie",
      T ~ player
    )
  ) %>%
  mutate(player = str_to_upper(player)) %>%
  mutate(team = str_to_upper(team)) %>%
  # rbind(tribble(~"player", ~"n", ~"player_id", ~"team", ~"team_id",
  #               "HARRY KANE", NA_integer_, 52657, "BAYERN MUNICH", 469,
  #               "CHUBA AKPOM", NA_integer_, 68532, "AJAX", 80,
  #               "NATHAN TELLA", NA_integer_, 107792, "BAYER LEVERKUSEN", 468,
  #               "OSCAR ESTUPINAN", NA_integer_, 104942, "METZ", 1772)) %>%
  group_by(player_id) %>%
  slice_min(team, with_ties = F) %>%
  filter(player_id != 107014)


gk = read.xlsx("data/DL-DATA2024.xlsx", sheet = "Goalkeepers")
def = read.xlsx("data/DL-DATA2024.xlsx", sheet = "Defenders")
mid = read.xlsx("data/DL-DATA2024.xlsx", sheet = "Midfielders")
forwards = read.xlsx("data/DL-DATA2024.xlsx", sheet = "Forwards")


data1 = union_all(def, mid) %>% union(forwards) %>% janitor::clean_names()
data2 = data1 %>%
  mutate(player = str_to_upper(player)) %>%
  mutate(player = stringi::stri_trans_general(player, id = "Latin-ASCII")) %>%
  fuzzyjoin::stringdist_join(
    player_id,
    by = "player",
    mode = "left",
    method = "jw",
    distance_col = "dist"
  ) %>%
  slice_min(order_by = dist, n = 1, by = "player.x") %>%
  mutate(num = row_number(), team = str_to_title(team)) %>%
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
  ) %>%
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
data3 = data2 %>%
  mutate(check_needed = dist > 0 & team != club) %>%
  merge(
    gk %>% janitor::clean_names() %>% dplyr::select(-total),
    by = "team"
  ) %>%
  dplyr::select(
    num,
    pos,
    player.x,
    player.y,
    club,
    team,
    league,
    total,
    notes,
    player_id,
    dist,
    check_needed
  ) %>%
  arrange(num)

write.csv(data3, "data/interim.csv", row.names = F, na = "")

data1 %>% count(player, club, pos) %>% filter(n > 1)


player_id2 = player_id %>%
  mutate(position = "", SBgoals = 0, SBapp = 0)

for (i in 1555:nrow(player_id2)) {
  # for(i in 1:100)
  skip_to_next <- FALSE
  # if(is.na(player_id2$id[i])){next}

  url = paste(
    "https://www.soccerbase.com/players/player.sd?player_id=",
    player_id2$player_id[i],
    "&season_id=156",
    sep = ""
  )
  link = RCurl::getURL(url)
  print(i)
  print(player_id$player[i])
  tryCatch(
    {
      tables = readHTMLTable(link)
      player_id2$position[i] = stringr::word(tables[[1]], 1)
      if (player_id2$player_id[i] == 75804) {
        tables$tpg$V7[5] = "1"
      }

      if (player_id2$player_id[i] == 52657) {
        tables$tpg$V7 = ""
      }
      appgoals = (tables$tpg) %>%
        filter(
          V1 %in%
            c(
              "Premier League",
              "EFL Cup",
              "English League Cup",
              "Europa League",
              "Community Shield",
              "Champions League",
              "FA Cup",
              "English FA Cup",
              "Europa Conference League",
              "Championship",
              "Championship Play-Off",
              "League One",
              "League One Play-Off",
              "League Two",
              "League Two Play-Off"
            )
        ) %>%
        mutate(Date = as.Date(substr(V2, 4, 13), "%d%b %Y")) %>%
        mutate(
          Goals = as.numeric(V7),
          App = 1,
          Goals = if_else(is.na(Goals), 0, Goals),
          player_id = player_id2$player_id[i],
          team = player_id2$team[i]
        )

      appgoals2 = appgoals %>%
        summarise(App = sum(App, na.rm = T), Goals = sum(Goals, na.rm = T))

      player_id2$SBgoals[i] = appgoals2[1, 2]

      player_id2$SBapp[i] = appgoals2[1, 1]

      weeklyreport = rbind(
        weeklyreport,
        appgoals %>% select(player_id, Date, Goals, App, team)
      )

      print(player_id2$SBgoals[i])
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
save(player_id2, file = "data/sbdata.RDa")
data1 = union_all(def, mid) %>% union(forwards) %>% janitor::clean_names()
data2 = player_id2 %>%
  fuzzyjoin::stringdist_join(
    data1 %>% mutate(player = str_to_upper(player)),
    by = "player",
    mode = "left",
    method = "jw",
    distance_col = "dist"
  ) %>%
  ungroup() %>%
  slice_min(order_by = dist, n = 1, by = "player.x")
data3 = data2 %>% filter(SBgoals > 0, position != "Goalkeeper")
