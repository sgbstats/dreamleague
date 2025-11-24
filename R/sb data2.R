library(XML)
library(tidyverse)
library(RCurl)
library(readxl)
library(openxlsx)
# library(beepr)
# library(chron)
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
      team == "Norwich" ~ "Norwich City",

      T ~ team
    )
  )

t = 0
player_id0 = tribble(
  ~player , ~player_id , ~team , ~team_id
)
for (i in 1:nrow(team_id)) {
  skip_to_next <- FALSE

  cat(paste0(team_id$team[i], "\n"))
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
      player == "Manny Monthe" ~ "EMMANUEL MONTHE",
      player_id == 151107 ~ "IGOR THIAGO",
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
  filter(player_id %notin% c(116945, 107014, 69468, 188923)) %>%
  rbind.data.frame(tribble(
    ~"player"          , ~"n" , ~"player_id" , ~"team"    , ~"team_id" ,
    "BORJA SAINZ"      ,    1 ,       124408 , "FC Porto" ,        978 ,
    "THELO AASGAARD"   ,    1 ,       133855 , "Rangers"  ,       2104 ,
    "ARMAND LAURIENTE" ,    1 ,       111761 , "Sassuolo" ,       4692 ,
    "HEUNG-MIN SON"    ,    1 ,        57526 , "LA FC"    ,       6693 ,
  ))


team_id = team_id %>% mutate(team = str_to_upper(team))


mostrecent = max(list.files(
  path = "data/legacy/playerdata/",
  pattern = NULL,
  all.files = FALSE,
  full.names = FALSE,
  recursive = FALSE,
  ignore.case = FALSE,
  include.dirs = FALSE,
  no.. = FALSE
))

load(paste("data/legacy/playerdata/", mostrecent, sep = ""))

player_id = player_id %>%
  rbind.data.frame(legacy) %>%
  group_by(player_id) %>%
  slice_min(team, with_ties = F) %>%
  filter(player_id %notin% c())

legacy = player_id
save(
  legacy,
  file = paste("data/legacy/playerdata/ids", Sys.Date(), ".RDa", sep = "")
)
save(team_id, player_id, file = "data/ids.RDa")
