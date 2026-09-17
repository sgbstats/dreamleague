library(readxl)
library(tidyverse)

dl2627 <- readxl::read_excel(
  "data/DreamLeague26-27.xlsx",
  na = c("SOLD"),
  sheet = "Stats",
  skip = 0,
  col_names = F
) |>
  suppressMessages() |>
  dplyr::select(2:8)

managers2627 <- readxl::read_excel(
  "data/DreamLeague26-27.xlsx",
  na = c("SOLD"),
  sheet = "Stats"
) |>
  suppressMessages() |>
  dplyr::select(11:12) |>
  na.omit() |>
  rename(manager = 1, team = 2) |>
  filter(team != "TEAM")

dl2526 <- readxl::read_excel(
  "data/old/DreamLeague25-26.xlsx",
  na = c("SOLD"),
  sheet = "Stats",
  skip = 0,
  col_names = F
) |>
  suppressMessages() |>
  dplyr::select(2:8)

managers2526 <- readxl::read_excel(
  "data/old/DreamLeague25-26.xlsx",
  na = c("SOLD"),
  sheet = "Stats"
) |>
  suppressMessages() |>
  dplyr::select(11:12) |>
  na.omit() |>
  rename(manager = 1, team = 2) |>
  filter(team != "TEAM")

dl2425 <- readxl::read_excel(
  "data/old/DreamLeague24-25.xlsx",
  na = c("SOLD"),
  sheet = "Stats",
  skip = 0,
  col_names = F
) |>
  suppressMessages() |>
  dplyr::select(2:8)

managers2425 <- readxl::read_excel(
  "data/old/DreamLeague24-25.xlsx",
  na = c("SOLD"),
  sheet = "Stats"
) |>
  suppressMessages() |>
  dplyr::select(11:12) |>
  na.omit() |>
  rename(manager = 1, team = 2) |>
  filter(team != "TEAM")


dl2324 <- readxl::read_excel(
  "data/old/DreamLeague23-24.xlsx",
  na = c("SOLD"),
  sheet = "Stats",
  skip = 0,
  col_names = F
) |>
  suppressMessages() |>
  dplyr::select(2:8)

managers2324 <- readxl::read_excel(
  "data/old/DreamLeague23-24.xlsx",
  na = c("SOLD"),
  sheet = "Stats"
) |>
  suppressMessages() |>
  dplyr::select(11:12) |>
  na.omit() |>
  rename(manager = 1, team = 2) |>
  filter(team != "TEAM")

dl2223 <- readxl::read_excel(
  "data/old/DreamLeague22-23.xlsx",
  na = c("SOLD"),
  sheet = "Stats",
  skip = 0,
  col_names = F
) |>
  suppressMessages() |>
  dplyr::select(2:8)

managers2223 <- readxl::read_excel(
  "data/old/DreamLeague22-23.xlsx",
  na = c("SOLD"),
  sheet = "Stats"
) |>
  suppressMessages() |>
  dplyr::select(11:12) |>
  na.omit() |>
  rename(manager = 1, team = 2) |>
  filter(team != "TEAM")

dl2122 <- readxl::read_excel(
  "data/old/DreamLeague21-22.xlsx",
  na = c("SOLD"),
  sheet = "Stats",
  skip = 0,
  col_names = F
) |>
  suppressMessages() |>
  dplyr::select(2:8)

managers2122 <- readxl::read_excel(
  "data/old/DreamLeague21-22.xlsx",
  na = c("SOLD"),
  sheet = "Stats"
) |>
  suppressMessages() |>
  dplyr::select(11:12) |>
  na.omit() |>
  rename(manager = 1, team = 2) |>
  filter(team != "TEAM")

dl2021 <- readxl::read_excel(
  "data/old/DreamLeague20-21.xlsx",
  na = c("SOLD"),
  sheet = "Stats",
  skip = 0,
  col_names = F
) |>
  suppressMessages() |>
  dplyr::select(2:8)

managers2021 <- readxl::read_excel(
  "data/old/DreamLeague20-21.xlsx",
  na = c("SOLD"),
  sheet = "Stats"
) |>
  suppressMessages() |>
  dplyr::select(11:12) |>
  na.omit() |>
  rename(manager = 1, team = 2) |>
  filter(team != "TEAM")

process <- function(dl, managers, league = "Didsbury") {
  teams <- dl |>
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
      is_transfer = grepl("TRANSFER|TRASNFER", cost, ignore.case = TRUE),
      cost = gsub("TRANSFER|TRASNFER", "", cost, ignore.case = TRUE)
    )

  managers <- managers |>
    rbind.data.frame(tribble(
      ~"manager"   , ~"team"   ,
      "WILL SMITH" , "I ROBOT"
    ))

  teams2 <- teams
  for (i in 1:nrow(teams)) {
    if (teams$player[i] %in% managers$team) {
      team <- teams$player[i]
    }
    if (
      teams$position[i] %in%
        c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")
    ) {
      teams2$team[i] <- team
    }
  }

  teams2 <- teams2 |> filter(team != "I ROBOT")
  managers <- managers |> filter(team != "I ROBOT")

  # sb_id=read.csv("data/sb_id.csv")
  load("data/ids.RDa")

  teams3a <- teams2 |>
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
        player == "ELI JUNIOR KROUPI" ~ "JUNIOR KROUPI",
        player == "GABRIEL MAGHALAES" ~ "GABRIEL",
        T ~ player
      )
    )

  if (league == "Didsbury") {
    teams3 <- teams3a |>
      mutate(
        bought = format(as.Date(bought), "%d-%b"),
        sold = format(as.Date(sold), "%d-%b")
      )
  } else if (league == "Original") {
    for (i in 1:(nrow(teams3a) - 1)) {
      teams3a$sold[i] <- teams3a$bought[i + 1]
    }
    teams3 <- teams3a |>
      mutate(
        bought = format(openxlsx::convertToDate(bought) - 1, "%d-%b"),
        sold = format(openxlsx::convertToDate(sold) - 1, "%d-%b")
      )
  }

  return(teams3)
}


data <-
  rbind.data.frame(
    process(dl2627, managers2627) |> mutate(season = "26-27"),
    process(dl2526, managers2526) |> mutate(season = "25-26"),
    process(dl2425, managers2425) |> mutate(season = "24-25"),
    process(dl2324, managers2324) |> mutate(season = "23-24"),
    process(dl2223, managers2223) |> mutate(season = "22-23"),
    process(dl2122, managers2122) |> mutate(season = "21-22"),
    process(dl2021, managers2021) |> mutate(season = "20-21")
  )

plot_data <- data |>
  drop_na(bought) |>
  mutate(
    season_start = 2000L + as.integer(substr(season, 1, 2)),
    bought_date = as.Date(bought, format = "%d-%b"),
    season_date = as.Date(
      paste0(
        if_else(month(bought_date) >= 8L, season_start, season_start + 1L),
        "-",
        format(bought_date, "%m-%d")
      )
    ),
    within_season_day = as.integer(
      season_date - as.Date(paste0(season_start, "-08-01"))
    ) +
      1L,
    plot_date = as.Date("2020-08-01") + within_season_day - 1L
  ) |>
  count(season, within_season_day, plot_date, name = "rows") |>
  arrange(season, within_season_day) |>
  group_by(season) |>
  mutate(cumulative_rows = cumsum(rows)) |>
  ungroup()

ggplot(
  plot_data,
  aes(
    x = plot_date,
    y = cumulative_rows,
    color = season,
    group = season
  )
) +
  geom_line() +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    expand = c(0, 0)
  ) +
  labs(
    x = "Date",
    y = "Cumulative number of tranfers",
    color = "Season"
  )

transfer_data <- data |>
  filter(!is.na(bought)) |>
  mutate(
    bought_date = as.Date(bought, format = "%d-%b"),
    transfer_month = month(bought_date, label = TRUE, abbr = FALSE),
    transfer_month_number = month(bought_date)
  )

# Highest-scoring transferred outfield player(s) for each calendar month.
most_goals_by_transfer_month <- transfer_data |>
  filter(
    position != "GOALKEEPER",
    !is.na(player),
    !is.na(goals)
  ) |>
  group_by(transfer_month, transfer_month_number) |>
  slice_max(goals, n = 1, with_ties = TRUE) |>
  ungroup() |>
  arrange(transfer_month_number, desc(goals), player) |>
  select(
    transfer_month,
    player,
    team,
    goals,
    bought,
    season
  )

goalkeeper_transfers <- transfer_data |>
  filter(position == "GOALKEEPER", !is.na(goals)) |>
  mutate(conceded = -goals)

# A lower conceded total is better; ties are retained. The current
# season is excluded because it is not yet complete.
best_goalkeeper_transfer <- goalkeeper_transfers |>
  filter(season != "26-27") |>
  group_by(transfer_month, transfer_month_number) |>
  slice_min(conceded, n = 1, with_ties = TRUE) |>
  ungroup() |>
  arrange(transfer_month_number, conceded, club) |>
  select(transfer_month, club, team, conceded, bought, season)

# A higher conceded total is worse; ties are retained.
worst_goalkeeper_transfer <- goalkeeper_transfers |>
  group_by(transfer_month, transfer_month_number) |>
  slice_max(conceded, n = 1, with_ties = TRUE) |>
  ungroup() |>
  arrange(transfer_month_number, desc(conceded), club) |>
  select(transfer_month, club, team, conceded, bought, season)
