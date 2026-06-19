# setup.R – creates mock data for all tests and saves fixture files used by shinytest2.
# This file is automatically sourced by testthat before running any test file.

library(dplyr)
library(lubridate)
library(tidyr)

# ---------------------------------------------------------------------------
# Season dates
# ---------------------------------------------------------------------------
season_start <- as.Date("2025-07-28")
season_end   <- as.Date("2026-06-30")

# ---------------------------------------------------------------------------
# Managers
# ---------------------------------------------------------------------------
mock_managers_d <- data.frame(
  manager = c("ADAM WENGER", "MIKE NUTMEG", "JAMES ERIMUS", "SARAH ALT"),
  team    = c("WENGER BOYS", "NUTMEG UTD",  "ERIMUS",       "ALTERNATIVE ULSTERMEN"),
  stringsAsFactors = FALSE
)

mock_managers_o <- data.frame(
  manager = c("TOM PHOENIX", "LEE DREAMER", "CHRIS KING",  "PAT SURREAL"),
  team    = c("PHOENIX FLYERS", "DREAMERS DREGS", "KING'S XI", "SURREAL MADRID"),
  stringsAsFactors = FALSE
)

mock_managers <- rbind(
  mock_managers_d |> mutate(league = "didsbury"),
  mock_managers_o |> mutate(league = "original")
)

# ---------------------------------------------------------------------------
# Helper: build a squad row-by-row
# ---------------------------------------------------------------------------
make_player <- function(position, player, club, cost, goals, bought, sold,
                        bought2, sold2, sbapp, sbgoals, team, league) {
  data.frame(
    position  = position,
    player    = player,
    club      = club,
    cost      = cost,
    goals     = goals,
    bought    = bought,
    sold      = sold,
    bought2   = as.Date(bought2),
    sold2     = as.Date(sold2),
    SBapp     = sbapp,
    SBgoals   = sbgoals,
    team      = team,
    league    = league,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# WENGER BOYS (didsbury) – highest total
#   GK:    Arsenal conceded 5 → SBgoals = -5
#   DEF×2: 2 + 3 goals
#   MID×3: 4 + 5 + 3 goals
#   FWD×5: 8 + 6 + 5 + 4 + 3 goals
#   total = -5 + 2+3 + 4+5+3 + 8+6+5+4+3 = -5+5+12+26 = 38
# ---------------------------------------------------------------------------
wb_rows <- rbind(
  make_player("GOALKEEPER", NA, "ARSENAL",   "10", -5, "01-Aug", NA, season_start, season_end, 20, -5, "WENGER BOYS", "didsbury"),
  make_player("DEFENDER",  "JOHN STONES",     "MANCHESTER CITY",  "8",  2, "01-Aug", NA, season_start, season_end, 18, 2, "WENGER BOYS", "didsbury"),
  make_player("DEFENDER",  "TRENT ALEXANDER", "LIVERPOOL",        "9",  3, "01-Aug", NA, season_start, season_end, 22, 3, "WENGER BOYS", "didsbury"),
  make_player("MIDFIELDER","MARTIN ODEGAARD", "ARSENAL",          "12", 4, "01-Aug", NA, season_start, season_end, 15, 4, "WENGER BOYS", "didsbury"),
  make_player("MIDFIELDER","PHIL FODEN",       "MANCHESTER CITY",  "14", 5, "01-Aug", NA, season_start, season_end, 19, 5, "WENGER BOYS", "didsbury"),
  make_player("MIDFIELDER","JAMES MADDISON",   "TOTTENHAM",        "10", 3, "01-Aug", NA, season_start, season_end, 17, 3, "WENGER BOYS", "didsbury"),
  make_player("FORWARD",  "ERLING HAALAND",   "MANCHESTER CITY",  "20", 8, "01-Aug", NA, season_start, season_end, 24, 8, "WENGER BOYS", "didsbury"),
  make_player("FORWARD",  "COLE PALMER",      "CHELSEA",          "18", 6, "01-Aug", NA, season_start, season_end, 21, 6, "WENGER BOYS", "didsbury"),
  make_player("FORWARD",  "BUKAYO SAKA",      "ARSENAL",          "16", 5, "01-Aug", NA, season_start, season_end, 20, 5, "WENGER BOYS", "didsbury"),
  make_player("FORWARD",  "OLLIE WATKINS",    "ASTON VILLA",      "14", 4, "01-Aug", NA, season_start, season_end, 18, 4, "WENGER BOYS", "didsbury"),
  make_player("FORWARD",  "JARROD BOWEN",     "WEST HAM",         "11", 3, "01-Aug", NA, season_start, season_end, 16, 3, "WENGER BOYS", "didsbury")
)

# ---------------------------------------------------------------------------
# NUTMEG UTD (didsbury) – second highest
#   total = -8 + 1+2 + 3+4+2 + 6+5+4+3+2 = -8+3+9+20 = 24
# ---------------------------------------------------------------------------
nu_rows <- rbind(
  make_player("GOALKEEPER", NA, "CHELSEA",          "10", -8, "01-Aug", NA, season_start, season_end, 20, -8, "NUTMEG UTD", "didsbury"),
  make_player("DEFENDER",  "GABRIEL",             "ARSENAL",          "7",  1, "01-Aug", NA, season_start, season_end, 17,  1, "NUTMEG UTD", "didsbury"),
  make_player("DEFENDER",  "REECE JAMES",         "CHELSEA",          "8",  2, "01-Aug", NA, season_start, season_end, 10,  2, "NUTMEG UTD", "didsbury"),
  make_player("MIDFIELDER","DECLAN RICE",         "ARSENAL",          "12", 3, "01-Aug", NA, season_start, season_end, 22,  3, "NUTMEG UTD", "didsbury"),
  make_player("MIDFIELDER","ALEXIS MAC ALLISTER", "LIVERPOOL",        "11", 4, "01-Aug", NA, season_start, season_end, 20,  4, "NUTMEG UTD", "didsbury"),
  make_player("MIDFIELDER","MORGAN ROGERS",       "ASTON VILLA",      "",   2, "01-Aug", NA, season_start, season_end, 18,  2, "NUTMEG UTD", "didsbury"),
  make_player("FORWARD",  "DOMINIC SOLANKE",     "TOTTENHAM",        "13", 6, "01-Aug", NA, season_start, season_end, 20,  6, "NUTMEG UTD", "didsbury"),
  make_player("FORWARD",  "DARWIN NUNEZ",        "LIVERPOOL",        "15", 5, "01-Aug", NA, season_start, season_end, 17,  5, "NUTMEG UTD", "didsbury"),
  make_player("FORWARD",  "RICHARLISON",         "TOTTENHAM",        "10", 4, "01-Aug", NA, season_start, season_end, 14,  4, "NUTMEG UTD", "didsbury"),
  make_player("FORWARD",  "LYLE FOSTER",         "BURNLEY",          "",   3, "01-Aug", NA, season_start, season_end, 15,  3, "NUTMEG UTD", "didsbury"),
  make_player("FORWARD",  "ALEX IWOBI",          "FULHAM",           "",   2, "01-Aug", NA, season_start, season_end, 16,  2, "NUTMEG UTD", "didsbury")
)

# ---------------------------------------------------------------------------
# ERIMUS (didsbury) – third, with one sold player
#   total = -10 + 1+1 + 2+2+1 + 4+3+2+1+1 = -10+2+5+11 = 8
# ---------------------------------------------------------------------------
er_rows <- rbind(
  make_player("GOALKEEPER", NA, "BRENTFORD",        "10", -10, "01-Aug", NA,        season_start, season_end,          20, -10, "ERIMUS", "didsbury"),
  make_player("DEFENDER",  "DAN BURN",            "NEWCASTLE",        "6",  1, "01-Aug", NA,        season_start, season_end,          15,   1, "ERIMUS", "didsbury"),
  make_player("DEFENDER",  "PEDRO PORRO",         "TOTTENHAM",        "7",  1, "01-Aug", NA,        season_start, season_end,          14,   1, "ERIMUS", "didsbury"),
  make_player("MIDFIELDER","MATT O'RILEY",        "BRIGHTON",         "9",  2, "01-Aug", NA,        season_start, season_end,          16,   2, "ERIMUS", "didsbury"),
  # This midfielder was sold in December
  make_player("MIDFIELDER","JAMES WARD PROWSE",   "NOTTM FOREST",     "8",  0, "01-Aug", "01-Dec", season_start, as.Date("2025-12-01"), 10,   0, "ERIMUS", "didsbury"),
  # Replacement via transfer (cost = "")
  make_player("MIDFIELDER","YOANE WISSA",         "BRENTFORD",        "",   2, "01-Dec", NA,        as.Date("2025-12-01"), season_end, 12,   2, "ERIMUS", "didsbury"),
  make_player("MIDFIELDER","LEWIS HALL",          "NEWCASTLE",        "8",  1, "01-Aug", NA,        season_start, season_end,          19,   1, "ERIMUS", "didsbury"),
  make_player("FORWARD",  "CALLUM WILSON",       "NEWCASTLE",        "11", 4, "01-Aug", NA,        season_start, season_end,          12,   4, "ERIMUS", "didsbury"),
  make_player("FORWARD",  "IVAN TONEY",          "BRENTFORD",        "13", 3, "01-Aug", NA,        season_start, season_end,          13,   3, "ERIMUS", "didsbury"),
  make_player("FORWARD",  "CHRIS WOOD",          "NOTTM FOREST",     "10", 2, "01-Aug", NA,        season_start, season_end,          14,   2, "ERIMUS", "didsbury"),
  make_player("FORWARD",  "ELIJAH ADEBAYO",      "LUTON",            "",   1, "01-Aug", NA,        season_start, season_end,          10,   1, "ERIMUS", "didsbury"),
  make_player("FORWARD",  "DANGO OUATTARA",      "BOURNEMOUTH",      "",   1, "01-Aug", NA,        season_start, season_end,          11,   1, "ERIMUS", "didsbury")
)

# ---------------------------------------------------------------------------
# ALTERNATIVE ULSTERMEN (didsbury) – fourth, zero goals
#   total = -15 + 0+0 + 0+0+0 + 0+0+0+0+0 = -15
# ---------------------------------------------------------------------------
au_rows <- rbind(
  make_player("GOALKEEPER", NA, "EVERTON",          "8",  -15, "01-Aug", NA, season_start, season_end, 20, -15, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("DEFENDER",  "MICHAEL KEANE",      "EVERTON",          "5",   0, "01-Aug", NA, season_start, season_end, 15,   0, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("DEFENDER",  "SEAMUS COLEMAN",     "EVERTON",          "5",   0, "01-Aug", NA, season_start, season_end, 14,   0, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("MIDFIELDER","JAMES GARNER",        "EVERTON",          "6",   0, "01-Aug", NA, season_start, season_end, 14,   0, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("MIDFIELDER","ABDOULAYE DOUCOURE",  "EVERTON",          "7",   0, "01-Aug", NA, season_start, season_end, 13,   0, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("MIDFIELDER","IDRISSA GUEYE",       "EVERTON",          "6",   0, "01-Aug", NA, season_start, season_end, 12,   0, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("FORWARD",  "DOMINIC CALVERT LEWIN","EVERTON",         "11",  0, "01-Aug", NA, season_start, season_end, 13,   0, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("FORWARD",  "DEMARAI GRAY",        "EVERTON",          "7",   0, "01-Aug", NA, season_start, season_end, 12,   0, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("FORWARD",  "ELLIS SIMMS",         "EVERTON",          "6",   0, "01-Aug", NA, season_start, season_end, 11,   0, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("FORWARD",  "BETO",                "EVERTON",          "8",   0, "01-Aug", NA, season_start, season_end, 10,   0, "ALTERNATIVE ULSTERMEN", "didsbury"),
  make_player("FORWARD",  "NEAL MAUPAY",         "EVERTON",          "7",   0, "01-Aug", NA, season_start, season_end,  9,   0, "ALTERNATIVE ULSTERMEN", "didsbury")
)

# ---------------------------------------------------------------------------
# Original league teams
# ---------------------------------------------------------------------------
# PHOENIX FLYERS – total = -3 + 2+3 + 4+5+2 + 7+5+4+3+2 = -3+5+11+21 = 34
pf_rows <- rbind(
  make_player("GOALKEEPER", NA, "BRIGHTON",         "10", -3, "01-Aug", NA, season_start, season_end, 20, -3, "PHOENIX FLYERS", "original"),
  make_player("DEFENDER",  "LEWIS DUNK",          "BRIGHTON",         "7",  2, "01-Aug", NA, season_start, season_end, 20,  2, "PHOENIX FLYERS", "original"),
  make_player("DEFENDER",  "TARIQ LAMPTEY",       "BRIGHTON",         "7",  3, "01-Aug", NA, season_start, season_end, 18,  3, "PHOENIX FLYERS", "original"),
  make_player("MIDFIELDER","KAORU MITOMA",        "BRIGHTON",         "12", 4, "01-Aug", NA, season_start, season_end, 17,  4, "PHOENIX FLYERS", "original"),
  make_player("MIDFIELDER","PASCAL GROSS",        "BRIGHTON",         "10", 5, "01-Aug", NA, season_start, season_end, 21,  5, "PHOENIX FLYERS", "original"),
  make_player("MIDFIELDER","SOLLY MARCH",         "BRIGHTON",         "10", 2, "01-Aug", NA, season_start, season_end, 14,  2, "PHOENIX FLYERS", "original"),
  make_player("FORWARD",  "JOAO PEDRO",           "BRIGHTON",         "14", 7, "01-Aug", NA, season_start, season_end, 19,  7, "PHOENIX FLYERS", "original"),
  make_player("FORWARD",  "DANNY WELBECK",        "BRIGHTON",         "9",  5, "01-Aug", NA, season_start, season_end, 17,  5, "PHOENIX FLYERS", "original"),
  make_player("FORWARD",  "JULIO ENCISO",         "BRIGHTON",         "10", 4, "01-Aug", NA, season_start, season_end, 12,  4, "PHOENIX FLYERS", "original"),
  make_player("FORWARD",  "EVAN FERGUSON",        "BRIGHTON",         "11", 3, "01-Aug", NA, season_start, season_end, 11,  3, "PHOENIX FLYERS", "original"),
  make_player("FORWARD",  "SIMON ADINGRA",        "BRIGHTON",         "9",  2, "01-Aug", NA, season_start, season_end, 15,  2, "PHOENIX FLYERS", "original")
)

# DREAMERS DREGS – total = -6 + 1+2 + 3+3+2 + 5+4+3+2+1 = -6+3+8+15 = 20
dd_rows <- rbind(
  make_player("GOALKEEPER", NA, "NOTTM FOREST",     "9",  -6, "01-Aug", NA, season_start, season_end, 20, -6, "DREAMERS DREGS", "original"),
  make_player("DEFENDER",  "FIKAYO TOMORI",       "MILAN",            "7",  1, "01-Aug", NA, season_start, season_end, 15,  1, "DREAMERS DREGS", "original"),
  make_player("DEFENDER",  "KIERAN TRIPPIER",     "NEWCASTLE",        "8",  2, "01-Aug", NA, season_start, season_end, 12,  2, "DREAMERS DREGS", "original"),
  make_player("MIDFIELDER","EMILE SMITH ROWE",    "ARSENAL",          "9",  3, "01-Aug", NA, season_start, season_end, 14,  3, "DREAMERS DREGS", "original"),
  make_player("MIDFIELDER","ADAM LALLANA",        "BRIGHTON",         "7",  3, "01-Aug", NA, season_start, season_end, 13,  3, "DREAMERS DREGS", "original"),
  make_player("MIDFIELDER","CONOR GALLAGHER",     "CHELSEA",          "9",  2, "01-Aug", NA, season_start, season_end, 17,  2, "DREAMERS DREGS", "original"),
  make_player("FORWARD",  "NICOLAS JACKSON",      "CHELSEA",          "14", 5, "01-Aug", NA, season_start, season_end, 18,  5, "DREAMERS DREGS", "original"),
  make_player("FORWARD",  "RASMUS HOJLUND",       "MANCHESTER UTD",   "14", 4, "01-Aug", NA, season_start, season_end, 15,  4, "DREAMERS DREGS", "original"),
  make_player("FORWARD",  "MICHAIL ANTONIO",      "WEST HAM",         "10", 3, "01-Aug", NA, season_start, season_end, 14,  3, "DREAMERS DREGS", "original"),
  make_player("FORWARD",  "TEEMU PUKKI",          "NORWICH",          "8",  2, "01-Aug", NA, season_start, season_end,  9,  2, "DREAMERS DREGS", "original"),
  make_player("FORWARD",  "SCOTT MCTOMINAY",      "NAPOLI",           "9",  1, "01-Aug", NA, season_start, season_end, 12,  1, "DREAMERS DREGS", "original")
)

# KING'S XI – total = -12 + 0+0 + 1+1+0 + 2+1+0+0+0 = -12+0+2+3 = -7
kx_rows <- rbind(
  make_player("GOALKEEPER", NA, "WOLVERHAMPTON",   "7", -12, "01-Aug", NA, season_start, season_end, 19, -12, "KING'S XI", "original"),
  make_player("DEFENDER",  "MATT DOHERTY",        "WOLVERHAMPTON",   "5",  0, "01-Aug", NA, season_start, season_end, 14,   0, "KING'S XI", "original"),
  make_player("DEFENDER",  "CONOR COADY",         "WOLVERHAMPTON",   "5",  0, "01-Aug", NA, season_start, season_end, 13,   0, "KING'S XI", "original"),
  make_player("MIDFIELDER","JOAO GOMES",           "WOLVERHAMPTON",   "7",  1, "01-Aug", NA, season_start, season_end, 17,   1, "KING'S XI", "original"),
  make_player("MIDFIELDER","MARIO LEMINA",        "WOLVERHAMPTON",   "7",  1, "01-Aug", NA, season_start, season_end, 16,   1, "KING'S XI", "original"),
  make_player("MIDFIELDER","TOMMY DOYLE",         "WOLVERHAMPTON",   "6",  0, "01-Aug", NA, season_start, season_end, 15,   0, "KING'S XI", "original"),
  make_player("FORWARD",  "MATHEUS CUNHA",        "WOLVERHAMPTON",   "11", 2, "01-Aug", NA, season_start, season_end, 17,   2, "KING'S XI", "original"),
  make_player("FORWARD",  "PEDRO NETO",           "WOLVERHAMPTON",   "10", 1, "01-Aug", NA, season_start, season_end, 16,   1, "KING'S XI", "original"),
  make_player("FORWARD",  "HWANG HEE CHAN",       "WOLVERHAMPTON",   "9",  0, "01-Aug", NA, season_start, season_end, 14,   0, "KING'S XI", "original"),
  make_player("FORWARD",  "FABIO SILVA",          "WOLVERHAMPTON",   "8",  0, "01-Aug", NA, season_start, season_end, 12,   0, "KING'S XI", "original"),
  make_player("FORWARD",  "ENNER VALENCIA",       "WOLVERHAMPTON",   "8",  0, "01-Aug", NA, season_start, season_end, 11,   0, "KING'S XI", "original")
)

# SURREAL MADRID – total = -20 + 0+0 + 0+0+0 + 0+0+0+0+0 = -20
sm_rows <- rbind(
  make_player("GOALKEEPER", NA, "SHEFFIELD UTD",    "5", -20, "01-Aug", NA, season_start, season_end, 18, -20, "SURREAL MADRID", "original"),
  make_player("DEFENDER",  "CHRIS BASHAM",        "SHEFFIELD UTD",   "4",  0, "01-Aug", NA, season_start, season_end, 13,   0, "SURREAL MADRID", "original"),
  make_player("DEFENDER",  "GEORGE BALDOCK",      "SHEFFIELD UTD",   "4",  0, "01-Aug", NA, season_start, season_end, 12,   0, "SURREAL MADRID", "original"),
  make_player("MIDFIELDER","OLIVER NORWOOD",      "SHEFFIELD UTD",   "5",  0, "01-Aug", NA, season_start, season_end, 12,   0, "SURREAL MADRID", "original"),
  make_player("MIDFIELDER","JOHN FLECK",           "SHEFFIELD UTD",   "5",  0, "01-Aug", NA, season_start, season_end, 11,   0, "SURREAL MADRID", "original"),
  make_player("MIDFIELDER","SANDER BERGE",        "SHEFFIELD UTD",   "6",  0, "01-Aug", NA, season_start, season_end, 10,   0, "SURREAL MADRID", "original"),
  make_player("FORWARD",  "RHIAN BREWSTER",       "SHEFFIELD UTD",   "8",  0, "01-Aug", NA, season_start, season_end,  9,   0, "SURREAL MADRID", "original"),
  make_player("FORWARD",  "BEN OSBORN",           "SHEFFIELD UTD",   "6",  0, "01-Aug", NA, season_start, season_end,  8,   0, "SURREAL MADRID", "original"),
  make_player("FORWARD",  "LYS MOUSSET",          "SHEFFIELD UTD",   "7",  0, "01-Aug", NA, season_start, season_end,  7,   0, "SURREAL MADRID", "original"),
  make_player("FORWARD",  "DAVID MCGOLDRICK",     "SHEFFIELD UTD",   "7",  0, "01-Aug", NA, season_start, season_end,  6,   0, "SURREAL MADRID", "original"),
  make_player("FORWARD",  "BILLY SHARP",          "SHEFFIELD UTD",   "7",  0, "01-Aug", NA, season_start, season_end,  5,   0, "SURREAL MADRID", "original")
)

# ---------------------------------------------------------------------------
# Combined dl
# ---------------------------------------------------------------------------
mock_dl <- rbind(
  wb_rows, nu_rows, er_rows, au_rows,
  pf_rows, dd_rows, kx_rows, sm_rows
)

# ---------------------------------------------------------------------------
# Daily scoring data
# ---------------------------------------------------------------------------
# Each row is one match day record. Dates must fall between bought2 and sold2.
# We'll create records for two game weeks:
#   GW1: 2025-08-16 (round 1 cup date is 2025-09-12, so before)
#   GW2: 2025-09-13 (inside cup round 1 window)
#   GW3: 2026-01-17 (inside cup SF window)

make_daily <- function(Date, position, player, club, sbgoals, app = 1, team, league,
                       bought2 = season_start, sold2 = season_end, cost = "10",
                       goals = sbgoals, bought = "01-Aug", sold = NA_character_) {
  data.frame(
    Date     = as.Date(Date),
    SBgoals  = sbgoals,
    App      = app,
    team     = team,
    position = position,
    player   = player,
    club     = club,
    cost     = cost,
    goals    = goals,
    bought   = bought,
    sold     = sold,
    bought2  = as.Date(bought2),
    sold2    = as.Date(sold2),
    league   = league,
    stringsAsFactors = FALSE
  )
}

mock_daily <- rbind(
  # GW1 (2025-08-16): WENGER BOYS scorers
  make_daily("2025-08-16", "FORWARD",   "ERLING HAALAND",   "MANCHESTER CITY", 2, team = "WENGER BOYS",          league = "didsbury"),
  make_daily("2025-08-16", "FORWARD",   "COLE PALMER",      "CHELSEA",         1, team = "WENGER BOYS",          league = "didsbury"),
  make_daily("2025-08-16", "MIDFIELDER","PHIL FODEN",        "MANCHESTER CITY", 1, team = "WENGER BOYS",          league = "didsbury"),
  make_daily("2025-08-16", "GOALKEEPER",NA,                  "ARSENAL",        -1, team = "WENGER BOYS",          league = "didsbury", player = NA_character_),

  # GW1: NUTMEG UTD
  make_daily("2025-08-16", "FORWARD",   "DOMINIC SOLANKE",  "TOTTENHAM",       1, team = "NUTMEG UTD",           league = "didsbury"),
  make_daily("2025-08-16", "GOALKEEPER",NA,                  "CHELSEA",        -2, team = "NUTMEG UTD",           league = "didsbury", player = NA_character_),

  # GW1: PHOENIX FLYERS
  make_daily("2025-08-16", "FORWARD",   "JOAO PEDRO",       "BRIGHTON",        2, team = "PHOENIX FLYERS",       league = "original"),
  make_daily("2025-08-16", "MIDFIELDER","KAORU MITOMA",     "BRIGHTON",        1, team = "PHOENIX FLYERS",       league = "original"),
  make_daily("2025-08-16", "GOALKEEPER",NA,                  "BRIGHTON",       -1, team = "PHOENIX FLYERS",       league = "original", player = NA_character_),

  # GW1: DREAMERS DREGS
  make_daily("2025-08-16", "FORWARD",   "NICOLAS JACKSON",  "CHELSEA",         1, team = "DREAMERS DREGS",       league = "original"),
  make_daily("2025-08-16", "GOALKEEPER",NA,                  "NOTTM FOREST",   -2, team = "DREAMERS DREGS",       league = "original", player = NA_character_),

  # GW2 (cup window 2025-09-12 to 2025-09-15): WENGER BOYS vs NUTMEG UTD (didsbury cup R1)
  make_daily("2025-09-13", "FORWARD",   "ERLING HAALAND",   "MANCHESTER CITY", 3, team = "WENGER BOYS",          league = "didsbury"),
  make_daily("2025-09-13", "MIDFIELDER","MARTIN ODEGAARD",  "ARSENAL",         1, team = "WENGER BOYS",          league = "didsbury"),
  make_daily("2025-09-13", "GOALKEEPER",NA,                  "ARSENAL",        -1, team = "WENGER BOYS",          league = "didsbury", player = NA_character_),

  make_daily("2025-09-13", "FORWARD",   "DOMINIC SOLANKE",  "TOTTENHAM",       1, team = "NUTMEG UTD",           league = "didsbury"),
  make_daily("2025-09-13", "MIDFIELDER","DECLAN RICE",       "ARSENAL",         1, team = "NUTMEG UTD",           league = "didsbury"),
  make_daily("2025-09-13", "GOALKEEPER",NA,                  "CHELSEA",        -3, team = "NUTMEG UTD",           league = "didsbury", player = NA_character_),

  # GW2: PHOENIX FLYERS vs DREAMERS DREGS (original cup R1)
  make_daily("2025-09-13", "FORWARD",   "JOAO PEDRO",       "BRIGHTON",        1, team = "PHOENIX FLYERS",       league = "original"),
  make_daily("2025-09-13", "GOALKEEPER",NA,                  "BRIGHTON",       -1, team = "PHOENIX FLYERS",       league = "original", player = NA_character_),

  make_daily("2025-09-13", "FORWARD",   "NICOLAS JACKSON",  "CHELSEA",         2, team = "DREAMERS DREGS",       league = "original"),
  make_daily("2025-09-13", "GOALKEEPER",NA,                  "NOTTM FOREST",    0, team = "DREAMERS DREGS",       league = "original", player = NA_character_),

  # GW3 (cup SF window 2026-01-16 to 2026-01-19): ERIMUS vs ALTERNATIVE ULSTERMEN
  make_daily("2026-01-17", "FORWARD",   "CALLUM WILSON",    "NEWCASTLE",       2, team = "ERIMUS",               league = "didsbury"),
  make_daily("2026-01-17", "MIDFIELDER","YOANE WISSA",       "BRENTFORD",       1, team = "ERIMUS",               league = "didsbury"),
  make_daily("2026-01-17", "GOALKEEPER",NA,                  "BRENTFORD",      -1, team = "ERIMUS",               league = "didsbury", player = NA_character_),

  make_daily("2026-01-17", "GOALKEEPER",NA,                  "EVERTON",        -2, team = "ALTERNATIVE ULSTERMEN",league = "didsbury", player = NA_character_),

  # GW3: KING'S XI vs SURREAL MADRID (original cup SF) – tied on total, King's XI wins on gf
  make_daily("2026-01-17", "FORWARD",   "MATHEUS CUNHA",    "WOLVERHAMPTON",   1, team = "KING'S XI",            league = "original"),
  make_daily("2026-01-17", "GOALKEEPER",NA,                  "WOLVERHAMPTON",  -1, team = "KING'S XI",            league = "original", player = NA_character_),

  make_daily("2026-01-17", "GOALKEEPER",NA,                  "SHEFFIELD UTD",  -1, team = "SURREAL MADRID",       league = "original", player = NA_character_)
)

# ---------------------------------------------------------------------------
# Cup ties
# ---------------------------------------------------------------------------
mock_cupties <- data.frame(
  comp  = c("didsbury", "didsbury", "original", "original", "bfl", "bfl"),
  round = c("R1",       "SF",       "R1",       "SF",       "R1",  "SF"),
  team1 = c("WENGER BOYS",      "ERIMUS",           "PHOENIX FLYERS", "KING'S XI",   "WENGER BOYS",   "NUTMEG UTD"),
  team2 = c("NUTMEG UTD",       "ALTERNATIVE ULSTERMEN", "DREAMERS DREGS", "SURREAL MADRID", "PHOENIX FLYERS", "DREAMERS DREGS"),
  date  = as.Date(c("2025-09-12", "2026-01-16", "2025-09-12", "2026-01-16", "2025-09-12", "2026-01-16")),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Time metadata
# ---------------------------------------------------------------------------
mock_time <- list(
  update_time = as.POSIXct("2026-03-30 12:00:00", tz = "UTC"),
  mod_d       = as.POSIXct("2026-03-29 10:00:00", tz = "UTC"),
  mod_o       = as.POSIXct("2026-03-29 11:00:00", tz = "UTC")
)

# Expose with names that match the app variables
dl       <- mock_dl
daily    <- mock_daily
cupties  <- mock_cupties
time     <- mock_time
managers <- mock_managers
managers_d <- mock_managers_d
managers_o <- mock_managers_o

# ---------------------------------------------------------------------------
# Compute the expected league table (same logic as app.R)
# ---------------------------------------------------------------------------
expected_league <- mock_managers |>
  merge(
    mock_dl |>
      group_by(team) |>
      summarise(total = sum(SBgoals, na.rm = TRUE), .groups = "drop"),
    by = "team", all = TRUE
  ) |>
  merge(
    mock_dl |>
      filter(position != "GOALKEEPER") |>
      group_by(team) |>
      summarise(gf = sum(SBgoals, na.rm = TRUE), .groups = "drop"),
    by = "team", all = TRUE
  ) |>
  merge(
    mock_dl |>
      filter(position == "GOALKEEPER") |>
      group_by(team) |>
      summarise(ga = -sum(SBgoals, na.rm = TRUE), .groups = "drop"),
    by = "team", all = TRUE
  ) |>
  arrange(-total, -gf) |>
  mutate(rank = row_number(), .by = "league")

# ---------------------------------------------------------------------------
# Save fixture files (used by the shinytest2 integration tests)
# ---------------------------------------------------------------------------
fixture_dir <- file.path(
  dirname(dirname(dirname(testthat::test_path()))), # app root
  "tests", "testthat", "fixtures"
)
if (!dir.exists(fixture_dir)) dir.create(fixture_dir, recursive = TRUE)

save(dl, daily, time, cupties,
     file = file.path(fixture_dir, "data.RDa"))

managers_d <- mock_managers_d
managers_o <- mock_managers_o
save(managers_d, managers_o,
     file = file.path(fixture_dir, "managers.RDa"))
