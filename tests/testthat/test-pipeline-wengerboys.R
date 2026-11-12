source(file.path("..", "..", "R", "wengerboys-lib.R"))

fixture <- function(name) {
  paste(readLines(file.path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("line-up parsing derives starters, substitutes and minutes", {
  lineup <- parse_lineup_bubble(fixture("lineup_bubble.html"))
  row <- function(id) lineup[lineup$player_id == id, ]

  expect_equal(row("134599")$status, "Started")
  expect_equal(row("134599")$entry_minute, 0)
  expect_equal(row("134599")$minutes, 90)
  expect_equal(row("72740")$minutes, 61)
  expect_equal(row("55425")$status, "Substitute")
  expect_equal(row("55425")$entry_minute, 61)
  expect_equal(row("55425")$minutes, 29)
  expect_equal(row("99454")$minutes, 72)
  expect_equal(row("165853")$minutes, 17)
  expect_equal(row("222266")$minutes, 1)
  expect_equal(row("201473")$status, "Unused substitute")
  expect_true(is.na(row("201473")$entry_minute))
  expect_equal(row("201473")$side, "home")
  expect_equal(row("157662")$side, "away")
  expect_equal(nrow(row("1")), 0)
})

test_that("line-up sending-off markers belong to the correct players", {
  html <- fixture("lineup_bubble.html")
  html <- sub(
    "H Male</a>", "H Male</a><img src='red.png' /> (s/o 65)",
    html, fixed = TRUE
  )
  html <- sub(
    "T Ince</a>, 61", "T Ince</a>, 61 <img src='red.png' /> (s/o 80)",
    html, fixed = TRUE
  )
  html <- sub(
    "J Slater</a>", "J Slater</a><img src='red.png' /> (s/o)",
    html, fixed = TRUE
  )
  html <- sub(
    "N Ramming</a>", "N Ramming</a><img src='red.png' /> (s/o 90+4)",
    html, fixed = TRUE
  )
  lineup <- parse_lineup_bubble(html)
  row <- function(id) lineup[lineup$player_id == id, ]

  expect_equal(row("134599")$dismissal_minute, 65)
  expect_equal(row("55425")$dismissal_minute, 80)
  expect_true(is.na(row("165851")$dismissal_minute))
  expect_true(is.na(row("201473")$dismissal_minute))
  expect_equal(row("185565")$dismissal_minute, 90)
  expect_equal(row("55425")$minutes, 29)
  expect_equal(row("72740")$minutes, 61)
  expect_equal(row("142753")$status, "Started")
})

test_that("player page parsing reads club, position and goals", {
  page <- parse_player_page(fixture("player_page.html"), 55425)

  expect_equal(page$position, "MIDFIELDER")
  expect_equal(page$club, "Tranmere Rovers")
  expect_equal(page$club_id, 2598)
  expect_equal(nrow(page$games), 14)
  expect_equal(sum(page$games$goals), 2)
  expect_true(all(is.na(page$games$dismissal)))
})

test_that("player page distinguishes dismissals from empty and yellow cards", {
  html <- xml2::read_html(fixture("player_page.html"))
  rows <- rvest::html_elements(html, "table#tpg tr.match")
  add_card <- function(row, cell, image) {
    div <- rvest::html_element(rvest::html_elements(row, "td")[[cell]], "div")
    xml2::xml_add_child(div, "img", src = image)
  }
  add_card(rows[[1]], 9, "red-card.png")
  add_card(rows[[2]], 8, "yellow-card.png")
  add_card(rows[[2]], 9, "red-card.png")
  add_card(rows[[3]], 8, "yellow-card.png")
  page <- parse_player_page(as.character(html), 55425)

  expect_equal(page$games$dismissal[1:3], c("direct red", "second yellow", NA))
  expect_true(all(is.na(page$games$dismissal_minute)))
})

test_that("team results parsing returns played games with ids", {
  games <- parse_team_results(fixture("team_results.html"))

  expect_true("960442" %in% games$game_id)
  expect_true(all(c("home_id", "away_id", "date", "comp", "played") %in% names(games)))
  expect_equal(games$date[games$game_id == "960442"], as.Date("2026-08-01"))
})

test_that("weekly games are classified per player", {
  page <- list(
    player_id = 55425, club_id = 2598, club = "Tranmere", joined = as.Date(NA),
    games = data.frame(
      game_id = "960442", goals = 1, stringsAsFactors = FALSE
    )
  )
  team_games <- data.frame(
    game_id = c("960442", "2"),
    date = as.Date(c("2026-08-01", "2026-08-02")),
    comp = c("English League Cup", "English League Cup"),
    home_id = c(2598, 2598), away_id = c(2175, 3),
    home = "Tranmere", away = c("Rochdale", "Other"),
    played = TRUE,
    stringsAsFactors = FALSE
  )
  lineups <- list(
    "960442" = parse_lineup_bubble(fixture("lineup_bubble.html")),
    "2" = data.frame(player_id = "1", status = "Started", minutes = 90, side = "home")
  )

  out <- classify_player_games(
    page, team_games, lineups, as.Date("2026-07-30"), as.Date("2026-08-05")
  )

  expect_equal(out$status, c("Substitute", "Not in squad"))
  expect_equal(out$minutes, c(29, NA))
  expect_equal(out$goals, c(1, 0))
})

test_that("dismissals override substitution minutes and retain match details", {
  page <- list(
    player_id = 55425, club_id = 2598, club = "Tranmere", joined = as.Date(NA),
    games = data.frame(
      game_id = c("960442", "2"), goals = c(1, 0),
      dismissal = c("direct red", "second yellow"),
      dismissal_minute = c(NA_real_, 67)
    )
  )
  team_games <- data.frame(
    game_id = c("960442", "2"),
    date = as.Date(c("2026-08-01", "2026-08-02")),
    comp = "English League Cup", home_id = 2598, away_id = c(2175, 3),
    home = "Tranmere", away = c("Rochdale", "Other"), played = TRUE
  )
  entry <- data.frame(player_id = "55425", status = "Started", minutes = 90, side = "home")
  lineups <- list("960442" = entry, "2" = transform(entry, dismissal_minute = 65))
  games <- classify_player_games(page, team_games, lineups, as.Date("2026-07-30"), as.Date("2026-08-05"))

  expect_equal(games$minutes, c(NA_real_, 65))
  expect_equal(games$dismissal, c("direct red", "second yellow"))
  expect_equal(games$goals, c(1, 0))
  expect_equal(games$dismissal_minute, c(NA_real_, 65))
  lineups[["2"]] <- transform(entry, status = "Substitute", entry_minute = 61, dismissal_minute = 65)
  games <- classify_player_games(page, team_games, lineups, as.Date("2026-07-30"), as.Date("2026-08-05"))
  expect_equal(games$minutes[2], 4)
  expect_equal(games$dismissal_minute[2], 65)
  lineups[["2"]]$entry_minute <- 70
  games <- classify_player_games(page, team_games, lineups, as.Date("2026-07-30"), as.Date("2026-08-05"))
  expect_equal(games$minutes[2], 0)
  lineups[["2"]]$entry_minute <- NA_real_
  games <- classify_player_games(page, team_games, lineups, as.Date("2026-07-30"), as.Date("2026-08-05"))
  expect_true(is.na(games$minutes[2]))
  page$games$dismissal[2] <- NA_character_
  games <- classify_player_games(page, team_games, lineups, as.Date("2026-07-30"), as.Date("2026-08-05"))
  expect_equal(games$minutes[2], 90)
  expect_true(is.na(games$dismissal_minute[2]))
})

test_that("weekly report pipeline carries a dismissal through to both emails", {
  player_html <- xml2::read_html(fixture("player_page.html"))
  row <- rvest::html_element(player_html, "#tpg960442")
  red <- rvest::html_element(rvest::html_elements(row, "td")[[9]], "div")
  yellow <- rvest::html_element(rvest::html_elements(row, "td")[[8]], "div")
  xml2::xml_add_child(red, "img", src = "red-card.png")
  xml2::xml_add_child(yellow, "img", src = "yellow-card.png")
  team_html <- fixture("team_results.html")
  lineup_html <- sub(
    "T Ince</a>, 61", "T Ince</a>, 61 <img src='red.png' /> (s/o 65)",
    fixture("lineup_bubble.html"), fixed = TRUE
  )
  root <- normalizePath(file.path("..", ".."))
  withr::local_dir(root)
  env <- new.env(parent = globalenv())
  sys.source("R/wengerboys-weekly.R", envir = env)
  env$sb_get_html <- function(path, ...) {
    if (grepl("/players/", path, fixed = TRUE)) return(as.character(player_html))
    if (grepl("lineup_bubble", path, fixed = TRUE)) return(lineup_html)
    team_html
  }
  roster <- data.frame(player_id = 55425, player = "PLAYER", position = "MIDFIELDER", club = "Tranmere")
  window <- list(start = as.Date("2026-07-30"), end = as.Date("2026-08-05"))
  reports <- NULL
  invisible(capture.output(reports <- env$wb_player_reports(
    roster, data.frame(team_id = 2598), window, "159"
  )))
  email <- build_report_email(reports, data.frame(), window)

  expect_equal(reports[[1]]$games$dismissal, "second yellow")
  expect_equal(reports[[1]]$games$minutes, 4)
  expect_equal(reports[[1]]$minutes, 4L)
  expect_equal(reports[[1]]$games_played, 1L)
  expect_match(email$html, "</table><p>Note: Dismissed against Rochdale", fixed = TRUE)
  expect_match(email$text, "NOTE: Dismissed against Rochdale", fixed = TRUE)
  expect_match(email$text, "second yellow at 65 minutes", fixed = TRUE)
})

test_that("club flags compare sheet club fuzzily and warn about leaving the 92", {
  league92 <- data.frame(team_id = c(1, 2))
  page <- list(club_id = 2, club = "Tranmere Rovers", joined = as.Date("2026-01-01"))

  expect_null(club_flags(page, "TRANMERE", league92)$note)
  expect_null(club_flags(page, "Tranmere Rovers FC", league92)$note)
  expect_null(club_flags(page, NA, league92)$note)
  expect_match(club_flags(page, "Wrexham", league92)$note, "new club")

  away <- list(club_id = 9, club = "Away FC", joined = as.Date(NA))
  both <- club_flags(away, "Wrexham", league92)
  expect_match(both$note, "new club")
  expect_match(both$warning, "outside the Football League 92")
  expect_match(club_flags(away, "Away", league92)$warning, "outside")
  expect_null(club_flags(away, "Away", league92)$note)
})

test_that("scouting picks the quota per position", {
  candidates <- data.frame(
    player = letters[1:12],
    club = "X",
    position = rep(c("DEFENDER", "MIDFIELDER", "FORWARD", "GOALKEEPER"), 3),
    goals = 1:12,
    apps = 5,
    stringsAsFactors = FALSE
  )
  picked <- scout_best(candidates, c(DEFENDER = 2, MIDFIELDER = 1, FORWARD = 1))

  expect_equal(nrow(picked), 4)
  expect_equal(picked$goals[picked$position == "DEFENDER"], c(9, 5))
})

test_that("league 92 file lists 92 clubs", {
  clubs <- read_league92(file.path("..", "..", "data", "league92_clubs.csv"))

  expect_equal(nrow(clubs), 92)
  expect_null(league92_warning(clubs))
})

test_that("report email includes warnings and scouting", {
  reports <- list(list(
    player = "A PLAYER", position = "FORWARD", club = "Away FC",
    games = data.frame(
      date = as.Date("2026-10-28"), opponent = "Foo", venue = "H", comp = "Premier League",
      status = "Unused substitute", minutes = 0, goals = 0
    ),
    goals = 0L, games_played = 0L, minutes = 0L, missed = 1L,
    warning = "Now at Away FC, which is outside the Football League 92.", note = NULL
  ))
  scouting <- data.frame(
    position = "FORWARD", player = "B", club = "C", goals = 4, apps = 3
  )
  email <- build_report_email(reports, scouting, wb_window(as.Date("2026-10-29")))

  expect_match(email$html, "outside the Football League 92")
  expect_match(email$text, "forward: B \\(C\\)")
  expect_false(grepl("Dismissed", email$html, fixed = TRUE))
  expect_false(grepl("Dismissed", email$text, fixed = TRUE))
})

test_that("dismissal notes follow the match table in both report formats", {
  games <- data.frame(
    date = as.Date(c("2026-10-28", "2026-10-29")), opponent = c("Foo & Bar", "Baz"),
    venue = c("H", "A"), comp = "Premier League", status = "Started",
    minutes = c(NA_real_, 67), goals = c(1, 0),
    dismissal = c("direct red", "second yellow"), dismissal_minute = c(NA_real_, 67)
  )
  report <- list(
    player = "A PLAYER", position = "FORWARD", club = "Club", games = games,
    goals = 1L, games_played = 2L, minutes = 67L, missed = 0L,
    warning = NULL, note = NULL
  )
  email <- build_report_email(list(report), data.frame(), wb_window(as.Date("2026-10-29")))

  expect_match(email$html, "</table><p>Note: Dismissed against Foo &amp; Bar", fixed = TRUE)
  expect_match(email$html, "second yellow at 67 minutes", fixed = TRUE)
  expect_match(email$html, "></td><td style='padding:4px 8px;border-bottom:1px solid #ddd'>1</td>", fixed = TRUE)
  expect_match(email$text, "Started |  | 1\n  Thu 29 Oct", fixed = TRUE)
  expect_match(email$text, "Started | 67 | 0\n  NOTE: Dismissed against Foo & Bar", fixed = TRUE)
  expect_match(email$text, "second yellow at 67 minutes", fixed = TRUE)
})
