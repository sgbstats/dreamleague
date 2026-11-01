source(file.path("..", "..", "R", "wengerboys-lib.R"))

fixture <- function(name) {
  paste(readLines(file.path("fixtures", name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("line-up parsing derives starters, substitutes and minutes", {
  lineup <- parse_lineup_bubble(fixture("lineup_bubble.html"))
  row <- function(id) lineup[lineup$player_id == id, ]

  expect_equal(row("134599")$status, "Started")
  expect_equal(row("134599")$minutes, 90)
  expect_equal(row("72740")$minutes, 61)
  expect_equal(row("55425")$status, "Substitute")
  expect_equal(row("55425")$minutes, 29)
  expect_equal(row("99454")$minutes, 72)
  expect_equal(row("165853")$minutes, 17)
  expect_equal(row("222266")$minutes, 1)
  expect_equal(row("201473")$status, "Unused substitute")
  expect_equal(row("201473")$side, "home")
  expect_equal(row("157662")$side, "away")
  expect_equal(nrow(row("1")), 0)
})

test_that("player page parsing reads club, position and goals", {
  page <- parse_player_page(fixture("player_page.html"), 55425)

  expect_equal(page$position, "MIDFIELDER")
  expect_equal(page$club, "Tranmere Rovers")
  expect_equal(page$club_id, 2598)
  expect_equal(nrow(page$games), 14)
  expect_equal(sum(page$games$goals), 2)
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
})
