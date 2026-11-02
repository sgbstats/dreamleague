test_that("player report section runs live and prints matches and club mismatches", {
  skip_if(nzchar(Sys.getenv("WENGERBOYS_SKIP_LIVE")), "WENGERBOYS_SKIP_LIVE is set")

  root <- normalizePath(file.path("..", ".."))
  withr::local_dir(root)
  source("R/wengerboys-weekly.R", local = TRUE)

  probe <- sb_get_html("/players/player.sd?player_id=55425&season_id=159")
  skip_if(is.na(probe), "Soccerbase is not reachable")

  dl <- data.frame(
    team = "WENGER BOYS",
    player = c("TOM INCE", "JOE MURPHY"),
    club = c("TRANMERE", "WREXHAM"),
    position = c("MIDFIELDER", "GOALKEEPER"),
    url = c(
      "https://www.soccerbase.com/players/player.sd?player_id=55425&season_id=159",
      "https://www.soccerbase.com/players/player.sd?player_id=15110&season_id=159"
    ),
    sold2 = as.Date(NA),
    league = "didsbury",
    stringsAsFactors = FALSE
  )
  today <- as.Date("2026-10-29")
  roster <- wb_roster(dl, "WENGER BOYS", today)

  output <- capture.output(
    reports <- wb_player_reports(
      roster, read_league92(), wb_window(today), "159"
    )
  )
  cat(output, sep = "\n")

  expect_length(reports, 2)
  expect_true(any(grepl("sheet club: WREXHAM | Soccerbase club: Tranmere", output, fixed = TRUE)))
  expect_true(any(grepl("NOTE:", output, fixed = TRUE)))
  expect_null(reports[[1]]$note)
  expect_match(reports[[2]]$note, "new club")
  expect_equal(reports[[1]]$games_played, 1L)
  expect_equal(reports[[1]]$minutes, 90L)
  expect_equal(reports[[2]]$missed, 1L)
})
