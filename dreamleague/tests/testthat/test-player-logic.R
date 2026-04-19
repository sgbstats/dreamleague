test_that("current players filter excludes sold players", {
  current <- mock_dl |> filter(is.na(sold))
  sold    <- mock_dl |> filter(!is.na(sold))

  # ERIMUS sold JAMES WARD PROWSE in December
  expect_false("JAMES WARD PROWSE" %in% current$player)
  expect_true("JAMES WARD PROWSE" %in% sold$player)
})

test_that("current players filter includes transfer replacements", {
  current <- mock_dl |> filter(is.na(sold))
  # YOANE WISSA was signed as a transfer (cost = ""); should still be current
  expect_true("YOANE WISSA" %in% current$player)
})

test_that("current players count per position matches squad rules", {
  current <- mock_dl |>
    filter(is.na(sold)) |>
    count(team, position) |>
    pivot_wider(names_from = "position", values_from = "n", values_fill = 0)

  # Each team should have: 1 GK, 2 DEF, 3 MID, 5 FWD (after accounting for transfers)
  # ERIMUS sold 1 MID and bought 1 MID: still 3 MID total
  for (tm in unique(current$team)) {
    row <- current[current$team == tm, ]
    expect_equal(row$GOALKEEPER,  1, info = paste(tm, "GK count"))
    expect_equal(row$DEFENDER,    2, info = paste(tm, "DEF count"))
    expect_equal(row$MIDFIELDER,  3, info = paste(tm, "MID count"))
    expect_equal(row$FORWARD,     5, info = paste(tm, "FWD count"))
  }
})

test_that("diagnostics query flags teams with wrong squad composition", {
  # Make a broken team: add an extra forward
  extra_dl <- rbind(
    mock_dl,
    make_player("FORWARD", "EXTRA PLAYER", "ARSENAL", "5", 0, "01-Aug", NA,
                season_start, season_end, 5, 0, "WENGER BOYS", "didsbury")
  )
  broken <- extra_dl |>
    filter(is.na(sold)) |>
    dplyr::select(team, player, club, position) |>
    mutate(
      position = factor(
        position,
        c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"),
        ordered = TRUE
      )
    ) |>
    count(team, position) |>
    tidyr::pivot_wider(names_from = "position", values_from = "n", values_fill = 0) |>
    filter(GOALKEEPER != 1 | DEFENDER != 2 | MIDFIELDER != 3 | FORWARD != 5)

  expect_true("WENGER BOYS" %in% broken$team)
  # Other teams should not appear
  expect_false(any(c("NUTMEG UTD", "ERIMUS", "ALTERNATIVE ULSTERMEN") %in% broken$team))
})

test_that("diagnostics query returns empty when all teams are valid", {
  valid_check <- mock_dl |>
    filter(is.na(sold)) |>
    dplyr::select(team, player, club, position) |>
    mutate(
      position = factor(
        position,
        c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"),
        ordered = TRUE
      )
    ) |>
    count(team, position) |>
    tidyr::pivot_wider(names_from = "position", values_from = "n", values_fill = 0) |>
    filter(GOALKEEPER != 1 | DEFENDER != 2 | MIDFIELDER != 3 | FORWARD != 5)

  expect_equal(nrow(valid_check), 0)
})

test_that("players taken table returns correct columns", {
  result <- mock_dl |>
    filter(is.na(sold), league == "didsbury") |>
    dplyr::select(team, player, club, position) |>
    rename_with(stringr::str_to_title)

  expect_true(all(c("Team", "Player", "Club", "Position") %in% names(result)))
  expect_equal(ncol(result), 4)
})

test_that("players taken filtered by didsbury only shows didsbury teams", {
  result <- mock_dl |>
    filter(is.na(sold), league == "didsbury") |>
    dplyr::select(team, player, club, position)

  expect_true(all(result$team %in% mock_managers_d$team))
  expect_false(any(result$team %in% mock_managers_o$team))
})

test_that("players taken filtered by original only shows original teams", {
  result <- mock_dl |>
    filter(is.na(sold), league == "original") |>
    dplyr::select(team, player, club, position)

  expect_true(all(result$team %in% mock_managers_o$team))
  expect_false(any(result$team %in% mock_managers_d$team))
})

test_that("outfield transfer count is computed correctly", {
  # NUTMEG UTD has 3 outfield players with cost="" (MORGAN ROGERS, LYLE FOSTER, ALEX IWOBI)
  transfers_used <- mock_dl |>
    filter(team == "NUTMEG UTD", position != "GOALKEEPER", cost == "") |>
    nrow()
  expect_equal(transfers_used, 3)
  expect_equal(8 - transfers_used, 5)  # 5 remaining
})

test_that("goalkeeper transfer count is computed correctly", {
  # No GK has an empty cost in the mock data
  gk_transfers <- mock_dl |>
    filter(team == "WENGER BOYS", position == "GOALKEEPER", cost == "") |>
    nrow()
  expect_equal(gk_transfers, 0)
  expect_equal(2 - gk_transfers, 2)  # 2 remaining
})

test_that("team display only shows current player columns after selection", {
  input_team  <- "WENGER BOYS"
  input_current <- TRUE

  teams3 <- mock_dl |>
    filter(team == input_team) |>
    { if (input_current) filter(., is.na(sold)) else . }() |>
    select(-sold, -bought2, -sold2, -SBapp, -league)

  table_data <- teams3 |>
    select(-goals) |>
    select(-team) |>
    rename("Goals" = "SBgoals") |>
    rename_with(stringr::str_to_title) |>
    relocate(Goals, .after = Club)

  expected_cols <- c("Position", "Player", "Club", "Goals", "Cost", "Bought")
  expect_true(all(expected_cols %in% names(table_data)))
  # Should not contain internal columns
  expect_false("Sold" %in% names(table_data))
  expect_false("League" %in% names(table_data))
})

test_that("team display includes sold players when current=FALSE", {
  teams3_all <- mock_dl |>
    filter(team == "ERIMUS") |>
    select(-bought2, -sold2, -SBapp, -league)

  expect_true("JAMES WARD PROWSE" %in% teams3_all$player)
})

test_that("scoring history is empty for player with no goals", {
  history <- mock_daily |>
    filter(player == "JOHN STONES", team == "WENGER BOYS", SBgoals != 0) |>
    select(date = Date, goals = SBgoals)

  # JOHN STONES has no daily records in mock data
  expect_equal(nrow(history), 0)
})

test_that("scoring history contains correct records for goal scorers", {
  history <- mock_daily |>
    filter(player == "ERLING HAALAND", team == "WENGER BOYS", SBgoals != 0) |>
    select(date = Date, goals = SBgoals) |>
    arrange(desc(date))

  expect_true(nrow(history) > 0)
  expect_true(all(history$goals > 0))
  expect_true(all(!is.na(history$date)))
})

test_that("GK scoring history uses team-based filter not player name", {
  # GK rows have player = NA; history uses team+position filter
  gk_history <- mock_daily |>
    filter(team == "WENGER BOYS", SBgoals != 0, position == "GOALKEEPER") |>
    select(date = Date, goals = SBgoals) |>
    arrange(desc(date))

  expect_true(nrow(gk_history) > 0)
})
