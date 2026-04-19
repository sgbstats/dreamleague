# Helper: replicate the cup score computation from the server
compute_cup_results <- function(comp_val, round_val,
                                cupties_df = mock_cupties,
                                daily_df   = mock_daily,
                                managers_df = mock_managers) {
  date <- cupties_df |>
    filter(comp == comp_val, round == round_val) |>
    pull(date) |>
    min(na.rm = TRUE)

  weekend <- daily_df |>
    filter(
      Date >= date,
      Date <= date + lubridate::days(3),
      Date >= bought2,
      Date <= sold2
    )

  scorers <- weekend |>
    filter(SBgoals != 0) |>
    mutate(
      name = paste0(
        ifelse(position == "GOALKEEPER", club, sub(".*\\s", "", player)),
        if_else(SBgoals == 1, "", paste0(" (", SBgoals, ")"))
      ) |> stringr::str_to_title()
    ) |>
    summarise(scorers = paste(name, collapse = ", ", sep = ""), .by = "team")

  main <- managers_df |>
    merge(
      weekend |> summarise(total = sum(SBgoals, na.rm = TRUE), .by = "team"),
      by = "team", all.x = TRUE
    ) |>
    merge(
      weekend |>
        filter(position != "GOALKEEPER") |>
        summarise(gf = sum(SBgoals), .by = "team"),
      by = "team", all.x = TRUE
    ) |>
    merge(
      weekend |>
        filter(position == "GOALKEEPER") |>
        summarise(ga = -sum(SBgoals), .by = "team"),
      by = "team", all.x = TRUE
    ) |>
    merge(scorers, by = "team", all.x = TRUE) |>
    mutate(
      ga     = replace(ga,     is.na(ga),     0),
      total  = replace(total,  is.na(total),  0),
      gf     = replace(gf,     is.na(gf),     0),
      scorers = replace(scorers, is.na(scorers), "")
    ) |>
    arrange(-total, -gf) |>
    mutate(
      team_manager = paste0(team, " (", manager, ")"),
      score        = paste0(total, " (", gf, "-", ga, ")")
    ) |>
    dplyr::select(team, team_manager, total, gf, score, scorers)

  cupties_df |>
    mutate(rn = row_number()) |>
    filter(comp == comp_val, round == round_val) |>
    merge(main, by.x = "team1", by.y = "team") |>
    merge(main, by.x = "team2", by.y = "team") |>
    mutate(
      winner = case_when(
        total.x > total.y ~ 1,
        total.x < total.y ~ 2,
        gf.x    > gf.y    ~ 1,
        gf.x    < gf.y    ~ 2
      )
    ) |>
    arrange(rn)
}

# ---------------------------------------------------------------------------

test_that("cup result has expected columns", {
  res <- compute_cup_results("didsbury", "R1")
  expect_true(all(c("team1", "team2", "winner", "total.x", "total.y", "gf.x", "gf.y") %in% names(res)))
})

test_that("WENGER BOYS beats NUTMEG UTD in didsbury R1", {
  res <- compute_cup_results("didsbury", "R1")
  tie <- res[res$team1 == "WENGER BOYS" & res$team2 == "NUTMEG UTD", ]
  # GW2 (2025-09-13): WB scored 3+1=4 gf, -1 gk → total=3
  # NU scored 1+1=2 gf, -3 gk → total=-1
  expect_equal(tie$winner, 1)
  expect_true(tie$total.x > tie$total.y)
})

test_that("team with higher gf wins when totals are tied", {
  # GW3 (2026-01-17): KX scored 1 outfield, -1 gk → total=0
  #                   SM scored 0 outfield, -1 gk → total=-1
  # Actually totals differ here. Let's build a synthetic tie.
  tied_dl <- data.frame(
    Date     = as.Date(c("2025-09-13", "2025-09-13", "2025-09-13", "2025-09-13")),
    SBgoals  = c(2, -2, 3, -3),   # total both 0, but gf differs
    position = c("FORWARD", "GOALKEEPER", "FORWARD", "GOALKEEPER"),
    player   = c("PLAYER A", NA, "PLAYER B", NA),
    club     = c("CLUB A", "CLUB X", "CLUB B", "CLUB Y"),
    team     = c("TEAM A", "TEAM A", "TEAM B", "TEAM B"),
    league   = c("test", "test", "test", "test"),
    bought2  = as.Date(c("2025-07-28", "2025-07-28", "2025-07-28", "2025-07-28")),
    sold2    = as.Date(c("2026-06-30", "2026-06-30", "2026-06-30", "2026-06-30")),
    stringsAsFactors = FALSE
  )
  tied_managers <- data.frame(
    manager = c("MGR A", "MGR B"),
    team    = c("TEAM A", "TEAM B"),
    league  = c("test", "test"),
    stringsAsFactors = FALSE
  )
  tied_cupties <- data.frame(
    comp  = "test",
    round = "R1",
    team1 = "TEAM A",
    team2 = "TEAM B",
    date  = as.Date("2025-09-12"),
    stringsAsFactors = FALSE
  )
  res <- compute_cup_results("test", "R1", tied_cupties, tied_dl, tied_managers)
  # TEAM A: total=0, gf=2; TEAM B: total=0, gf=3 → TEAM B wins (winner=2)
  expect_equal(res$winner, 2)
})

test_that("cup score string format is 'total (gf-ga)'", {
  res <- compute_cup_results("didsbury", "R1")
  score_x <- res$score.x
  # Should match pattern like "3 (4--1)" or "3 (4-1)"
  expect_match(score_x, "^-?\\d+ \\(-?\\d+-\\d+\\)$")
})

test_that("scorer string uses last name for outfield, club for GK", {
  res <- compute_cup_results("didsbury", "R1")
  scorers_wb <- res$scorers.x[res$team1 == "WENGER BOYS"]
  # ERLING HAALAND scored 3 → "Haaland (3)"
  # MARTIN ODEGAARD scored 1 → "Odegaard"
  # GK (ARSENAL) conceded 1 → "-Arsenal" (negative, so won't appear as scorer)
  expect_match(scorers_wb, "Haaland")
  expect_match(scorers_wb, "Odegaard")
  # Multiple goals should show count in brackets
  expect_match(scorers_wb, "Haaland \\(3\\)")
})

test_that("GK scorer uses club name not player name", {
  # In the mock data, NUTMEG UTD GK (CHELSEA) conceded 3 → SBgoals=-3
  # Negative SBgoals means GK is a scorer in the negative sense;
  # but the scorer string only includes SBgoals != 0, and for a GK
  # the scorer label uses the club name.
  # Build a scenario where GK has positive contribution (clean sheet bonus).
  gk_dl <- data.frame(
    Date     = as.Date("2025-09-13"),
    SBgoals  = 1,   # positive means clean-sheet bonus or similar
    position = "GOALKEEPER",
    player   = NA_character_,
    club     = "ARSENAL",
    team     = "TEST TEAM",
    league   = "test",
    bought2  = as.Date("2025-07-28"),
    sold2    = as.Date("2026-06-30"),
    stringsAsFactors = FALSE
  )
  scorers <- gk_dl |>
    filter(SBgoals != 0) |>
    mutate(
      name = paste0(
        ifelse(position == "GOALKEEPER", club, sub(".*\\s", "", player)),
        if_else(SBgoals == 1, "", paste0(" (", SBgoals, ")"))
      ) |> stringr::str_to_title()
    ) |>
    summarise(scorers = paste(name, collapse = ", "), .by = "team")
  expect_equal(scorers$scorers, "Arsenal")
})

test_that("single-goal scorer has no count suffix", {
  # MARTIN ODEGAARD scored 1 goal in GW2
  gw2_wb <- mock_daily |>
    filter(
      team == "WENGER BOYS", Date == as.Date("2025-09-13"),
      player == "MARTIN ODEGAARD"
    )
  scorer_str <- gw2_wb |>
    filter(SBgoals != 0) |>
    mutate(
      name = paste0(
        ifelse(position == "GOALKEEPER", club, sub(".*\\s", "", player)),
        if_else(SBgoals == 1, "", paste0(" (", SBgoals, ")"))
      ) |> stringr::str_to_title()
    ) |>
    pull(name)
  expect_equal(scorer_str, "Odegaard")
  expect_false(grepl("\\(", scorer_str))
})

test_that("multi-goal scorer has count suffix", {
  # ERLING HAALAND scored 3 in GW2
  gw2_haaland <- mock_daily |>
    filter(
      team == "WENGER BOYS", Date == as.Date("2025-09-13"),
      player == "ERLING HAALAND"
    )
  scorer_str <- gw2_haaland |>
    filter(SBgoals != 0) |>
    mutate(
      name = paste0(
        ifelse(position == "GOALKEEPER", club, sub(".*\\s", "", player)),
        if_else(SBgoals == 1, "", paste0(" (", SBgoals, ")"))
      ) |> stringr::str_to_title()
    ) |>
    pull(name)
  expect_equal(scorer_str, "Haaland (3)")
})

test_that("winner is NA when scores are completely equal", {
  equal_dl <- data.frame(
    Date     = as.Date(c("2025-09-13", "2025-09-13")),
    SBgoals  = c(1, 1),
    position = c("FORWARD", "FORWARD"),
    player   = c("PLAYER A", "PLAYER B"),
    club     = c("CLUB A", "CLUB B"),
    team     = c("TEAM A", "TEAM B"),
    league   = c("test", "test"),
    bought2  = as.Date(c("2025-07-28", "2025-07-28")),
    sold2    = as.Date(c("2026-06-30", "2026-06-30")),
    stringsAsFactors = FALSE
  )
  equal_managers <- data.frame(
    manager = c("MGR A", "MGR B"), team = c("TEAM A", "TEAM B"),
    league = "test", stringsAsFactors = FALSE
  )
  equal_cup <- data.frame(
    comp = "test", round = "R1",
    team1 = "TEAM A", team2 = "TEAM B",
    date = as.Date("2025-09-12"), stringsAsFactors = FALSE
  )
  res <- compute_cup_results("test", "R1", equal_cup, equal_dl, equal_managers)
  expect_true(is.na(res$winner))
})

test_that("cup results filter correctly by competition", {
  d_res <- compute_cup_results("didsbury", "R1")
  o_res <- compute_cup_results("original", "R1")

  # Didsbury R1 should be between didsbury teams only
  didsbury_teams <- mock_managers_d$team
  expect_true(all(d_res$team1 %in% didsbury_teams | d_res$team2 %in% didsbury_teams))

  # Original R1 should be between original teams only
  original_teams <- mock_managers_o$team
  expect_true(all(o_res$team1 %in% original_teams | o_res$team2 %in% original_teams))
})

test_that("cup results filter correctly by round", {
  r1_res <- compute_cup_results("didsbury", "R1")
  sf_res <- compute_cup_results("didsbury", "SF")

  # R1: WENGER BOYS vs NUTMEG UTD
  expect_true(any(r1_res$team1 == "WENGER BOYS" & r1_res$team2 == "NUTMEG UTD"))
  # SF: ERIMUS vs ALTERNATIVE ULSTERMEN
  expect_true(any(sf_res$team1 == "ERIMUS" & sf_res$team2 == "ALTERNATIVE ULSTERMEN"))
})
