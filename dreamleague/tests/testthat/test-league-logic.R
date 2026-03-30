test_that("league table has required columns", {
  result <- expected_league
  expect_true(all(c("team", "manager", "league", "total", "gf", "ga", "rank") %in% names(result)))
})

test_that("league total equals sum of all SBgoals for each team", {
  for (tm in unique(mock_dl$team)) {
    expected_total <- sum(mock_dl$SBgoals[mock_dl$team == tm], na.rm = TRUE)
    actual_total   <- expected_league$total[expected_league$team == tm]
    expect_equal(actual_total, expected_total,
                 info = paste("team:", tm))
  }
})

test_that("league gf equals sum of outfield SBgoals", {
  for (tm in unique(mock_dl$team)) {
    expected_gf <- sum(
      mock_dl$SBgoals[mock_dl$team == tm & mock_dl$position != "GOALKEEPER"],
      na.rm = TRUE
    )
    actual_gf <- expected_league$gf[expected_league$team == tm]
    expect_equal(actual_gf, expected_gf, info = paste("team:", tm))
  }
})

test_that("league ga equals goals conceded (negation of GK SBgoals)", {
  for (tm in unique(mock_dl$team)) {
    gk_goals    <- sum(
      mock_dl$SBgoals[mock_dl$team == tm & mock_dl$position == "GOALKEEPER"],
      na.rm = TRUE
    )
    expected_ga <- -gk_goals
    actual_ga   <- expected_league$ga[expected_league$team == tm]
    expect_equal(actual_ga, expected_ga, info = paste("team:", tm))
  }
})

test_that("total equals gf minus ga for each team", {
  for (tm in unique(mock_dl$team)) {
    row <- expected_league[expected_league$team == tm, ]
    expect_equal(row$total, row$gf - row$ga, info = paste("team:", tm))
  }
})

test_that("ranking within each league is 1-based and sequential", {
  for (lg in c("didsbury", "original")) {
    ranks <- sort(expected_league$rank[expected_league$league == lg])
    expect_equal(ranks, seq_along(ranks))
  }
})

test_that("rank 1 in didsbury league has highest total", {
  d_table <- expected_league |>
    filter(league == "didsbury") |>
    arrange(rank)

  expect_equal(d_table$rank[1], 1L)
  expect_true(all(d_table$total[1] >= d_table$total))
})

test_that("rank 1 in original league has highest total", {
  o_table <- expected_league |>
    filter(league == "original") |>
    arrange(rank)

  expect_equal(o_table$rank[1], 1L)
  expect_true(all(o_table$total[1] >= o_table$total))
})

test_that("filtering league table by 'didsbury' returns only didsbury teams", {
  d_table <- expected_league |> filter(league == "didsbury")
  expect_true(all(d_table$team %in% mock_managers_d$team))
  expect_false(any(d_table$team %in% mock_managers_o$team))
})

test_that("filtering league table by 'original' returns only original teams", {
  o_table <- expected_league |> filter(league == "original")
  expect_true(all(o_table$team %in% mock_managers_o$team))
  expect_false(any(o_table$team %in% mock_managers_d$team))
})

test_that("tie-breaking uses gf descending when totals are equal", {
  # Build two synthetic teams with equal totals but different gf
  tie_managers <- data.frame(
    manager = c("A", "B"),
    team    = c("TEAM A", "TEAM B"),
    league  = c("test", "test"),
    stringsAsFactors = FALSE
  )
  tie_dl <- data.frame(
    team     = c("TEAM A", "TEAM A", "TEAM B", "TEAM B"),
    position = c("GOALKEEPER", "FORWARD", "GOALKEEPER", "FORWARD"),
    SBgoals  = c(-2, 5, -1, 4),  # A: total=3 gf=5, B: total=3 gf=4
    stringsAsFactors = FALSE
  )
  tie_league <- tie_managers |>
    merge(
      tie_dl |> group_by(team) |> summarise(total = sum(SBgoals, na.rm = TRUE), .groups = "drop"),
      by = "team", all = TRUE
    ) |>
    merge(
      tie_dl |> filter(position != "GOALKEEPER") |>
        group_by(team) |> summarise(gf = sum(SBgoals, na.rm = TRUE), .groups = "drop"),
      by = "team", all = TRUE
    ) |>
    merge(
      tie_dl |> filter(position == "GOALKEEPER") |>
        group_by(team) |> summarise(ga = -sum(SBgoals, na.rm = TRUE), .groups = "drop"),
      by = "team", all = TRUE
    ) |>
    arrange(-total, -gf) |>
    mutate(rank = row_number(), .by = "league")

  expect_equal(tie_league$rank[tie_league$team == "TEAM A"], 1L)
  expect_equal(tie_league$rank[tie_league$team == "TEAM B"], 2L)
})

test_that("teams with no goals have expected zero/negative values", {
  au_row <- expected_league[expected_league$team == "ALTERNATIVE ULSTERMEN", ]
  expect_equal(au_row$gf, 0)
  expect_true(au_row$ga > 0)   # goals against is positive
  expect_true(au_row$total < 0)
})

test_that("WENGER BOYS ranks first in didsbury league", {
  d_table <- expected_league |> filter(league == "didsbury")
  expect_equal(d_table$team[d_table$rank == 1], "WENGER BOYS")
})

test_that("PHOENIX FLYERS ranks first in original league", {
  o_table <- expected_league |> filter(league == "original")
  expect_equal(o_table$team[o_table$rank == 1], "PHOENIX FLYERS")
})
