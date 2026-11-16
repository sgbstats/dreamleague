root <- normalizePath(file.path("..", ".."))
withr::local_dir(root)

auction_env <- new.env(parent = globalenv())
sys.source("R/didsbury-auction-report.R", envir = auction_env)

auction_players <- function() {
  data.frame(
    league = "didsbury",
    position = c(
      "GOALKEEPER", "GOALKEEPER",
      rep("DEFENDER", 2),
      rep("MIDFIELDER", 3),
      rep("FORWARD", 6),
      "FORWARD", "FORWARD"
    ),
    player = c(
      "KEEPER A", "KEEPER B",
      "DEFENDER A", "DEFENDER B",
      "MID A", "MID B", "MID C",
      "FORWARD A", "FORWARD B", "FORWARD C", "FORWARD D", "FORWARD E",
      "FORWARD F", "TRANSFER", "NO COST"
    ),
    club = "CLUB",
    cost = c(
      10, 30, rep(5, 2), rep(5, 3), rep(5, 5), 45, 1, NA
    ),
    is_transfer = c(rep(FALSE, 13), TRUE, FALSE),
    SBgoals = c(
      0, -100, rep(0, 10), 30, 999, 999
    ),
    sold = c(as.Date("2026-08-10"), rep(as.Date(NA), 14)),
    url = c(
      "/teams/team.sd?team_id=10", "/teams/team.sd?team_id=11",
      rep("/teams/team.sd?team_id=20", 13)
    ),
    stringsAsFactors = FALSE
  )
}

test_that("Didsbury selector maximizes goals within budget and ignores transfers", {
  result <- auction_env$didsbury_auction_best_team(
    auction_players(),
    data.frame(team_id = c(10, 11), conceded = c(5, 0)),
    budget = 100
  )

  expect_equal(result$total_cost, 100)
  expect_equal(result$net_goals, 25)
  expect_equal(result$goalkeeper_conceded, 5)
  expect_true("KEEPER A" %in% result$players$player)
  expect_true("FORWARD F" %in% result$players$player)
  expect_false(any(c("TRANSFER", "NO COST") %in% result$players$player))
  expect_equal(
    unname(as.integer(table(factor(
      result$players$position,
      levels = names(auction_env$didsbury_auction_quotas)
    )))),
    c(1L, 2L, 3L, 5L)
  )
  report <- auction_env$didsbury_auction_html(result)
  expect_match(report, "Cost: &#163;100 / &#163;100", fixed = TRUE)
  expect_match(report, "FORWARD F", fixed = TRUE)
  expect_match(report, "full season to date", fixed = TRUE)
})

test_that("goalkeeper selection uses full-season concessions after sale", {
  auction_env$sb_get_html <- function(path, ...) {
    expect_match(path, "team_id=2598", fixed = TRUE)
    paste(
      readLines(
        "tests/testthat/fixtures/team_results.html",
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
  }
  dl <- data.frame(
    league = "didsbury",
    position = "GOALKEEPER",
    cost = 1,
    is_transfer = FALSE,
    url = "https://www.soccerbase.com/teams/team.sd?team_id=2598",
    stringsAsFactors = FALSE
  )

  conceded <- auction_env$didsbury_auction_goalkeeper_conceded(
    dl,
    today = as.Date("2026-08-03")
  )

  expect_equal(conceded$conceded, 2)
})

test_that("team result parsing exposes the full-time score", {
  html <- paste(
    readLines(
      "tests/testthat/fixtures/team_results.html",
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
  games <- auction_env$parse_team_results(html)

  expect_equal(games$home_goals[games$game_id == "960442"], 2)
  expect_equal(games$away_goals[games$game_id == "960442"], 2)
})
