source(file.path("..", "..", "R", "validate-dreamleague-data.R"))

bundle_under_test <- getOption("dreamleague.test_bundle", NULL)

make_managers <- function() {
  data.frame(
    manager = c("A", "B"),
    team = c("TEAM A", "TEAM B"),
    league = c("didsbury", "original"),
    stringsAsFactors = FALSE
  )
}

make_dl <- function() {
  data.frame(
    team = c(rep("TEAM A", 11), rep("TEAM B", 11)),
    player = c(paste("Player", 1:22)),
    club = c(paste("Club", 1:22)),
    position = c(
      "GOALKEEPER",
      rep("DEFENDER", 2),
      rep("MIDFIELDER", 3),
      rep("FORWARD", 5),
      "GOALKEEPER",
      rep("DEFENDER", 2),
      rep("MIDFIELDER", 3),
      rep("FORWARD", 5)
    ),
    cost = as.character(rep(5, 22)),
    goals = rep(0, 22),
    sold = rep("", 22),
    bought = rep("01-Jul", 22),
    SBgoals = rep(0, 22),
    SBapp = rep(0, 22),
    url = rep("https://example.com", 22),
    is_transfer = rep(FALSE, 22),
    bought2 = as.Date(rep("2026-07-01", 22)),
    sold2 = as.Date(rep("2027-06-30", 22)),
    league = c(rep("didsbury", 11), rep("original", 11)),
    stringsAsFactors = FALSE
  )
}

make_daily <- function(dl) {
  data.frame(
    Date = as.Date(rep("2026-08-01", nrow(dl))),
    App = rep(1, nrow(dl)),
    team = dl$team,
    position = dl$position,
    player = dl$player,
    club = dl$club,
    cost = dl$cost,
    goals = dl$goals,
    sold = dl$sold,
    bought = dl$bought,
    SBapp = rep(1, nrow(dl)),
    url = dl$url,
    is_transfer = dl$is_transfer,
    bought2 = dl$bought2,
    sold2 = dl$sold2,
    SBgoals = rep(0, nrow(dl)),
    league = dl$league,
    stringsAsFactors = FALSE
  )
}

make_time <- function() {
  list(
    update_time = as.POSIXct("2026-08-24 11:30:00", tz = "UTC"),
    mod_d = as.POSIXct("2026-08-24 11:00:00", tz = "UTC"),
    mod_o = as.POSIXct("2026-08-24 11:30:00", tz = "UTC")
  )
}

make_cupties <- function() {
  data.frame(
    comp = c("didsbury", "original"),
    round = c("SF", "SF"),
    team1 = c("TEAM A", "TEAM B"),
    team2 = c("TEAM B", "TEAM A"),
    date = as.Date(c("2026-08-10", "2026-08-17")),
    stringsAsFactors = FALSE
  )
}

test_that("valid app bundle passes shape validation", {
  managers <- make_managers()
  dl <- make_dl()
  daily <- make_daily(dl)
  time <- make_time()
  cupties <- make_cupties()

  expect_invisible(validate_dreamleague_bundle(
    dl,
    daily,
    time,
    cupties,
    managers
  ))
})

test_that("missing required dl column fails", {
  managers <- make_managers()
  dl <- make_dl()[setdiff(names(make_dl()), "league")]
  daily <- make_daily(make_dl())
  time <- make_time()
  cupties <- make_cupties()

  expect_error(
    validate_dreamleague_bundle(dl, daily, time, cupties, managers),
    "dl is missing required columns: league"
  )
})

test_that("invalid squad shape fails", {
  managers <- make_managers()
  dl <- make_dl()[-2, ]
  daily <- make_daily(dl)
  time <- make_time()
  cupties <- make_cupties()

  expect_error(
    validate_dreamleague_bundle(dl, daily, time, cupties, managers),
    "Current squad shape is invalid"
  )
})

test_that("unknown cup team fails", {
  managers <- make_managers()
  dl <- make_dl()
  daily <- make_daily(dl)
  time <- make_time()
  cupties <- make_cupties()
  cupties$team2[1] <- "MISSING TEAM"

  expect_error(
    validate_dreamleague_bundle(dl, daily, time, cupties, managers),
    "cupties reference unknown teams"
  )
})

test_that("BFL R1 starts on 18 September 2026", {
  bfl_r1 <- read.csv(testthat::test_path("..", "..", "data", "cupties.csv")) |>
    dplyr::filter(comp == "bfl", round == "R1")

  expect_gt(nrow(bfl_r1), 0)
  expect_true(all(bfl_r1$date == "18/09/2026"))
})

test_that("preprocessing bundle passes validation when provided", {
  skip_if(is.null(bundle_under_test), "No preprocessing bundle supplied")

  expect_invisible(
    validate_dreamleague_bundle(
      bundle_under_test$dl,
      bundle_under_test$daily,
      bundle_under_test$time,
      bundle_under_test$cupties,
      bundle_under_test$managers
    )
  )
})
