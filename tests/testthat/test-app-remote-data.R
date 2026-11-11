suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(shinyWidgets)
  library(reactable)
  library(shinydashboard)
})

remote_app_functions <- function() {
  env <- new.env(parent = globalenv())
  expressions <- parse(file.path("..", "..", "dreamleague", "app.R"))
  for (expression in expressions) {
    if (
      is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      is.call(expression[[3]]) && identical(expression[[3]][[1]], as.name("function"))
    ) {
      eval(expression, env)
    }
  }
  env$app_env <- env
  env$no_data_message <- "No data has been retrieved. Please try again later."
  env$initial_remote_load <- list(status = "failed", source = "No data retrieved", error = "offline")
  env$remote_data_state <- reactiveVal(env$initial_remote_load)
  env$clear_runtime_objects()
  sys.source(file.path("..", "..", "dreamleague", "data-export.R"), env)
  sys.source(file.path("..", "..", "dreamleague", "supabase-storage.R"), env)
  env
}

synthetic_app_bundle <- function() {
  dl <- data.frame(
    team = "TEST TEAM",
    player = c(NA, "TEST DEFENDER", "TEST MIDFIELDER", "TEST FORWARD"),
    club = "TEST CLUB",
    position = c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"),
    SBgoals = c(-1, 1, 2, 3), league = "didsbury", sold = NA_character_,
    bought = "2026-07-01", bought2 = as.Date("2026-07-01"),
    sold2 = as.Date("2027-07-01"), SBapp = 1, is_transfer = FALSE,
    goals = 1, cost = 1, url = ""
  )
  list(
    dl = dl,
    daily = transform(dl, Date = as.Date("2026-11-01"), App = 1),
    time = list(
      update_time = as.POSIXct("2026-11-01", tz = "UTC"),
      mod_d = as.POSIXct("2026-10-30", tz = "UTC"),
      mod_o = as.POSIXct("2026-10-31", tz = "UTC")
    ),
    cupties = data.frame(
      comp = "bfl", round = "Round 1", date = as.Date("2026-11-01"),
      team1 = "TEST TEAM", team2 = "TEST TEAM"
    )
  )
}

install_synthetic_app_data <- function(env) {
  list2env(synthetic_app_bundle(), env)
  env$managers_d <- data.frame(manager = "TEST MANAGER", team = "TEST TEAM")
  env$managers_o <- data.frame(manager = character(), team = character())
  env$build_runtime_objects(Sys.time())
  list(status = "updated", source = "Supabase Storage", retrieved_at = Sys.time())
}

test_that("Supabase is preferred and Drive is attempted only after failure", {
  env <- remote_app_functions()
  calls <- character()
  success <- function() {
    calls <<- c(calls, "supabase")
    list(status = "updated", source = "Supabase Storage")
  }
  drive <- function() {
    calls <<- c(calls, "drive")
    list(status = "updated", source = "Google Drive")
  }
  result <- env$retrieve_remote_data(list(Supabase = success, Drive = drive))
  expect_identical(calls, "supabase")
  expect_identical(result$source, "Supabase Storage")
  expect_s3_class(result$retrieved_at, "POSIXct")
  result <- env$retrieve_remote_data(list(
    Supabase = function() stop("Supabase offline"), Drive = drive
  ))
  expect_identical(result$source, "Google Drive")
  expect_identical(calls, c("supabase", "drive"))
})

test_that("remote failures clear source and derived data without using local bundles", {
  env <- remote_app_functions()
  install_synthetic_app_data(env)
  result <- env$retrieve_remote_data(list(
    Supabase = function() stop("offline"),
    Drive = function() stop("missing")
  ))
  expect_identical(result$status, "failed")
  expect_match(result$error, "Supabase: offline")
  expect_match(result$error, "Drive: missing")
  for (name in c("dl", "daily", "cupties", "time", "managers", "league", "file_updates")) {
    expect_null(get(name, env))
  }
  expect_identical(env$teamslist, character())
  expect_false(exists("load_local_bundle", env, inherits = FALSE))
})

test_that("Supabase loader uses only downloaded objects and normalizes runtime data", {
  env <- remote_app_functions()
  env$managers_d <- data.frame(manager = "TEST MANAGER", team = "TEST TEAM")
  env$managers_o <- data.frame(manager = character(), team = character())
  env$supabase_download_object <- function(destination) {
    bundle <- list2env(synthetic_app_bundle())
    save(list = ls(bundle), envir = bundle, file = destination)
  }
  result <- env$retrieve_remote_data(list(Supabase = env$load_supabase_bundle))
  expect_identical(result$status, "updated")
  expect_identical(env$dl, synthetic_app_bundle()$dl)
  expect_true(all(c("App", "SBapp") %in% names(env$daily)))
  expect_equal(env$league$total, 5)
})

test_that("Drive loader downloads the newest remote bundle and rejects invalid bundles", {
  env <- remote_app_functions()
  env$managers_d <- data.frame(manager = "TEST MANAGER", team = "TEST TEAM")
  env$managers_o <- data.frame(manager = character(), team = character())
  chosen <- NULL
  valid <- TRUE
  testthat::local_mocked_bindings(
    drive_download = function(file, path, overwrite) {
      chosen <<- file$id[[1]]
      bundle <- list2env(if (valid) synthetic_app_bundle() else list(dl = data.frame()))
      save(list = ls(bundle), envir = bundle, file = path)
    },
    .package = "googledrive"
  )
  listing <- data.frame(
    name = c("data.RDa", "data.RDa", "unrelated.RDa"),
    id = c("older", "newest", "unrelated"),
    modified_time = as.POSIXct(c("2026-11-01", "2026-11-02", "2026-11-03"), tz = "UTC")
  )
  result <- env$pull_remote_bundle(listing)
  expect_identical(chosen, "newest")
  expect_equal(result$modified_time, listing$modified_time[[2]])
  expect_identical(env$dl, synthetic_app_bundle()$dl)
  valid <- FALSE
  result <- env$retrieve_remote_data(list(Drive = function() env$pull_remote_bundle(listing)))
  expect_identical(result$status, "failed")
  expect_match(result$error, "missing objects")
  expect_null(env$dl)
})

test_that("Drive credential failures are handled lazily rather than aborting app startup", {
  withr::local_envvar(GOOGLE_PRIVATE_KEY = "")
  env <- remote_app_functions()
  sys.source(file.path("..", "..", "dreamleague", "google-credentials.R"), env)
  metadata <- withr::local_tempfile(fileext = ".json")
  writeLines("{}", metadata)
  expect_false(env$try_drive_auth(metadata))
  expect_false(env$try_drive_auth(tempfile()))
  expect_identical(
    env$retrieve_remote_data(list(Supabase = function() list(status = "updated", source = "Supabase")))$status,
    "updated"
  )
})

test_that("no-data startup, retries and failed refreshes update all data-dependent outputs", {
  env <- remote_app_functions()
  env$load_supabase_bundle <- function() install_synthetic_app_data(env)
  env$load_drive_bundle <- function() stop("Drive offline")
  testServer(env$server, {
    session$setInputs(
      league = "didsbury", league_teams = "didsbury", league_players = "didsbury",
      league_team_history = "didsbury", comp_cup = "bfl",
      start = as.Date("2026-10-31"), end = as.Date("2026-11-02")
    )
    expect_match(output$no_data_status$html, "No data has been retrieved")
    expect_match(output$data_download$html, 'disabled="disabled"', fixed = TRUE)
    for (name in c("table", "team_out", "team_history_out", "teamtext",
                   "playerstaken", "diagnostics", "update_time", "player_warning",
                   "cup", "round_date2")) {
      expect_error(output[[name]], "No data has been retrieved", info = name)
    }
    session$setInputs(retry_remote = 1)
    expect_null(output$no_data_status)
    expect_match(output$data_download$html, "download_data")
    expect_match(output$update_time$html, "2026-11-01")
    expect_true(nzchar(output$table))
    env$load_supabase_bundle <- function() stop("Supabase offline")
    session$setInputs(force_pull_supabase = 1)
    expect_match(output$no_data_status$html, "No data has been retrieved")
    expect_match(output$data_download$html, 'disabled="disabled"', fixed = TRUE)
    expect_error(output$table, "No data has been retrieved")
    expect_error(output$update_time, "No data has been retrieved")
    expect_null(env$dl)
    session$setInputs(force_pull_drive = 1)
    expect_match(output$no_data_status$html, "No data has been retrieved")
    env$load_drive_bundle <- function() install_synthetic_app_data(env)
    session$setInputs(force_pull_drive = 2)
    expect_null(output$no_data_status)
    expect_match(output$data_download$html, "download_data")
  })
})

test_that("a refresh in another session invalidates the shared no-data state", {
  env <- remote_app_functions()
  env$remote_data_state(install_synthetic_app_data(env))
  testServer(env$server, {
    session$setInputs(league = "didsbury")
    expect_true(nzchar(output$table))
    env$clear_runtime_objects()
    env$remote_data_state(list(status = "failed", source = "No data retrieved", error = "offline"))
    session$flushReact()
    expect_error(output$table, "No data has been retrieved")
    expect_match(output$data_download$html, 'disabled="disabled"', fixed = TRUE)
  })
})
