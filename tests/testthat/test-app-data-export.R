source(file.path("..", "..", "dreamleague", "data-export.R"))

test_that("ZIP exports exactly the current bundle and readable metadata, not config", {
  bundle <- new.env(parent = emptyenv())
  bundle$dl <- data.frame(player = "TEST PLAYER", goals = 2L)
  bundle$daily <- data.frame(Date = as.Date("2026-11-01"), SBgoals = 2L)
  bundle$cupties <- data.frame(round = "Round 1", date = as.Date("2026-11-01"))
  bundle$time <- list(update_time = as.POSIXct("2026-11-01", tz = "UTC"))
  bundle$credentials <- "synthetic excluded value"
  bundle$config <- list(token = "synthetic excluded value")
  destination <- withr::local_tempfile(fileext = ".zip")
  retrieved <- as.POSIXct("2026-11-11 10:00:00", tz = "UTC")
  write_data_zip(bundle, destination, "Supabase Storage", retrieved)
  entries <- utils::unzip(destination, list = TRUE)$Name
  expect_setequal(entries, c("dl.csv", "daily.csv", "cupties.csv", "data.RDa", "metadata.txt"))
  extraction <- withr::local_tempdir()
  utils::unzip(destination, exdir = extraction)
  snapshot <- new.env(parent = emptyenv())
  load(file.path(extraction, "data.RDa"), snapshot)
  expect_setequal(ls(snapshot), c("dl", "daily", "time", "cupties"))
  for (name in ls(snapshot)) {
    expect_identical(get(name, snapshot), get(name, bundle))
  }
  expect_equal(read.csv(file.path(extraction, "dl.csv")), bundle$dl)
  metadata <- readLines(file.path(extraction, "metadata.txt"))
  expect_true(any(grepl("Supabase Storage", metadata, fixed = TRUE)))
  expect_true(any(grepl("2026-11-11T10:00:00Z", metadata, fixed = TRUE)))
  expect_false(any(grepl("synthetic excluded value", metadata, fixed = TRUE)))
})

test_that("ZIP creation rejects missing or cleared data rather than exporting an old snapshot", {
  destination <- withr::local_tempfile(fileext = ".zip")
  bundle <- new.env(parent = emptyenv())
  expect_error(
    write_data_zip(bundle, destination, "No data", Sys.time()),
    "No data has been retrieved"
  )
  expect_false(file.exists(destination))
  for (name in c("dl", "daily", "time", "cupties")) assign(name, NULL, bundle)
  expect_error(
    write_data_zip(bundle, destination, "No data", Sys.time()),
    "No data has been retrieved"
  )
  expect_false(file.exists(destination))
})
