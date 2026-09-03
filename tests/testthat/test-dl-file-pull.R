source(file.path("..", "..", "R", "dl-file-pull.R"))

test_that("downloads an xlsx when the remote file is newer", {
  local_path <- tempfile(fileext = ".xlsx")
  file.create(local_path)
  Sys.setFileTime(local_path, as.POSIXct("2026-09-01 10:00:00", tz = "UTC"))
  downloaded <- FALSE

  result <- refresh_remote_xlsx(
    local_path,
    listing_fn = function(name) {
      data.frame(
        name = name,
        modified_time = as.POSIXct("2026-09-02 10:00:00", tz = "UTC")
      )
    },
    download_fn = function(file, path, overwrite) downloaded <<- TRUE
  )

  expect_identical(result, "downloaded")
  expect_true(downloaded)
})

test_that("skips download when the local xlsx is current", {
  local_path <- tempfile(fileext = ".xlsx")
  file.create(local_path)
  Sys.setFileTime(local_path, as.POSIXct("2026-09-02 10:00:00", tz = "UTC"))
  downloaded <- FALSE

  result <- refresh_remote_xlsx(
    local_path,
    listing_fn = function(name) {
      data.frame(
        name = name,
        modified_time = as.POSIXct("2026-09-01 10:00:00", tz = "UTC")
      )
    },
    download_fn = function(file, path, overwrite) downloaded <<- TRUE
  )

  expect_identical(result, "current")
  expect_false(downloaded)
})

test_that("downloads when the local xlsx is missing", {
  local_path <- tempfile(fileext = ".xlsx")
  downloaded <- FALSE

  result <- refresh_remote_xlsx(
    local_path,
    listing_fn = function(name) {
      data.frame(
        name = name,
        modified_time = as.POSIXct("2026-09-01 10:00:00", tz = "UTC")
      )
    },
    download_fn = function(file, path, overwrite) downloaded <<- TRUE
  )

  expect_identical(result, "downloaded")
  expect_true(downloaded)
})

test_that("keeps the local xlsx when the remote file is unavailable", {
  local_path <- tempfile(fileext = ".xlsx")
  file.create(local_path)
  downloaded <- FALSE

  result <- refresh_remote_xlsx(
    local_path,
    listing_fn = function(name) {
      structure(list(error = "offline"), class = "drive_listing_error")
    },
    download_fn = function(file, path, overwrite) downloaded <<- TRUE
  )

  expect_identical(result, "unavailable")
  expect_false(downloaded)
})
