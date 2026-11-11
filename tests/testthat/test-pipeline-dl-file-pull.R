source(file.path("..", "..", "R", "dl-file-pull.R"))

remote_xlsx_listing <- function(name) {
  data.frame(
    name = name,
    modified_time = as.POSIXct("2026-09-01 10:00:00", tz = "UTC")
  )
}

test_that("normal and forced processing always download, even with newer local files", {
  for (force in c(FALSE, TRUE)) {
    local_path <- tempfile(fileext = ".xlsx")
    writeLines("stale local workbook", local_path)
    Sys.setFileTime(local_path, as.POSIXct("2026-09-02 10:00:00", tz = "UTC"))
    writeLines("9999999999", paste0(local_path, ".drivetime"))
    result <- refresh_remote_xlsx(
      local_path,
      force = force,
      listing_fn = remote_xlsx_listing,
      download_fn = function(file, path, overwrite) writeLines("remote workbook", path)
    )
    expect_identical(result$status, "downloaded")
    expect_equal(result$modified_time, remote_xlsx_listing("file")$modified_time)
    expect_identical(readLines(local_path), "remote workbook")
    unlink(c(local_path, paste0(local_path, ".drivetime")))
  }
})

test_that("missing local workbook is downloaded", {
  local_path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(local_path))
  result <- refresh_remote_xlsx(
    local_path,
    listing_fn = remote_xlsx_listing,
    download_fn = function(file, path, overwrite) writeLines("remote workbook", path)
  )
  expect_identical(result$status, "downloaded")
  expect_identical(readLines(local_path), "remote workbook")
})

test_that("listing failures never return an existing workbook", {
  local_path <- tempfile(fileext = ".xlsx")
  writeLines("stale workbook", local_path)
  on.exit(unlink(local_path))
  for (force in c(FALSE, TRUE)) {
    expect_error(
      refresh_remote_xlsx(
        local_path, force = force,
        listing_fn = function(name) {
          structure(list(error = "offline"), class = "drive_listing_error")
        }
      ),
      "Drive listing unavailable.*offline"
    )
    expect_error(
      refresh_remote_xlsx(
        local_path, force = force,
        listing_fn = function(name) {
          data.frame(name = character(), modified_time = as.POSIXct(character()))
        }
      ),
      "Google Drive XLSX not found"
    )
  }
  expect_identical(readLines(local_path), "stale workbook")
})

test_that("failed and empty downloads abort without replacing the local workbook", {
  local_path <- tempfile(fileext = ".xlsx")
  writeLines("stale workbook", local_path)
  on.exit(unlink(local_path))
  expect_error(
    refresh_remote_xlsx(
      local_path, listing_fn = remote_xlsx_listing,
      download_fn = function(file, path, overwrite) {
        writeLines("partial download", path)
        stop("download failed")
      }
    ),
    "download failed"
  )
  expect_identical(readLines(local_path), "stale workbook")
  expect_error(
    refresh_remote_xlsx(
      local_path, listing_fn = remote_xlsx_listing,
      download_fn = function(file, path, overwrite) file.create(path)
    ),
    "empty XLSX"
  )
  expect_identical(readLines(local_path), "stale workbook")
})

test_that("local XLSX is available only through an explicit testing opt-in", {
  withr::local_envvar(GITHUB_ACTIONS = "false")
  local_path <- tempfile(fileext = ".xlsx")
  writeLines("test workbook", local_path)
  on.exit(unlink(local_path))
  result <- refresh_remote_xlsx(
    local_path, local_test = TRUE,
    listing_fn = function(...) stop("must not contact Drive"),
    download_fn = function(...) stop("must not download")
  )
  expect_identical(result$status, "local_test")
  expect_equal(result$modified_time, file.info(local_path)$mtime[[1]])
  expect_error(refresh_remote_xlsx(tempfile(), local_test = TRUE), "not found")
  expect_error(
    refresh_remote_xlsx(local_path, force = TRUE, local_test = TRUE),
    "cannot be combined"
  )
  withr::local_envvar(GITHUB_ACTIONS = "true")
  expect_error(refresh_remote_xlsx(local_path, local_test = TRUE), "GitHub Actions")
})

test_that("numbered Drive copies compete with the original by modified time", {
  local_path <- withr::local_tempfile(fileext = ".xlsx")
  for (newest in c("DreamLeague26-27.xlsx", "DreamLeague26-27 (1).xlsx", "DreamLeague26-27 (12).xlsx")) {
    chosen <- NULL
    result <- refresh_remote_xlsx(
      local_path, remote_name = "DreamLeague26-27.xlsx",
      listing_fn = function(name) {
        names <- c(
          "DreamLeague26-27.xlsx", "DreamLeague26-27 (1).xlsx",
          "DreamLeague26-27 (12).xlsx", "DreamLeague25-26 (1).xlsx",
          "DreamLeague26-27 backup.xlsx"
        )
        times <- rep(as.POSIXct("2026-11-01", tz = "UTC"), length(names))
        times[names == newest] <- as.POSIXct("2026-11-02", tz = "UTC")
        times[4:5] <- as.POSIXct("2026-11-03", tz = "UTC")
        data.frame(name = names, modified_time = times)
      },
      download_fn = function(file, path, overwrite) {
        chosen <<- file$name[[1]]
        writeLines(chosen, path)
      }
    )
    expect_identical(chosen, newest)
    expect_identical(readLines(local_path), newest)
    expect_equal(result$modified_time, as.POSIXct("2026-11-02", tz = "UTC"))
  }
})

test_that("a numbered copy is used when the original is absent", {
  local_path <- withr::local_tempfile(fileext = ".xlsx")
  refresh_remote_xlsx(
    local_path, remote_name = "DL26-27.xlsx",
    listing_fn = function(name) remote_xlsx_listing("DL26-27 (2).xlsx"),
    download_fn = function(file, path, overwrite) writeLines(file$name, path)
  )
  expect_identical(readLines(local_path), "DL26-27 (2).xlsx")
})

test_that("workbook name matching is anchored and escapes regex characters", {
  pattern <- remote_xlsx_pattern("DL26-27.xlsx")
  expect_true(all(grepl(pattern, c("DL26-27.xlsx", "DL26-27 (1).xlsx", "DL26-27(2).XLSX"))))
  expect_false(any(grepl(pattern, c(
    "DL25-26.xlsx", "DL26-27 backup.xlsx", "DL26-27 (x).xlsx",
    "other DL26-27 (1).xlsx", "DL26-27 (1).xlsx.bak"
  ))))
  expect_true(grepl(remote_xlsx_pattern("DL[26].xlsx"), "DL[26] (1).xlsx"))
  expect_false(grepl(remote_xlsx_pattern("DL[26].xlsx"), "DL2.xlsx"))
})

test_that("Drive authentication reports success or failure explicitly", {
  path <- withr::local_tempfile(fileext = ".json")
  writeLines("test metadata", path)
  fail <- FALSE
  testthat::local_mocked_bindings(
    drive_auth = function(path) {
      if (fail) stop("authentication rejected")
      invisible(NULL)
    },
    .package = "googledrive"
  )
  expect_true(try_drive_auth(path))
  fail <- TRUE
  expect_false(try_drive_auth(path))
})
